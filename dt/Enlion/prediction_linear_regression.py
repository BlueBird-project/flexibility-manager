import numpy as np
import pandas as pd
import matplotlib.pyplot as plt
from sklearn.linear_model import LinearRegression
from sklearn.metrics import mean_squared_error, mean_absolute_error
import argparse
import time
import os
import pickle
import warnings

warnings.filterwarnings("ignore")


class PredictorLinearRegression:
    def __init__(self):
        self.models = {}  # One model per endogenous variable
        self.past_states = 1  # Number of lagged endogenous values to use
        self.path = "./"
        self.endog_cols = []
        self.exog_cols = []
        self.feature_cols = []

    def load_and_clean_data(self, path):
        """Load dataset from CSV, drop missing values, and print head."""
        print("Loading dataset...")
        data = pd.read_csv(path, index_col=0, parse_dates=True)
        data = data.dropna()
        print("Dataset loaded and cleaned.")
        print(data.head())
        return data

    def create_features(self, endog, exog):
        """
        Create feature matrix by combining:
        - Lagged endogenous variables (past_states lags)
        - Current exogenous variables (no lag)
        
        Returns X (features) and y (targets), aligned properly.
        """
        self.endog_cols = endog.columns.tolist()
        self.exog_cols = exog.columns.tolist()
        
        # Start with empty features DataFrame
        features = pd.DataFrame(index=endog.index)
        
        # Add lagged endogenous variables
        for lag in range(1, self.past_states + 1):
            for col in endog.columns:
                features[f"{col}_lag{lag}"] = endog[col].shift(lag)
        
        # Add current exogenous variables (no lag)
        for col in exog.columns:
            features[f"{col}_current"] = exog[col]
        
        # Drop rows with NaN (from lagging)
        features = features.dropna()
        
        # Align target with features
        valid_idx = features.index
        y = endog.loc[valid_idx]
        
        self.feature_cols = features.columns.tolist()
        return features, y

    def save_weights_csv(self, output_path):
        if not self.models or not self.feature_cols:
            print("No trained models or features available to save weights.")
            return

        rows = []
        for target_col, model in self.models.items():
            for feature_name, coef in zip(self.feature_cols, model.coef_):
                rows.append({
                    "target": target_col,
                    "feature": feature_name,
                    "weight": float(coef)
                })
            rows.append({
                "target": target_col,
                "feature": "intercept",
                "weight": float(model.intercept_)
            })

        weights_df = pd.DataFrame(rows)
        weights_df.to_csv(output_path, index=False)
        print(f"Saved linear regression weights to {output_path}")

    def create_model(self, endog, exog, load_existing=True):
        """
        Train a separate LinearRegression model for each endogenous variable.
        Features = exog + lagged endog -> Target = endog
        """
        models_dir = os.path.join(self.path, "models")
        try:
            os.makedirs(models_dir, exist_ok=True)
        except Exception:
            pass

        model_file = os.path.join(models_dir, f"linear_reg_lag{self.past_states}.pkl")

        # Load existing fitted models if available
        if load_existing and os.path.exists(model_file):
            print(f"Loading saved Linear Regression models from {model_file}")
            try:
                with open(model_file, 'rb') as f:
                    saved_data = pickle.load(f)
                    self.models = saved_data['models']
                    self.endog_cols = saved_data['endog_cols']
                    self.exog_cols = saved_data['exog_cols']
                print("Models loaded successfully.")
                return
            except Exception as e:
                print(f"Failed to load saved models (will refit). Error: {e}")

        # Create features and targets
        X, y = self.create_features(endog, exog)
        
        start_model = time.time()
        
        # Train one model per endogenous variable
        self.models = {}
        for col in y.columns:
            model = LinearRegression()
            model.fit(X, y[col])
            self.models[col] = model
            print(f"Trained model for '{col}' - R² score: {model.score(X, y[col]):.4f}")
        
        end_model = time.time()
        print(f"Time to build and fit Linear Regression models: {end_model - start_model:.2f} seconds")

        # Save fitted models for future reuse
        try:
            save_data = {
                'models': self.models,
                'endog_cols': self.endog_cols,
                'exog_cols': self.exog_cols
            }
            with open(model_file, 'wb') as f:
                pickle.dump(save_data, f)
            print(f"Saved fitted models to {model_file}")
        except Exception as e:
            print(f"Warning: failed to save fitted models: {e}")

        # Save weights for external use
        try:
            weights_file = os.path.join(models_dir, f"linear_reg_lag{self.past_states}_weights.csv")
            self.save_weights_csv(weights_file)
        except Exception as e:
            print(f"Warning: failed to save weights CSV: {e}")

    def predict(self, exog_future, endog_history, exog_history=None):
        """
        Predict future endogenous values step-by-step.
        
        For each step:
        1. Use lagged endog values + lagged exog values as features
        2. Predict next endog values
        3. Update lagged values for next step
        
        Args:
            exog_future: DataFrame with future exogenous values
            endog_history: DataFrame with the last `past_states` endogenous values
            exog_history: DataFrame with the last `past_states` exogenous values (optional, uses last rows from exog_future if not provided)
        
        Returns:
            DataFrame with predictions
        """
        start_pred = time.time()
        
        steps = len(exog_future)
        if steps == 0:
            print("No future exogenous data provided for prediction.")
            return None

        # Initialize predictions storage
        predictions = {col: [] for col in self.endog_cols}
        
        # Keep track of lagged endogenous values (rolling window)
        # Start with the last `past_states` values from history
        endog_buffer = endog_history.iloc[-self.past_states:].copy()
        
        # Keep track of lagged exogenous values (rolling window)
        if exog_history is not None:
            exog_buffer = exog_history.iloc[-self.past_states:].copy()
        else:
            # Fallback: assume the caller provided a longer exog_future that starts earlier
            # This maintains backward compatibility but is not ideal
            exog_buffer = pd.DataFrame(columns=self.exog_cols)
            for col in self.exog_cols:
                exog_buffer[col] = [0.0] * self.past_states
        
        # Predict step by step
        for i in range(steps):
            # Build feature vector for this step
            features = {}
            
            # Add lagged endogenous variables
            for lag in range(1, self.past_states + 1):
                for col in self.endog_cols:
                    if lag <= len(endog_buffer):
                        features[f"{col}_lag{lag}"] = endog_buffer[col].iloc[-lag]
                    else:
                        # If not enough history, use the oldest available
                        features[f"{col}_lag{lag}"] = endog_buffer[col].iloc[0]
            
            # Add current exogenous variables (no lag)
            for col in self.exog_cols:
                features[f"{col}_current"] = exog_future[col].iloc[i]
            
            # Create feature DataFrame
            X_step = pd.DataFrame([features])
            
            # Predict for each endogenous variable
            new_endog = {}
            for col in self.endog_cols:
                pred_val = self.models[col].predict(X_step)[0]
                predictions[col].append(pred_val)
                new_endog[col] = pred_val
            
            # Update endog buffer with new prediction for next step's lagged features
            new_endog_row = pd.DataFrame([new_endog], index=[exog_future.index[i]])
            endog_buffer = pd.concat([endog_buffer, new_endog_row]).iloc[-self.past_states:]
            
            # Update exog buffer with current exog values for next step's lagged features
            new_exog = {col: exog_future[col].iloc[i] for col in self.exog_cols}
            new_exog_row = pd.DataFrame([new_exog], index=[exog_future.index[i]])
            exog_buffer = pd.concat([exog_buffer, new_exog_row]).iloc[-self.past_states:]
        
        # Create predictions DataFrame
        y_pred = pd.DataFrame(predictions, index=exog_future.index)
        
        #print(f"Predictions made for {steps} steps.")
        end_pred = time.time()
        elapsed_time_ms = (end_pred - start_pred) * 1000
        #print(f"Time to predict with Linear Regression: {elapsed_time_ms:.2f} ms")

        # Save predictions to CSV
        try:
            os.makedirs(os.path.join(self.path, "plots"), exist_ok=True)
        except Exception:
            pass
        y_pred.to_csv(os.path.join(self.path, "plots", "linreg_predictions_future.csv"))

        return y_pred


def main():
    parser = argparse.ArgumentParser(
        description="Train/predict linear regression model for waterworks data."
    )
    parser.add_argument(
        "mode",
        nargs="?",
        default="single",
        choices=["single", "multiple"],
        help="Use 'multiple' to run sliding-window retraining. Default is single-model mode.",
    )
    args = parser.parse_args()

    predictor = PredictorLinearRegression()
    predictor.past_states = 24  # Number of lagged endogenous values to use

    df = predictor.load_and_clean_data("waterworks_synthetic_dataset.csv")

    endog_vars = df[["tank_level_m3", 
                     "total_pump_power_kw",
                     "pump_1_flow_m3h","pump_2_flow_m3h", "pump_3_flow_m3h", "pump_4_flow_m3h"]]
    exog_vars = df[[
        "outflow_m3h",
        #"total_inflow_m3h",
        "pump_1_setpoint_percent", 
        "pump_2_setpoint_percent", 
        "pump_3_setpoint_percent", 
        "pump_4_setpoint_percent"
    ]]

    # Prediction horizon (steps ahead to forecast)
    horizon = 24 * 3
    # Use the last 4*horizon observed steps as training history
    train_window = horizon * 3

    def run_prediction_window(train_start, train_end):
        exog_vars_train = exog_vars.iloc[train_start:train_end]
        endog_vars_train = endog_vars.iloc[train_start:train_end]

        # Exogenous inputs for prediction horizon
        exog_future = exog_vars.iloc[train_end:train_end + horizon]

        # Need the last `past_states` endogenous and exogenous values for prediction
        endog_history = endog_vars.iloc[train_end - predictor.past_states:train_end]
        exog_history = exog_vars.iloc[train_end - predictor.past_states:train_end]

        print(f"\nTraining on window {train_start}:{train_end} (length={len(exog_vars_train)}) "
              f"to predict next {horizon} steps starting at index {exog_future.index[0]}")

        predictor.create_model(endog_vars_train, exog_vars_train, load_existing=False)
        y_pred = predictor.predict(exog_future, endog_history, exog_history)
        pred_start_idx = exog_future.index[0] if len(exog_future) > 0 else None

        # Get actual future values for comparison
        endog_future = None
        try:
            endog_future = endog_vars.iloc[train_end:train_end + len(exog_future)]
        except Exception:
            endog_future = None

        # Plot results
        for col in y_pred.columns:
            plt.figure(figsize=(12, 5))

            # Plot training window (observed)
            plt.plot(endog_vars_train.index, endog_vars_train[col],
                     label='Training (Observed)', linestyle='-')

            # Plot predictions
            plt.plot(y_pred.index, y_pred[col],
                     label='Prediction', linestyle='--')

            # Plot actual future values for comparison
            mse_text = None
            if endog_future is not None and len(endog_future) > 0:
                try:
                    plt.plot(endog_future.index, endog_future[col],
                             label='Actual Future', linestyle=':', color='black')
                    # Compute MSE
                    common_idx = endog_future.index.intersection(y_pred.index)
                    if len(common_idx) > 0:
                        true_vals = endog_future.loc[common_idx, col]
                        pred_vals = y_pred.loc[common_idx, col]
                        mse_val = mean_squared_error(true_vals, pred_vals)
                        mae_val = mean_absolute_error(true_vals, pred_vals)
                        mse_text = f"MSE={mse_val:.4f}\nMAE={mae_val:.4f}"
                except Exception:
                    pass

            # Mark prediction start
            if pred_start_idx is not None:
                plt.axvline(x=pred_start_idx, color='gray', linestyle=':',
                            label='Prediction Start')

            plt.legend()
            plt.title(f"Linear Regression (lag={predictor.past_states}) - {col}")
            plt.xlabel("Time")
            plt.ylabel(col)
            plt.grid(True)

            # Display MSE/MAE
            if mse_text is not None:
                ax = plt.gca()
                ax.text(0.02, 0.95, mse_text, transform=ax.transAxes, fontsize=10,
                        verticalalignment='top',
                        bbox=dict(facecolor='white', alpha=0.7, edgecolor='none'))

            plt.tight_layout()
            safe_col = str(col).replace(' ', '_')
            os.makedirs(os.path.join(predictor.path, "plots"), exist_ok=True)
            plt.savefig(os.path.join(predictor.path, "plots",
                                     f"linreg_prediction_future_{safe_col}.png"))
            plt.close()

        print(f"Plots saved for prediction window starting at {pred_start_idx}")

    if args.mode == "multiple":
        # Sliding window over prediction start points
        for pred_start in range(len(df) // 2, len(df), horizon):
            # Require a full prediction horizon available
            if pred_start + horizon > len(df):
                break

            # Training window
            train_end = pred_start
            train_start = max(0, train_end - train_window)

            if train_start >= train_end:
                print(f"Insufficient history for training at prediction start {pred_start}; skipping.")
                continue

            run_prediction_window(train_start, train_end)
    else:
        # Default mode: train and predict once using the most recent full horizon.
        train_end = len(df) - horizon
        train_start = max(0, train_end - train_window)

        if train_start >= train_end:
            raise ValueError("Insufficient history for single-model run.")

        run_prediction_window(train_start, train_end)


if __name__ == "__main__":
    main()
