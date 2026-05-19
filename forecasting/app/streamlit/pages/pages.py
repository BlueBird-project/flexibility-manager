import streamlit as st
import os
import pandas as pd
from datetime import datetime, timedelta
import plotly.express as px
import plotly.graph_objects as go
from codecarbon import EmissionsTracker
from sklearn.metrics import mean_absolute_error, mean_absolute_percentage_error, mean_squared_error 
from chronos import Chronos2Pipeline
import time
import torch

def Coverage(y_true, y_pred, threshold = 0.75):
        import numpy as np
        total_ = 0
        for true, pred in zip(y_true, y_pred):
            if true >= float(pred)*threshold and true <= float(pred)*(2-threshold):
                total_ = total_ + 1
        
        return np.round((total_/len(y_true))*100,2)

def PageInit(parent_dir):

    if "data" not in st.session_state or st.session_state["data"] is None:
        
        st.image(os.path.join(parent_dir,"src", r"bluebird-logo-V8-horizontal.png"), width="stretch")

        file = st.file_uploader("Upload a file", type=["csv", "feather", "xlsx"])

        if file is not None:
            data = pd.read_csv(file) if file.name.endswith("csv") else pd.read_feather(file) if file.name.endswith("feather") else pd.read_excel(file)

            st.dataframe(data, height=100)

            col_0, col_1, col_2 = st.columns(3)

            with col_0:
                col_ds = st.selectbox("Select date column", options=data.columns)
            with col_1:
                col_y = st.selectbox("Select target column", options=data.columns)
            with col_2:
                col_id = st.selectbox("Select id column", options=data.columns)
            

            if st.button("Submit"):
                
                data = data.rename(columns={col_ds: "ds", col_y: "y", col_id: "unique_id"})

                st.session_state["data"] = data[["ds", "y", "unique_id"]]
                st.rerun()

def PageView(data):
        
    with st.expander("Check DB"):
        st.dataframe(data, height=100)

    
    unique_ids = st.multiselect("Select unique_id", options=data["unique_id"].unique(), default=None)

    data_dates = data[data["unique_id"].isin(unique_ids)]["ds"] if unique_ids is not None and len(unique_ids) > 0 else data["ds"]
    col_date_0, col_date_1 = st.columns(2)

    with col_date_0:

        date_max = max(data_dates)
        if type(date_max) == str:
            date_max = pd.to_datetime(date_max)
            date_min_option = st.date_input("Select min date", value=date_max- timedelta(days = 10))
            date_min_option = datetime.strftime(date_min_option, "%Y-%m-%d")
        else:
            date_min_option = st.date_input("Select min date", value=date_max- timedelta(days = 10))
            date_min_option = datetime.strftime(date_min_option, "%Y-%m-%d")
    with col_date_1:

        if type(date_max) == str:
            date_max_option = st.date_input("Select max date", value=date_max)
            date_max_option = datetime.strftime(date_max_option, "%Y-%m-%d")
        else:
            date_max_option = st.date_input("Select max date", value=date_max)
            date_max_option = datetime.strftime(date_max_option, "%Y-%m-%d")   

    data_to_plot = data[(data["ds"] >= date_min_option) & (data["ds"] <= date_max_option) & (data["unique_id"].isin(unique_ids))]


    if unique_ids is not None and len(unique_ids) < 5:
        fig = px.line(data[(data["ds"] >= date_min_option) & (data["ds"] <= date_max_option) & (data["unique_id"].isin(unique_ids))], x="ds", y="y", color="unique_id")

        st.plotly_chart(fig, width="stretch")
    elif unique_ids is not None and len(unique_ids) >= 5:
        st.error("Choose less than 5 unique_ids to plot")

def PageForecasting(data):

    if "forecast_data" not in st.session_state or st.session_state["forecast_data"] is None:

        unique_ids = st.multiselect("Select unique_id", options=data["unique_id"].unique(), default=None)

        data_ids = data[data["unique_id"].isin(unique_ids)]
        if data_ids.shape[0] != 0:
            date_input = st.date_input("Select date to forecast", 
                value = pd.to_datetime(max(data_ids["ds"]))- timedelta(days = 10), 
            )
        
            col_forecast_0, col_forecast_1 = st.columns(2)

            with col_forecast_0:
                context = st.slider("Context length (days)", min_value=1, max_value=100, value=10)
            with col_forecast_1:
                forecast_horizon = st.number_input("Forecast horizon (days)", min_value=1, max_value=5, value=1)
            
            min_date_context = datetime.strftime(date_input - timedelta(days=context), "%Y-%m-%d")
            date_input = datetime.strftime(date_input, "%Y-%m-%d")

            data_to_forecast = data[(data["ds"] >= min_date_context) & (data["ds"] <= date_input) & (data["unique_id"].isin(unique_ids))]

            if st.button("Run Inference"):
                
                with st.status("Running inference...", expanded = True) as status:

                    tracker = EmissionsTracker()
                    tracker.start()

                    st.write("Loading pretrained model...")
                    if os.name == "nt":
                        device = "cuda" if torch.cuda.is_available() else "cpu"
                    elif os.name == "posix":
                        device = "mps"
                    
                    pipeline = Chronos2Pipeline.from_pretrained(
                        "amazon/chronos-2",
                        device_map=device
                    )
                    st.write("Preprocessing data...")
                    data_to_forecast["ds"] = pd.to_datetime(data_to_forecast["ds"])
                    data_to_forecast = data_to_forecast.sort_values(by=["unique_id", "ds"])

                    st.write("Generating forecasts...")
                    data_to_forecast = data_to_forecast.rename(columns={"ds": "timestamp", "y": "target", "unique_id": "item_id"})
                    # Predicción
                    forecast = pipeline.predict_df(
                        data_to_forecast,
                        prediction_length=24*forecast_horizon
                    )

                    emissions = tracker.stop()

                    data_process = {
                        "duration": tracker.final_emissions_data.duration,
                        "emissions": tracker.final_emissions_data.emissions,
                        "energy_consumed": tracker.final_emissions_data.energy_consumed,
                        "water_consumed": tracker.final_emissions_data.water_consumed
                    }

                    st.session_state["forecast_data"] = forecast
                    st.session_state["emissions_data"] = data_process
                    status.update(
                        label="Download complete!", state="complete", expanded=False
                    )

                    time.sleep(1)

                    st.rerun()

    else:
        st.subheader("Forecast results")
        
        toggle_check = st.toggle("Show graph", value=False)
        
        forecast_data = st.session_state["forecast_data"]
        forecast_data["ds"] = forecast_data["timestamp"].apply(lambda x: datetime.strftime(x, "%Y-%m-%d %H:%M:%S"))
        results = pd.merge(data, forecast_data[["ds", "item_id", "predictions", "0.9", "0.8"]], left_on=["ds", "unique_id"], right_on=["ds", "item_id"], how="inner")
        results = results.rename(columns={"predictions": "forecast", "0.9": "upper_bound_90", "0.8": "upper_bound_80"})
        results["lower_bound_90"] = results["forecast"] - (results["upper_bound_90"] - results["forecast"])

        with st.expander("Emissions data"):
            emissions_data = st.session_state["emissions_data"]
            st.metric("Duration (s)", emissions_data["duration"], border= True)
            st.metric("Emissions (kgCO2eq)", emissions_data["emissions"], border= True)
            st.metric("Energy consumed (kWh)", emissions_data["energy_consumed"], border= True)
            st.metric("Water consumed (L)", emissions_data["water_consumed"], border= True)     
        col_metrics_0, col_metrics_1, col_metrics_2, col_metrics_4 = st.columns(4)

        with col_metrics_0:
            st.metric("MAE", round(mean_absolute_error(results["y"], results["forecast"]), 2), border= True)
        with col_metrics_1:
            st.metric("MAPE", f'{100 - round(mean_absolute_percentage_error(results["y"], results["forecast"]) * 100, 2)}%', border= True)
        with col_metrics_2:
            st.metric("RMSE", f'{round(mean_squared_error(results["y"], results["forecast"]) ** 0.5, 2)}', border= True)
        with col_metrics_4:
            st.metric("Coverage", f'{Coverage(results["y"], results["forecast"], threshold=0.95)}%', border= True)

        if toggle_check:
            fig = go.Figure()
            fig.add_trace(go.Scatter(x=results["ds"], y=results["y"], mode="markers", name="Actual"))
            fig.add_trace(go.Scatter(x=results["ds"], y=results["forecast"], mode="lines", name="Forecast"))
            fig.add_trace(go.Scatter(x=results["ds"], y=results["upper_bound_90"], mode="lines", name="Upper Bound 90", line=dict(dash='dash')))
            fig.add_trace(go.Scatter(x=results["ds"], y=results["lower_bound_90"], mode="lines", name="Lower Bound 90", line=dict(dash='dash')))
            fig.update_layout(title="Forecast vs Actual", xaxis_title="Date", yaxis_title="Value")
            fig.update_layout(legend=dict(orientation="h", yanchor="bottom", y=1.02, xanchor="right", x=1))
            fig.update_layout(yaxis=dict(range=[min(results[["y", "lower_bound_90"]].min())*0.85, max(results[["y", "upper_bound_90"]].max())*1.1]))
            st.plotly_chart(fig, width="stretch")
        
        if st.button("Try another inference"):
            st.session_state["forecast_data"] = None
            st.rerun()