library(ctsmTMB)

source("build_discrete_state_space_multiroom_v2.R")  # Source the updated function
source("write_model_in_json.R")

############################################################
# 0. Configuration
############################################################

SPLIT_HOUR <- 192 - 28   # Change this to set where training ends (in hours)
# Data before SPLIT_HOUR is training; the rest is test.
# E.g. SPLIT_HOUR = 18 -> train on first 18h, test on last 6h.

n_rooms <- 5  # Number of rooms

# Should the impact of the door openings be estimated 
estiamte_Q_door_peak <- FALSE

# Should the door opening be known to the 
door_prediction <- FALSE

############################################################
# 1. Load and prepare data
############################################################

df.raw <- read.csv("simulation_data_8day_noise_real_door_opening.csv")

df.obs      <- df.raw
df.obs$t    <- df.raw$time_min / 60   # minutes -> hours

# ── map columns ────────────────────────────────────────────────────
{
  # Observations (temperatures)
  df.obs$y_Ta1 <- df.obs$T_air_room1_degC
  df.obs$y_Ta2 <- df.obs$T_air_room2_degC
  df.obs$y_Ta3 <- df.obs$T_air_room3_degC
  df.obs$y_Ta4 <- df.obs$T_air_room4_degC
  df.obs$y_Ta5 <- df.obs$T_air_room5_degC
  
  # Inputs (cooling power) - adjust column names as needed
  df.obs$u1 <- df.obs$cooling_allocated_room1_W
  df.obs$u2 <- df.obs$cooling_allocated_room2_W
  df.obs$u3 <- df.obs$cooling_allocated_room3_W
  df.obs$u4 <- df.obs$cooling_allocated_room4_W
  df.obs$u5 <- df.obs$P3_freezer_W
  
  # Disturbances (door openings)
  if (estiamte_Q_door_peak) {
    Q_door_peak <- 1  # Will estimate the actual value via k4
  } else {
    Q_door_peak <- 4500  # Fixed known value
  }
  
  df.obs$d1 <- df.obs$door_open_room1 * Q_door_peak
  df.obs$d2 <- df.obs$door_open_room2 * Q_door_peak
  df.obs$d3 <- df.obs$door_open_room3 * Q_door_peak
  df.obs$d4 <- df.obs$door_open_room4 * Q_door_peak
  df.obs$d5 <- df.obs$door_open_room5 * Q_door_peak
}

# ── Train / test split ────────────────────────────────────────────────────────
train_idx <- df.obs$t <= SPLIT_HOUR
test_idx  <- df.obs$t >  SPLIT_HOUR

df.train <- df.obs[train_idx, ]
df.test  <- df.obs[test_idx,  ]

cat(sprintf("\nData split at %.1f h:  %d training rows,  %d test rows\n",
            SPLIT_HOUR, nrow(df.train), nrow(df.test)))

############################################################
# 2. Build the ctsmTMB model
############################################################
{
  model <- ctsmTMB$new()
  
  model$addInput(u1, u2, u3, u4, u5,
                 d1, d2, d3, d4, d5)
  
  # ── Reduced system equations ────────────────────────────────────────────────
  # Effective rates for each room:
  # k1_i = UA_i/Ca_i      (wall heat transfer rate)
  # k2_i = UAap_i/Ca_i    (air-product coupling rate)
  # k3   = COP/Ca_i       (cooling efficiency - could be room-specific)
  # k4_i = Q_door/Ca_i or 1/Ca_i  (door opening effect)
  # k5_i = UAap_i/Cp_i    (product thermal rate)
  
  if (estiamte_Q_door_peak) {
    # Estimate Q_door_peak, so k4*d*1000 where d is binary and k4 = Q_door/(1000*Ca)
    
    model$addSystem(
      # Room 1
      dTa1 ~ ( k1_1*(Tamb - Ta1) + k2_1*(Tp1 - Ta1) - k3*u1 + k4_1*d1*1000 ) * dt + exp(sigma_a) * dw1,
      dTp1 ~ ( k5_1*(Ta1 - Tp1) ) * dt + exp(sigma_p) * dw2,
      
      # Room 2
      dTa2 ~ ( k1_2*(Tamb - Ta2) + k2_2*(Tp2 - Ta2) - k3*u2 + k4_2*d2*1000 ) * dt + exp(sigma_a) * dw3,
      dTp2 ~ ( k5_2*(Ta2 - Tp2) ) * dt + exp(sigma_p) * dw4,
      
      # Room 3
      dTa3 ~ ( k1_3*(Tamb - Ta3) + k2_3*(Tp3 - Ta3) - k3*u3 + k4_3*d3*1000 ) * dt + exp(sigma_a) * dw5,
      dTp3 ~ ( k5_3*(Ta3 - Tp3) ) * dt + exp(sigma_p) * dw6,
      
      # Room 4
      dTa4 ~ ( k1_4*(Tamb - Ta4) + k2_4*(Tp4 - Ta4) - k3*u4 + k4_4*d4*1000 ) * dt + exp(sigma_a) * dw7,
      dTp4 ~ ( k5_4*(Ta4 - Tp4) ) * dt + exp(sigma_p) * dw8,
      
      # Room 5
      dTa5 ~ ( k1_5*(Tamb - Ta5) + k2_5*(Tp5 - Ta5) - k3*u5 + k4_5*d5*1000 ) * dt + exp(sigma_a) * dw9,
      dTp5 ~ ( k5_5*(Ta5 - Tp5) ) * dt + exp(sigma_p) * dw10
    )
    
  } else {
    # Q_door_peak is known and already included in d1-d5
    
    model$addSystem(
      # Room 1
      dTa1 ~ ( k1_1*(Tamb - Ta1) + k2_1*(Tp1 - Ta1) - k3*u1 + k4_1*d1 ) * dt + exp(sigma_a) * dw1,
      #dTa1 ~ ( k1_1*(Tamb - Ta1) - k3*u1 + k4_1*d1 ) * dt + exp(sigma_a) * dw1,
      dTp1 ~ ( k5_1*(Ta1 - Tp1) ) * dt + exp(sigma_p) * dw2,
      
      # Room 2
      dTa2 ~ ( k1_2*(Tamb - Ta2) + k2_2*(Tp2 - Ta2) - k3*u2 + k4_2*d2 ) * dt + exp(sigma_a) * dw3,
      #dTa2 ~ ( k1_2*(Tamb - Ta2) - k3*u2 + k4_2*d2 ) * dt + exp(sigma_a) * dw3,
      dTp2 ~ ( k5_2*(Ta2 - Tp2) ) * dt + exp(sigma_p) * dw4,
      
      # Room 3
      dTa3 ~ ( k1_3*(Tamb - Ta3) + k2_3*(Tp3 - Ta3) - k3*u3 + k4_3*d3 ) * dt + exp(sigma_a) * dw5,
      #dTa3 ~ ( k1_3*(Tamb - Ta3) - k3*u3 + k4_3*d3 ) * dt + exp(sigma_a) * dw5,
      dTp3 ~ ( k5_3*(Ta3 - Tp3) ) * dt + exp(sigma_p) * dw6,
      
      # Room 4
      dTa4 ~ ( k1_4*(Tamb - Ta4) + k2_4*(Tp4 - Ta4) - k3*u4 + k4_4*d4 ) * dt + exp(sigma_a) * dw7,
      #dTa4 ~ ( k1_4*(Tamb - Ta4) - k3*u4 + k4_4*d4 ) * dt + exp(sigma_a) * dw7,
      dTp4 ~ ( k5_4*(Ta4 - Tp4) ) * dt + exp(sigma_p) * dw8,
      
      # Room 5
      dTa5 ~ ( k1_5*(Tamb - Ta5) + k2_5*(Tp5 - Ta5) - k3*u5 + k4_5*d5 ) * dt + exp(sigma_a) * dw9,
      dTp5 ~ ( k5_5*(Ta5 - Tp5) ) * dt + exp(sigma_p) * dw10
    )
  }
}
  # ── Observation equation ─────────────────────────────────────────────────────
  model$addObs(
    y_Ta1 ~ Ta1,
    y_Ta2 ~ Ta2,
    y_Ta3 ~ Ta3,
    y_Ta4 ~ Ta4,
    y_Ta5 ~ Ta5
  )

{  
  model$setVariance(
    y_Ta1 ~ sigma_obs^2,
    y_Ta2 ~ sigma_obs^2,
    y_Ta3 ~ sigma_obs^2,
    y_Ta4 ~ sigma_obs^2,
    y_Ta5 ~ sigma_obs^2
  )
  
  # ── Parameters ──────────────────────────────────────────────────────────────
  model$setParameter(
    # Effective drift rates [1/h] for Room 1
    k1_1 = c(initial = 1.0, lower = 0, upper = 10),
    #k2_1 = c(initial = 1.0, lower = 0, upper = 10),
    k2_1 = c(initial = 2.85),
    k4_1 = c(initial = 0.001, lower = 0, upper = 1),
    #k5_1 = c(initial = 1.0, lower = 0, upper = 10),
    k5_1 = c(initial = 1.221429),
    
    # Effective drift rates [1/h] for Room 2
    k1_2 = c(initial = 1.0, lower = 0, upper = 10),
    #k2_2 = c(initial = 1.0, lower = 0, upper = 10),
    k2_2 = c(initial = 3.312),
    k4_2 = c(initial = 0.001, lower = 0, upper = 1),
    #k5_2 = c(initial = 1.0, lower = 0, upper = 10),
    k5_2 = c(initial = 1.577143),
    
    # Effective drift rates [1/h] for Room 3
    k1_3 = c(initial = 1.0, lower = 0, upper = 10),
    #k2_3 = c(initial = 1.0, lower = 0, upper = 10),
    k2_3 = c(initial = 2.3520),
    k4_3 = c(initial = 0.001, lower = 0, upper = 1),
    #k5_3 = c(initial = 1.0, lower = 0, upper = 10),
    k5_3 = c(initial = 1.1200),
    
    # Effective drift rates [1/h] for Room 4
    k1_4 = c(initial = 1.0, lower = 0, upper = 10),
    #k2_4 = c(initial = 1.0, lower = 0, upper = 10),
    k2_4 = c(initial = 3.076364),
    k4_4 = c(initial = 0.001, lower = 0, upper = 1),
    #k5_4 = c(initial = 1.0, lower = 0, upper = 10),
    k5_4 = c(initial = 1.381224),
    
    # Effective drift rates [1/h] for Room 5
    k1_5 = c(initial = 1.0, lower = 0, upper = 10),
    #k2_5 = c(initial = 1.0, lower = 0, upper = 10),
    k2_5 = c(initial = 4.550),
    k4_5 = c(initial = 0.001, lower = 0, upper = 1),
    #k5_5 = c(initial = 1.0, lower = 0, upper = 10),
    k5_5 = c(initial = 2.340),
    
    # Shared COP parameter (assuming same cooling system)
    k3 = c(initial = 0.01, lower = 0, upper = 10),
    
    # Noise terms (shared across rooms for regularization)
    sigma_obs = c(initial = 1e-1, lower = -30, upper = 1),
    sigma_a   = c(initial = 1e-1, lower = -30, upper = 1),
    sigma_p   = c(initial = 1e-1, lower = -30, upper = 1),
    
    # Fixed parameters
    Tamb = c(initial = 15.0)
  )

  # ── Initial states ──────────────────────────────────────────────────────────
  x0 <- c(
    Ta1 = df.train$T_air_room1_degC[1],
    Ta2 = df.train$T_air_room2_degC[1],
    Ta3 = df.train$T_air_room3_degC[1],
    Ta4 = df.train$T_air_room4_degC[1],
    Ta5 = df.train$T_air_room5_degC[1],
    
    Tp1 = df.train$T_air_room1_degC[1],
    Tp2 = df.train$T_air_room2_degC[1],
    Tp3 = df.train$T_air_room3_degC[1],
    Tp4 = df.train$T_air_room4_degC[1],
    Tp5 = df.train$T_air_room5_degC[1]
  )
  
  model$setInitialState(list(x0 = x0, p0 = 1e-4 * diag(10)))
}
############################################################
# 3. Estimate on TRAINING data only
############################################################

fit <- model$estimate(
  df.train,
  use.hessian = TRUE,
  method  = "ekf",
  control = list(eval.max = 100000, iter.max = 4000, trace = 10)
)

dt <- 60
    
cat(sprintf("\n=== State Space Model in SECONDS ===\n"))
cat(sprintf("Sampling time dt = %.1f seconds = %.2f minutes\n", 
            dt, dt / 60))

ss_model <- build_discrete_state_space_multiroom_v2(
  model$getParameters(), 
  dt = dt,
  n_rooms = 5,
  time_unit = "seconds",
  estiamte_Q_door_peak = estiamte_Q_door_peak
)

create_model_json(ss_model,file_name = "cold_room_model_cont.json")
