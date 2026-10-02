library(expm)

build_discrete_state_space_multiroom_v2 <- function(est, dt, n_rooms = 5, 
                                                 time_unit = "hours",
                                                 estiamte_Q_door_peak = TRUE) {
  
  # Helper function to extract estimate by row name
  get_est <- function(name) {
    est[name, "estimate"]
  }
  
  # Extract ambient temperature
  Tamb <- get_est("Tamb")
  
  # Extract shared COP parameter
  k3 <- get_est("k3")
  
  # Convert k3 if using seconds
  if (time_unit == "seconds") {
    k3 <- k3 / 3600
  } else if (time_unit != "hours") {
    stop("time_unit must be either 'hours' or 'seconds'")
  }
  
  # State space dimensions
  n_states <- 2 * n_rooms
  n_inputs <- n_rooms
  n_dist   <- 1 + n_rooms
  
  # ── Continuous-time A matrix ──────────────────────────────────────
  A <- matrix(0, nrow = n_states, ncol = n_states)
  
  for (i in 1:n_rooms) {
    
    # Extract room parameters
    k1_i <- get_est(sprintf("k1_%d", i))
    k2_i <- get_est(sprintf("k2_%d", i))
    k5_i <- get_est(sprintf("k5_%d", i))
    
    # Convert to per-second if needed
    if (time_unit == "seconds") {
      k1_i <- k1_i / 3600
      k2_i <- k2_i / 3600
      k5_i <- k5_i / 3600
    }
    
    # State indices
    ia <- 2 * (i - 1) + 1
    ip <- 2 * (i - 1) + 2
    
    # Air node
    A[ia, ia] <- -(k1_i + k2_i)
    A[ia, ip] <-  k2_i
    
    # Product node
    A[ip, ia] <-  k5_i
    A[ip, ip] <- -k5_i
  }
  
  # ── Input matrix B ────────────────────────────────────────────────
  B <- matrix(0, nrow = n_states, ncol = n_inputs)
  
  for (i in 1:n_rooms) {
    ia <- 2 * (i - 1) + 1
    B[ia, i] <- -k3
  }
  
  # ── Disturbance matrix E ──────────────────────────────────────────
  E <- matrix(0, nrow = n_states, ncol = n_dist)
  
  for (i in 1:n_rooms) {
    
    k1_i <- get_est(sprintf("k1_%d", i))
    k4_i <- get_est(sprintf("k4_%d", i))
    
    if (time_unit == "seconds") {
      k1_i <- k1_i / 3600
      k4_i <- k4_i / 3600
    }
    
    ia <- 2 * (i - 1) + 1
    
    # Ambient disturbance
    E[ia, 1] <- k1_i
    
    # Door disturbance
    if (estiamte_Q_door_peak) {
      E[ia, i + 1] <- k4_i * 1000
    } else {
      E[ia, i + 1] <- k4_i
    }
  }
  
  # ── Exact discretization ──────────────────────────────────────────
  total_dim <- n_states + n_inputs + n_dist
  
  M <- matrix(0, nrow = total_dim, ncol = total_dim)
  
  M[1:n_states, 1:n_states] <- A
  M[1:n_states, (n_states + 1):(n_states + n_inputs)] <- B
  M[1:n_states, (n_states + n_inputs + 1):total_dim] <- E
  
  Md <- expm(M * dt)
  
  # Discrete matrices
  Ad <- Md[1:n_states, 1:n_states]
  
  Bd <- Md[
    1:n_states,
    (n_states + 1):(n_states + n_inputs)
  ]
  
  Ed <- Md[
    1:n_states,
    (n_states + n_inputs + 1):total_dim
  ]
  
  # ── Observation matrix C ──────────────────────────────────────────
  C <- matrix(0, nrow = n_rooms, ncol = n_states)
  
  for (i in 1:n_rooms) {
    ia <- 2 * (i - 1) + 1
    C[i, ia] <- 1
  }
  
  # ── Noise covariances ─────────────────────────────────────────────
  sigma_a   <- exp(get_est("sigma_a"))
  sigma_p   <- exp(get_est("sigma_p"))
  sigma_obs <- get_est("sigma_obs")
  
  Q_diag <- rep(c(sigma_a^2, sigma_p^2), n_rooms)
  
  Q_continuous <- diag(Q_diag)
  
  R <- diag(rep(sigma_obs^2, n_rooms))
  
  # Message
  if (time_unit == "seconds") {
    cat(sprintf(
      "\nConverted k-parameters from [1/hour] to [1/second] for all %d rooms\n",
      n_rooms
    ))
  }
  
  return(list(
    Ad = Ad,
    Bd = Bd,
    Ed = Ed,
    C  = C,
    Q_continuous = Q_continuous,
    R  = R,
    A  = A,
    B  = B,
    E  = E,
    dt = dt,
    time_unit = time_unit,
    n_states = n_states,
    n_inputs = n_inputs,
    n_dist   = n_dist,
    n_rooms  = n_rooms,
    Tamb = Tamb
  ))
}