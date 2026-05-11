library(jsonlite)

create_model_json <- function(model,
                              file_name = "model.json",
                              model_id = "fridge_freezer_model",
                              description = "Five-room refrigeration system",
                              version = "1.0.0",
                              room_names = c(
                                "fridge_dairy",
                                "fridge_finishedProducts",
                                "fridge_meat",
                                "fridge_vegetables",
                                "freezer"
                              ),
                              x0 = c(
                                3, 3,
                                4, 4,
                                2, 2,
                                5, 5,
                                -18, -18
                              ),
                              Kgain = 2500,
                              timestamp = format(Sys.time(), "%Y-%m-%dT%H:%M:%SZ", tz = "UTC")) {
  
  n_rooms <- model$n_rooms
  
  # ── Names ─────────────────────────────────────────────────────────
  
  state_names <- c()
  
  for (room in room_names) {
    state_names <- c(
      state_names,
      paste0("T_air_", room),
      paste0("T_prod_", room)
    )
  }
  
  input_names <- paste0("P_compressor_", room_names)
  
  disturbance_names <- c(
    "T_ambient",
    paste0("Q_door_", room_names)
  )
  
  output_names <- paste0("T_air_", room_names)
  
  # ── Units ─────────────────────────────────────────────────────────
  
  units <- list(
    states = rep("degC", model$n_states),
    inputs = rep("W", model$n_inputs),
    disturbances = c("degC", rep("W", n_rooms)),
    measured = rep("degC", model$n_rooms)
  )
  
  # ── Build JSON structure ──────────────────────────────────────────
  
  json_list <- list(
    
    metadata = list(
      model_id = model_id,
      model_type = "Direct-Physical",
      description = description,
      version = version,
      time_unit = model$time_unit
    ),
    
    state_space = list(
      
      measure_timestamp = timestamp,
      
      sampling_time = model$dt,
      
      nx = model$n_states,
      nu = model$n_inputs,
      nd = model$n_dist,
      ny = model$n_rooms,
      
      state_names = state_names,
      input_names = input_names,
      disturbance_names = disturbance_names,
      output_names = output_names,
      
      units = units,
      
      Kgain = list(Kgain),
      
      x0 = as.numeric(x0),
      
      A = unname(split(model$A, row(model$A))),
      B = unname(split(model$B, row(model$B))),
      E = unname(split(model$E, row(model$E))),
      C = unname(split(model$C, row(model$C)))
    )
  )
  
  # ── Write JSON file ───────────────────────────────────────────────
  
  write_json(
    json_list,
    path = file_name,
    pretty = TRUE,
    auto_unbox = TRUE,
    digits = NA
  )
  
  cat(sprintf("JSON model written to: %s\n", file_name))
  
  return(json_list)
}