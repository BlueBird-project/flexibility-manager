HourlyTemperatureModel <- function(df, studied_space, collindant_spaces, lags_temperature, lags_setpointTemperature, lags_collindant_setpoint_temperature,
                                 lags_outdoorTemperature, lags_collindant_temperature, sensor_sensitivity, predictionHorizonInHours, modelId, plots=F, modelMode="train"){
  
  suppressMessages(suppressWarnings(library(ggplot2)))
  suppressMessages(suppressWarnings(library(htmlwidgets)))
  suppressMessages(suppressWarnings(library(plotly)))
  suppressMessages(suppressWarnings(library(carrier)))
  suppressMessages(suppressWarnings(library(digest)))
  
  modelName <- "HourlyTemperatureModel"
  manualplotsName <- sub("_Model","_", modelId)
  
  if (!dir.exists(paste0(settings$OutputDataDirectory))){dir.create(settings$OutputDataDirectory,F)}
  if (!dir.exists(paste0(settings$OutputDataDirectory,"/models"))){dir.create(paste0(settings$OutputDataDirectory,"/models"),F)}
  if(plots){
    dir.create(paste(settings$OutputDataDirectory,"plots",sep="/"),F)
  }
  
  
  exp_filter <- function(x, alpha) {
    y <- numeric(length(x))
    y[1] <- x[1]                
    
    for (i in 2:length(x)) {
      y[i] <- alpha * x[i] + (1 - alpha) * y[i - 1]
    }
    
    return(y)
  }
  
  # exp_filter_contiguous_sec <- function(time, x, alpha, expected_dt_sec) {
  #   y <- numeric(length(x))
  #   y[1] <- x[1]
  #   
  #   for (i in 2:length(x)) {
  #     dt_min <- as.numeric(time[i] - time[i - 1], units = "secs")
  #     
  #     if (dt_min > expected_dt_sec * 1.1) {
  #       # Si no es contiguo: reiniciar filtro
  #       y[i] <- x[i]
  #     } else {
  #       # Si es contiguo: aplicar suavizado
  #       y[i] <- alpha * x[i] + (1 - alpha) * y[i - 1]
  #     }
  #   }
  #   return(y)
  # }
  
  exp_filter_contiguous_sec <- function(time, x, alpha, expected_dt_sec) {
    
    y <- numeric(length(x))
    
    y[1] <- x[1]
    
    for (i in 2:length(x)) {
      
      # if (is.na(time[i]) || is.na(time[i - 1])) {
      #   y[i] <- y[i - 1]
      #   next
      # }
      
      dt_sec <- as.numeric(time[i] - time[i - 1], units = "secs")
      
      # if (is.na(x[i])) {
      #   y[i] <- y[i - 1]
      #   next
      # }
      
      if (is.na(x[i - 1])) {
        y[i] <- x[i]
        next
      }
      
      if (is.na(dt_sec) || dt_sec > expected_dt_sec * 1.1) {
        y[i] <- x[i]
        next
      }
      
      y[i] <- alpha * x[i] + (1 - alpha) * y[i - 1]
    }
    
    return(y)
  }
  iso_to_seconds <- function(duration_iso) {
    p <- lubridate::as.period(duration_iso)
    p@year * 365*24*3600 +
      p@month * 30*24*3600 +
      p@day * 24*3600 +
      p@hour * 3600 +
      p@minute * 60 +
      p@.Data
  }
  
  ####
  # TRAIN AND PREDICT ----
  ####
  
  write("## Training process is starting",stderr())
  
  df <- df %>% 
    dplyr::select(time, localtime, airTemperature,OperationMode, GHI, WindSpeed,
                  azimuth,elevation,WindDir,
                  colnames(df)[Reduce(`|`, lapply(collindant_spaces, function(pat) {
                    grepl(paste0("\\AmbTemp_",pat,"\\b"), colnames(df))
                  }))],
                  colnames(df)[Reduce(`|`, lapply(studied_space, function(pat) {
                    grepl(paste0("\\AmbTemp_",pat,"\\b"), colnames(df))
                  }))],
                  colnames(df)[Reduce(`|`, lapply(studied_space, function(pat) {
                    grepl(paste0("\\TempSP_",pat,"\\b"), colnames(df))
                  }))],
                  colnames(df)[Reduce(`|`, lapply(studied_space, function(pat) {
                    grepl(paste0("\\PW_",pat,"\\b"), colnames(df))
                  }))],
                  colnames(df)[Reduce(`|`, lapply(studied_space, function(pat) {
                    grepl(paste0("\\OpMode_",pat,"\\b"), colnames(df))
                  }))],
                  colnames(df)[Reduce(`|`, lapply(collindant_spaces, function(pat) {
                    grepl(paste0("\\TempSP_",pat,"\\b"), colnames(df))
                  }))],
                  colnames(df)[Reduce(`|`, lapply(collindant_spaces, function(pat) {
                    grepl(paste0("\\PW_",pat,"\\b"), colnames(df))
                  }))]
    )
  
  #Create fs components
  df <- cbind(df,"azimuth" =onlineforecast::fs(df$azimuth/360, nharmonics = 2))
  df <- cbind(df,"elevation" =onlineforecast::fs(df$azimuth/360, nharmonics = 2))
  df <- cbind(df,"WindDir" =onlineforecast::fs(df$azimuth/360, nharmonics = 2))
  
  # Set all temperatures in kelvins
  df <- df %>% mutate(across(all_of(colnames(df)[Reduce(`|`, lapply(collindant_spaces, function(pat) {
    grepl(paste0("\\AmbTemp_",pat,"\\b"), colnames(df))
  }))]), ~ . + 273.15)) %>%
    mutate(across(all_of(colnames(df)[Reduce(`|`, lapply(studied_space, function(pat) {
      grepl(paste0("\\AmbTemp_",pat,"\\b"), colnames(df))
    }))]), ~ . + 273.15))  
  
  if(modelMode!="dynamicsEstimator") {
    df <- df %>% 
      mutate(across(all_of(colnames(df)[Reduce(`|`, lapply(studied_space, function(pat) {
        grepl(paste0("\\TempSP_",pat,"\\b"), colnames(df))
      }))]), ~ . + 273.15)) %>% 
      mutate(across(all_of(colnames(df)[Reduce(`|`, lapply(collindant_spaces, function(pat) {
        grepl(paste0("\\TempSP_",pat,"\\b"), colnames(df))
      }))]), ~ . + 273.15))
  }
  
  
  # Rename the studied temperature and the output temperature as their standard names
  df <- df %>% rename(temperature = all_of(colnames(df)[Reduce(`|`, lapply(studied_space, function(pat) {
    grepl(paste0("\\AmbTemp_",pat,"\\b"), colnames(df))
  }))])) %>%
    rename(outdoorTemperature = airTemperature)
  
  #If cooling mode and setpoint < temp -> Setpoint = Setpoint, else 0
  #If heating mode and setpoint > temp -> Setpoint = Setpoint, else 0
  #For the collindant spaces
  
  if(modelMode!="dynamicsEstimator") {
    df <- df %>% mutate(across(all_of(colnames(df)[Reduce(`|`, lapply(studied_space, function(pat) {
      grepl(paste0("\\TempSP_",pat,"\\b"), colnames(df))
    }))]), ~ ifelse((.>temperature & OperationMode<3), . , ifelse((.<temperature & OperationMode>2), ., 0))))
    if(!is.null(collindant_spaces))
      df <-df %>%
        mutate(
          !!!setNames(
            map(collindant_spaces, function(pat) {
              if_else(
                (.[[paste0("TempSP_", pat)]] > (.[[paste0("AmbTemp_", pat)]]-sensor_sensitivity) & (.[["OperationMode"]]<3)),
                .[[paste0("TempSP_", pat)]],
                if_else((.[[paste0("TempSP_", pat)]] < (.[[paste0("AmbTemp_", pat)]]+sensor_sensitivity) & .[["OperationMode"]]>2),
                        .[[paste0("TempSP_", pat)]], 0)
              )
            }),
            paste0("TempSP_",collindant_spaces)
          )
        )
  }
  
  
  df %>% mutate(outdoorTemperature=exp_filter(outdoorTemperature,0.8))
  
  
  
  # Instead of using the temperatures, use the difference between temperatures and the studied space temperature
  # df <- df %>% mutate(across(all_of(colnames(df)[Reduce(`|`, lapply(collindant_spaces, function(pat) {
  #   grepl(paste0("\\AmbTemp_",pat,"\\b"), colnames(df))
  # }))]), ~ . - temperature)) %>% 
  #   mutate(diffTemperature = outdoorTemperature -temperature) 
  df <- df %>% mutate(diffTemperature = outdoorTemperature -temperature)
  
  
  if(modelMode!="dynamicsEstimator") {
    df <- df %>%
      reduce(collindant_spaces, function(data, num) {
        mutate(data, !!paste0("TempSP_", num) := .[[paste0("TempSP_", num)]] * .[[paste0("PW_", num)]])
      }, .init = .) %>% select(-colnames(df)[Reduce(`|`, lapply(collindant_spaces, function(pat) {
        grepl(paste0("\\PW_",pat,"\\b"), colnames(df))
      }))])
    
    #If cooling mode and setpoint < temp -> Setpoint = Setpoint, else 0
    #If heating mode and setpoint > temp -> Setpoint = Setpoint, else 0
    #For the studied space
    df <- df %>% mutate(across(all_of(colnames(df)[Reduce(`|`, lapply(studied_space, function(pat) {
      grepl(paste0("\\TempSP_",pat,"\\b"), colnames(df))
    }))]), ~ ifelse((.>(temperature-sensor_sensitivity) & OperationMode<3), . , ifelse((.<(temperature+sensor_sensitivity) & OperationMode>2), ., 0))))
  }
  
  
  #Generate the lags of temperature variables that will be added to the model
  
  # lags_temperature <- lags_temperature
  if (lags_temperature>0) {
    for (i in 1:lags_temperature) {
      df <- df %>%
        mutate(!!paste0("temperature_l", i) := lag(temperature, i))
    }
  }
  
  # lags_outdoorTemperature <- lags_outdoorTemperature
  if (lags_outdoorTemperature>0){
    for (i in 1:lags_outdoorTemperature) {
      df <- df %>%
        mutate(!!paste0("outdoorTemperature_l", i) := lag(outdoorTemperature, i))
    }
  }
  
  # lags_collindant_temperature <- lags_collindant_temperature
  if (lags_collindant_temperature > 0 &!is.null(collindant_spaces)){
    df <- df %>% bind_cols(map_dfc(colnames(df)[Reduce(`|`, lapply(collindant_spaces, function(pat) {
      grepl(paste0("\\AmbTemp_",pat,"\\b"), colnames(df))
    }))], function(col) {
      map_dfc(1:lags_collindant_temperature, function(lag_n) {
        lag(df[[col]], lag_n)
      }) %>%
        setNames(paste0(col, "_l", 1:lags_collindant_temperature))
    }))
  }
  
  
  if(modelMode!="dynamicsEstimator") {
    if (lags_setpointTemperature>0) {
      df <- df %>% bind_cols(map_dfc(colnames(df)[Reduce(`|`, lapply(studied_space, function(pat) {
        grepl(paste0("\\TempSP_",pat,"\\b"), colnames(df))
      }))], function(col) {
        map_dfc(1:lags_setpointTemperature, function(lag_n) {
          lag(df[[col]], lag_n)
        }) %>%
          setNames(paste0(col, "_l", 1:lags_setpointTemperature))
      }))
    }
    
    if (lags_collindant_setpoint_temperature>0 & !is.null(collindant_spaces)) {
      df <- df %>% bind_cols(map_dfc(colnames(df)[Reduce(`|`, lapply(collindant_spaces, function(pat) {
        grepl(paste0("\\TempSP_",pat,"\\b"), colnames(df))
      }))], function(col) {
        map_dfc(1:lags_collindant_setpoint_temperature, function(lag_n) {
          lag(df[[col]], lag_n)
        }) %>%
          setNames(paste0(col, "_l", 1:lags_collindant_setpoint_temperature))
      }))
    }
  }
  
 
  
  #Delete NA rows appeared due to the maximmum lag of the temperatures
  if(modelMode!="dynamicsEstimator") {df <- df[(max(lags_outdoorTemperature,lags_collindant_temperature,lags_temperature)+1):length(df$time),]}
  
  #Select only values for which the HVAC system is in ON (1) state
  df <- df %>%
    filter(
      if_any(starts_with("PW_"), ~ . >= 1)
    )
  #Set localtime column
  # df$localtime <- df$time
  
  #Separate between the data when the HVAC is in heating system and when it is in cooling system
  df_heating <- df[df$OperationMode<3,]
  df_cooling <- df[df$OperationMode>2,]
  
  #Smooth temperatures timeseries (only when they are contiguous samples)
  
  if(nrow(df_heating)>1) {
    #Smooth heating mode space temperature
    df_heating <- df_heating %>%
      mutate(
        across(
          contains("\\temperature\\b"),
          ~ exp_filter_contiguous_sec(
            time = df_heating$time,    
            x    = .,           
            alpha = 0.6,
            expected_dt_sec = iso_to_seconds(detect_time_step(df))    
          )
        )
      )
    #Smooth heating mode collindant spaces temperatures
    df_heating <- df_heating %>%
      mutate(
        across(
          contains("AmbTemp"),
          ~ exp_filter_contiguous_sec(
            time = df_heating$time,     # vector timestamps
            x    = .,           # la columna actual
            alpha = 0.6,
            expected_dt_sec = iso_to_seconds(detect_time_step(df))        # parámetro del filtro
          )
        )
      )
  }
  if(nrow(df_cooling)>1) {
    #Smooth cooling mode space temperatures
    df_cooling <- df_cooling %>%
      mutate(
        across(
          contains("temperature"),
          ~ exp_filter_contiguous_sec(
            time = df_cooling$time,     # vector timestamps
            x    = .,           # la columna actual
            alpha = 0.6,
            expected_dt_sec = iso_to_seconds(detect_time_step(df))        # parámetro del filtro
          )
        )
      )
    #Smooth cooling mode collindant spaces temperatures
    df_cooling <- df_cooling %>%
      mutate(
        across(
          contains("AmbTemp"),
          ~ exp_filter_contiguous_sec(
            time = df_cooling$time,     # vector timestamps
            x    = .,           # la columna actual
            alpha = 0.6,
            expected_dt_sec = iso_to_seconds(detect_time_step(df))        # parámetro del filtro
          )
        )
      )
  }
  

  if (modelMode!="train" && file.exists(paste0(settings$OutputDataDirectory,"/models/",modelId,".rds"))) {
    
    #Upload the existent models and their training period information
    loadModel <- readRDS(paste0(settings$OutputDataDirectory,"/models/",modelId,".rds"))
    trained_models <- loadModel$trained_models
    modelInfo <- loadModel$modelInfo
    
    write(sprintf("The existent model has been uploaded, the initial training date of the model is: %s and the final training date is: %s.", 
                  modelInfo$FirstDate, modelInfo$Lastdate),stderr())
    
  } else{

    # Assess the occupancy scenario only when a minimum number of days with non-outlier data are available  
    # inside the min-max range of days after the EEM (all of them, defined in settings/EEMAssessmentConditions)
    if(sum(!is.na(df$temperature)) <= 
       hourly_timesteps(settings$EEMAssessmentConditions$MinDaysWithoutOutliers$PT1H*24, detect_time_step(df$time))) {
    
      write(sprintf(
        "* %s wasn't assessed because there are not enough valid days to make the estimation", studied_space) ,stderr())
     return(NULL)
    
    } else {
      
      ###
      # Setting the model parameters and transformations to be done during the model training ----
      ###
      
      write("* Setting model parameters and transformations",stderr())
      
      
      #
      
      # Collindant spaces temperature variables transformations that will be added to the model
      # if(!is.null(collindant_spaces)) {
      #   # Collindant spaces temperature (and its lags) transformations that will be added to the model
      #   collindant_spaces_transformations <- setNames(
      #     lapply(colnames(df)[Reduce(`|`, lapply(collindant_spaces, function(pat) {
      #       grepl(paste0("AmbTemp_",pat,"_l"), colnames(df))
      #     }))], function(c) paste0("lpf_ts(...,featuresNames='",c,"',smoothingTimeScaleParameter=param$alpha_c",", inplace=F)")),
      #     paste0(colnames(df)[Reduce(`|`, lapply(collindant_spaces, function(pat) {
      #       grepl(paste0("AmbTemp_",pat,"_l"), colnames(df))
      #     }))], "_lpf")
      #   )
      #   # Collindant spaces setpoint temperature (and its lags) transformations that will be added to the model
      #   setpointCollindantTemperature_transformations <- setNames(
      #     lapply(colnames(df)[Reduce(`|`, lapply(collindant_spaces, function(pat) {
      #       grepl(paste0("TempSP_",pat,""), colnames(df))
      #     }))], function(c) paste0("lpf_ts(...,featuresNames='",c,"',smoothingTimeScaleParameter=param$alpha_csp",", inplace=F)")),
      #     paste0(colnames(df)[Reduce(`|`, lapply(collindant_spaces, function(pat) {
      #       grepl(paste0("TempSP_",pat,""), colnames(df))
      #     }))], "_lpf")
      #   )
      # } else {
      #   collindant_spaces_transformations <- list()
      #   setpointCollindantTemperature_transformations <- list()
      # }
      # 
      # # Outdoor temperature (and its lags) transformations that will be added to the model
      # outdoor_temperature_transformations <- setNames(
      #   lapply(colnames(df)[Reduce(`|`, lapply("outdoorTemperature", function(pat) {
      #     grepl(paste0(pat), colnames(df))
      #   }))], function(c) paste0("lpf_ts(...,featuresNames='",c,"',smoothingTimeScaleParameter=param$alpha_ot",", inplace=F)")),
      #   paste0(colnames(df)[Reduce(`|`, lapply("outdoorTemperature", function(pat) {
      #     grepl(paste0(pat), colnames(df))
      #   }))], "_lpf")
      # )
      # 
      # # Studied space temperature lags transformations that will be added to the model
      # temperature_transformations <- setNames(
      #   lapply(colnames(df)[Reduce(`|`, lapply("temperature_", function(pat) {
      #     grepl(paste0(pat), colnames(df))
      #   }))], function(c) paste0("lpf_ts(...,featuresNames='",c,"',smoothingTimeScaleParameter=param$alpha_t",", inplace=F)")),
      #   paste0(colnames(df)[Reduce(`|`, lapply("temperature_", function(pat) {
      #     grepl(paste0(pat), colnames(df))
      #   }))], "_lpf")
      # )
      # 
      # # Studied space temperature setpoint (and its lags) transformations that will be added to the model
      # setpointTemperature_transformations <- setNames(
      #   lapply(colnames(df)[Reduce(`|`, lapply(studied_space, function(pat) {
      #     grepl(paste0("TempSP_",pat,""), colnames(df))
      #   }))], function(c) paste0("lpf_ts(...,featuresNames='",c,"',smoothingTimeScaleParameter=param$alpha_sp",", inplace=F)")),
      #   paste0(colnames(df)[Reduce(`|`, lapply(studied_space, function(pat) {
      #     grepl(paste0("TempSP_",pat,""), colnames(df))
      #   }))], "_lpf")
      # )
      
      
      if(!is.null(collindant_spaces)) {
        collindant_spaces_params <- list(
          "alpha_c"=list(
            "datatype"="discrete",
            "levels"=c(0.6,0.8,0.9,0.95,0.975)
          ))
        setpointCollindantTemperature__params <- list(
          "alpha_csp"=list(
            "datatype"="discrete",
            "levels"=c(0.6,0.8,0.9,0.95,0.975)
          ))
      } else {
        collindant_spaces_params <- list()
        setpointCollindantTemperature__params <- list()
      }
      
      outdoor_temperature_params <- list(
        "alpha_ot"=list(
          "datatype"="discrete",
          "levels"=c(0.6,0.8,0.9,0.95,0.975)
        ))
      
      temperature_params <- list(
        "alpha_t"=list(
          "datatype"="discrete",
          "levels"=c(0.6,0.8,0.9,0.95,0.975)
        ))
      
      setpointTemperature__params <- list(
        "alpha_sp"=list(
          "datatype"="discrete",
          "levels"=c(0.6,0.8,0.9,0.95,0.975)
        ))
      
      
      generalParams <- list(
        "nhar"=list(
          "datatype"="integer",
          "nlevels"=4,
          "min"=6,
          "max"=10
        ),
        "lambda"=list(
          "datatype"="discrete",
          "levels"=c(get_lpf_smoothing_time_scale(data.frame("time"=df$time),timeConstantInHours = 24*31*24),
                     get_lpf_smoothing_time_scale(data.frame("time"=df$time),timeConstantInHours = 18*31*24),
                     get_lpf_smoothing_time_scale(data.frame("time"=df$time),timeConstantInHours = 12*31*24),
                     get_lpf_smoothing_time_scale(data.frame("time"=df$time),timeConstantInHours = 10*31*24),
                     get_lpf_smoothing_time_scale(data.frame("time"=df$time),timeConstantInHours = 6*31*24),
                     get_lpf_smoothing_time_scale(data.frame("time"=df$time),timeConstantInHours = 3*31*24),
                     get_lpf_smoothing_time_scale(data.frame("time"=df$time),timeConstantInHours = 2*31*24),
                     get_lpf_smoothing_time_scale(data.frame("time"=df$time),timeConstantInHours = 1*31*24))
        ),
        "alpha_sol"=list(
          "datatype"="discrete",
          "levels"=c(0.6,0.8,0.9,0.95,0.975)
        ),
        "alpha_wind"=list(
          "datatype"="discrete",
          "levels"=c(0.6,0.8,0.9,0.95,0.975)
        )
      )
      generalTransformationSentences <- list(
        # # Fill some gaps in the outdoor temperature time series.
        # "temperature" = "na.locf(
        #                         na.locf(
        #                           na.approx(temperature,na.rm = F),
        #                           fromLast = T, na.rm = F
        #                         ),
        #                         na.rm=F)",
        
        # Low Pass Filtered (LPF) outdoor temperature
        # "wslpf" = "lpf_ts(...,featuresNames='WindSpeed',smoothingTimeScaleParameter=param$alpha_wind,
        #                  inplace=F)",
        # "slp" = "lpf_ts(...,featuresNames='GHI',smoothingTimeScaleParameter=param$alpha_sol,
        #                  inplace=F)",
        "scos1" = "azimuth.cos1*elevation.cos1*GHI",
        "ssin1" = "azimuth.sin1*elevation.sin1*GHI",
        "scos2" = "azimuth.cos2*elevation.cos2*GHI",
        "ssin2" = "azimuth.sin2*elevation.sin2*GHI",
        "wcos1" = "WindDir.cos1*WindSpeed*diffTemperature",
        "wsin1" = "WindDir.sin1*WindSpeed*diffTemperature",
        "wcos2" = "WindDir.cos2*WindSpeed*diffTemperature",
        "wsin2" = "WindDir.sin2*WindSpeed*diffTemperature"
      )
      #List all the parameters for the model
      params <- c(generalParams)
      #List all the transformation sententences for the model
      transformationSentences <- c(generalTransformationSentences)
      
      
      predictors <- c(names(transformationSentences[!names(transformationSentences)%in%c("wslpf","slp")]),
                      grep("temperature_l", names(df), value = TRUE),
                      grep("outdoorTemperature", names(df), value = TRUE),
                      grep("TempSP_", names(df), value = TRUE),
                      names(df)[grepl("AmbTemp", names(df)) & grepl("l", names(df))])
      
      # trControl <- trainControl(method="none")
      
      formula <- (paste("temperature", "~", paste(predictors, collapse = " + ")))
      
      # formula <- paste("temperature", "~", "tlpf + outdoorTemperature + wcos1")
      
      
      trControl <- trainControl(method = "repeatedcv",
                                number=5,
                                repeats=2)
      logOutput <- F
      
      ###
      # Model training ----
      ###
      # 
      # n <- nrow(df)
      # n_train <- floor(0.7 * n)
      # df_train <- df[1:n_train,]
      # df_test <- df[(n_train+1):nrow(df),]
      
      write("* Training of the model",stderr())
      
      minMonthsTraining <- 9
      model <- function(params, df, formula, transformationSentences, ...){
        Temp_model <- eval(parse(text = paste0(
          '  function(params, df, ...) {
        args <- list(...)
        train(',
          formula,',
          data=df,
          method = RLS(
            data.frame(parameter = names(params),
                       class = mapply(names(params),FUN=function(i) params[[i]][','"datatype"',']))
          ),
          tuneGrid = expand.grid(params),
          trControl = trControl,
          logOutput = logOutput,
          minMonthsTraining = minMonthsTraining,
          continuousTime = F,
          transformationSentences = args$transformationSentences
        )
      }')))
        # Temp_model <- function(params, df, ...) {
        #     args <- list(...)
        #     train(
        #       temperature ~ tlpf + outdoorTemperature,
        #       data=df,
        #       method = RLS(
        #         data.frame(parameter = names(params),
        #                    class = mapply(names(params),FUN=function(i) params[[i]]["datatype"]))
        #       ),
        #       tuneGrid = expand.grid(params),
        #       trControl = trControl,
        #       logOutput = logOutput,
        #       minMonthsTraining = minMonthsTraining,
        #       continuousTime = T,
        #       transformationSentences = args$transformationSentences
        #     )
        #   }
        best_params <- hyperparameters_tuning(
          opt_criteria = "minimise",
          opt_function = function(X, df, ...) {
            mod <- Temp_model(X, df, ...)
            expected <- df$temperature
            obtained <- biggr::predict.train(
              object = mod,
              newdata = df,
              forceGlobalInputFeatures = NULL,
              predictionHorizonInHours = 0,
              modelWindow = NULL,
              modelSelection = NULL
            )
            RMSE(expected[(length(expected)*0.25):(length(expected)-24)], obtained[(length(obtained)*0.25):(length(obtained)-24)], na.rm = T)
          },
          features = params,
          maxiter = 5,
          popSize = 10,
          df = df,
          parallel = F,
          transformationSentences = transformationSentences
        )
        mod <- Temp_model(best_params, df,
                          transformationSentences = transformationSentences
        )
        return(mod)
      }
      
      # df_heating_train <- rbind(
      #   df_heating,
      #   transform(df_heating[nrow(df_heating), ][rep(1, 24), ],
      #             time = seq(df_heating$time[nrow(df_heating)] + 3600, by = "hour", length.out = 24),
      #             localtime = seq(df_heating$localtime[nrow(df_heating)] + 3600, by = "hour", length.out = 24))
      # )
      # 
      # df_cooling_train <- rbind(
      #   df_cooling,
      #   transform(df_cooling[nrow(df_cooling), ][rep(1, 24), ],
      #             time = seq(df_cooling$time[nrow(df_cooling)] + 3600, by = "hour", length.out = 24),
      #             localtime = seq(df_cooling$localtime[nrow(df_cooling)] + 3600, by = "hour", length.out = 24))
      # )
      
      # predictor_df<- df[(length(df$time)-23):length(df$time),]
      # predictor_df$time <- predictor_df$time + (3600*24)
      # predictor_df$localtime <- predictor_df$localtime+(3600*24)
      
      
      # Generate trained models
      trained_models <- list(
        "heating" = model(
          params=params,
          df = df_heating,
          formula=formula,
          transformationSentences = transformationSentences
        ),
        "cooling" = model(
          params=params,
          df = df_cooling,
          formula=formula,
          transformationSentences = transformationSentences
        )
      )
      
      # df_test <- rbind(df[2330:2339,],df[1431:1441,],df[2061:2068,])
      
      
      # Generate the predictor object for each model
      predictor_heating <- carrier::crate(function(x, forceGlobalInputFeatures = NULL,predictionHorizonInHours=predictionHorizonInHours,
                                                   modelWindow="%Y-%m-%d", modelSelection="rmse", forceOneStepPrediction=T){
        mod <- !!trained_models
        biggr::predict.train(
          object = mod[["heating"]],
          newdata = x, 
          forceGlobalInputFeatures = forceGlobalInputFeatures,
          predictionHorizonInHours = predictionHorizonInHours,
          forceOneStepPrediction = forceOneStepPrediction
        )
      })
      predictor_cooling <- carrier::crate(function(x, forceGlobalInputFeatures = NULL,predictionHorizonInHours=predictionHorizonInHours,
                                                   modelWindow="%Y-%m-%d", modelSelection="rmse", forceOneStepPrediction=T){
        mod <- !!trained_models
        biggr::predict.train(
          object = mod[["cooling"]],
          newdata = x, 
          forceGlobalInputFeatures = forceGlobalInputFeatures,
          predictionHorizonInHours = predictionHorizonInHours,
          forceOneStepPrediction = forceOneStepPrediction
        )
      })
      # predictor_cooling(x=df_cooling_predictor, predictionHorizonInHours=predictionHorizonInHours)
      #Store information about the model
      modelInfo <- {}
      
      #Training period
      modelInfo$FirstDate = parsedate::format_iso_8601(min(df$time))
      modelInfo$Lastdate = parsedate::format_iso_8601(max(df$time))
      
      # Store the errors
      df_aux <- df
      
      df_aux_heating <- df_heating
      if (nrow(df_heating>0)) {
        df_aux_heating$predicted <- predictor_heating(df_aux_heating, predictionHorizonInHours=predictionHorizonInHours)
      }
      df_aux_cooling <- df_cooling
      if (nrow(df_cooling>0)) {
        df_aux_cooling$predicted <- predictor_cooling(df_aux_cooling, predictionHorizonInHours=predictionHorizonInHours)
      } 
      
      df_aux <- df_aux %>% 
        full_join(rbind(df_aux_heating, df_aux_cooling) %>% select(time, predicted) %>% distinct(),by = "time") %>%
        arrange(time)
      df_aux$predicted[df_aux$predicted < 0] <- 0
      
      # df_eval_aux <- df_aux[(nrow(df_aux)-30*24):nrow(df_aux),]
      if (nrow(df_aux_heating)>30*24) {
        df_eval_aux_heating <- df_aux_heating[(nrow(df_aux_heating)-30*24):(nrow(df_aux_heating)),]
      } else {
        df_eval_aux_heating <- df_aux_heating
      }
      modelInfo$MAEmetric_heating <- MAE(df_eval_aux_heating$temperature,
                                         df_eval_aux_heating$predicted,na.rm = T)
      modelInfo$RMSEmetric_heating <- RMSE(df_eval_aux_heating$temperature,
                                           df_eval_aux_heating$predicted,na.rm = T)
      modelInfo$CVRMSEmetric_heating <- modelInfo$RMSEmetric_heating / mean(df_eval_aux_heating$temperature,na.rm=T)
      modelInfo$r2metric_heating <- cor(df_eval_aux_heating$temperature,
                                        df_eval_aux_heating$predicted,
                                        use = "na.or.complete")^2
      
      if (nrow(df_aux_cooling)>30*24) {
        df_eval_aux_cooling <- df_aux_cooling[(nrow(df_aux_cooling)-30*24):(nrow(df_aux_cooling)),]
      } else
      {
        df_eval_aux_cooling <- df_aux_cooling
      }
      modelInfo$MAEmetric_cooling <- MAE(df_eval_aux_cooling$temperature,
                                         df_eval_aux_cooling$predicted,na.rm = T)
      modelInfo$RMSEmetric_cooling <- RMSE(df_eval_aux_cooling$temperature,
                                           df_eval_aux_cooling$predicted,na.rm = T)
      modelInfo$CVRMSEmetric_cooling <- modelInfo$RMSEmetric_cooling / mean(df_eval_aux_cooling$temperature,na.rm=T)
      modelInfo$r2metric_cooling <- cor(df_eval_aux_cooling$temperature,
                                        df_eval_aux_cooling$predicted,
                                        use = "na.or.complete")^2
      
      # Store the final model parameters obtained from hyperparameter tunning
      modelInfo$HeatingParameters <- list()
      for (i in 1:ncol(trained_models[["heating"]]$bestTune)){
        modelInfo$HeatingParameters[[colnames(trained_models[["heating"]]$bestTune)[i]]] <-
          as.character(trained_models[["heating"]]$bestTune[1,i])
      }
      modelInfo$CoolingParameters <- list()
      for (i in 1:ncol(trained_models[["cooling"]]$bestTune)){
        modelInfo$CoolingParameters[[colnames(trained_models[["cooling"]]$bestTune)[i]]] <-
          as.character(trained_models[["cooling"]]$bestTune[1,i])
      }
      
      #Generate a list with the model info and the trained models 
      saveModel <- list(
        trained_models = trained_models,
        modelInfo = modelInfo
      )
      #Save the trained models and its training period information
      saveRDS(saveModel, file=paste0(settings$OutputDataDirectory,"/models/",modelId,".rds"))
    }
  }
  
  #Results
  
  df_result <- df
  
  if(modelMode=="train") {
    # Generate the predictor objects
    predictor_heating <- carrier::crate(function(x, forceGlobalInputFeatures = NULL,predictionHorizonInHours,
                                                 modelWindow="%Y-%m-%d", modelSelection="rmse", forceOneStepPrediction=T){
      mod <- !!trained_models
      biggr::predict.train(
        object = mod[["heating"]],
        newdata = x,
        forceGlobalInputFeatures = forceGlobalInputFeatures,
        predictionHorizonInHours = predictionHorizonInHours,
        forceOneStepPrediction = forceOneStepPrediction
      )
    })
    predictor_cooling <- carrier::crate(function(x, forceGlobalInputFeatures = NULL, predictionHorizonInHours,
                                                 modelWindow="%Y-%m-%d", modelSelection="rmse", forceOneStepPrediction=T){
      mod <- !!trained_models
      biggr::predict.train(
        object = mod[["cooling"]],
        newdata = x,
        forceGlobalInputFeatures = forceGlobalInputFeatures,
        predictionHorizonInHours = predictionHorizonInHours,
        forceOneStepPrediction = forceOneStepPrediction
      )
    })
    df_result_heating <- df_heating
    if (nrow(df_heating>0)) {
      df_result_heating$predicted <- predictor_heating(df_result_heating, predictionHorizonInHours=predictionHorizonInHours)
    } 
    df_result_cooling <- df_cooling
    if (nrow(df_cooling>0)) {
      df_result_cooling$predicted <- predictor_cooling(df_result_cooling, predictionHorizonInHours=predictionHorizonInHours)
    } 
    if (nrow(df_result_heating)+nrow(df_result_cooling)>0) {
      df_result <- df_result %>% 
        full_join(rbind(df_result_heating, df_result_cooling) %>% select(time, predicted) %>% distinct(),by = "time") %>%
        arrange(time)
    } else {
      df_result$predicted <- NA}
  }
  
  if(modelMode=="predict"){
    df_result_heating <- df_heating
    if (nrow(df_heating>0)) {
      df_result_heating_trans <- data_transformation_wrapper(data=df_result_heating,
                                                             features=trained_models$heating$finalModel$meta$features,
                                                             transformationSentences = trained_models$heating$finalModel$meta$transformationSentences,
                                                             param=trained_models$heating$finalModel$meta$param)
      fterms <- attr(terms(trained_models$heating$finalModel$meta$formula), "term.labels")
      
      df_result_heating_vars <- as.matrix(df_result_heating_trans$data[, fterms, drop = FALSE])
      df_result_heating_coefs <- as.numeric(trained_models$heating$finalModel$coefficients[nrow(trained_models$heating$finalModel$coefficients),])
      df_result_heating$predicted <- as.numeric(df_result_heating_vars %*% df_result_heating_coefs)
    } 
    
    df_result_cooling <- df_cooling
    if (nrow(df_cooling>0)) {
      df_result_cooling_trans <- data_transformation_wrapper(data=df_result_cooling,
                                                             features=trained_models$cooling$finalModel$meta$features,
                                                             transformationSentences = trained_models$cooling$finalModel$meta$transformationSentences,
                                                             param=trained_models$cooling$finalModel$meta$param)
      fterms <- attr(terms(trained_models$cooling$finalModel$meta$formula), "term.labels")
      
      df_result_cooling_vars <- as.matrix(df_result_cooling_trans$data[, fterms, drop = FALSE])
      df_result_cooling_coefs <- as.numeric(trained_models$cooling$finalModel$coefficients[nrow(trained_models$cooling$finalModel$coefficients),])
      df_result_cooling$predicted <- as.numeric(df_result_cooling_vars %*% df_result_cooling_coefs)
    } 
    
    if (nrow(df_result_heating)+nrow(df_result_cooling)>0) {
      df_result <- df_result %>% 
        full_join(rbind(df_result_heating, df_result_cooling) %>% select(time, predicted) %>% distinct(),by = "time") %>%
        arrange(time)
      # #Compute the model residuals
      # df_result <- df_result %>% 
      #   mutate(residuals=temperature-predicted)
    } else {
      df_result$predicted <- NA}
    
  }
  
  
  if(modelMode=="dynamicsEstimator"){
    
    fterms <- attr(terms(trained_models$heating$finalModel$meta$formula), "term.labels")
    df_result_vars <- data_transformation_wrapper(data=df,
                                                           features=grep("TempSP", trained_models$heating$finalModel$meta$features, value = TRUE, invert = TRUE),
                                                           transformationSentences = trained_models$heating$finalModel$meta$transformationSentences,
                                                           param=trained_models$heating$finalModel$meta$param)$data[, grep("TempSP", fterms, value = TRUE, invert = TRUE), drop = FALSE]
    df_result_vars <- cbind(df_result_vars,time=df$time)
    df_result_vars <- df_result_vars %>% select(matches("outdoorTemperature|ssin|scos"),time)
    df_result_vars <- df_result_vars[order(df_result_vars$time), ]
    
    
    df_result_heating <- df_heating
    if (nrow(df_heating)>0) {
      df_result_heating_trans <- data_transformation_wrapper(data=df_result_heating,
                                                             features=grep("TempSP", trained_models$heating$finalModel$meta$features, value = TRUE, invert = TRUE),
                                                             transformationSentences = trained_models$heating$finalModel$meta$transformationSentences,
                                                             param=trained_models$heating$finalModel$meta$param)
      fterms <- attr(terms(trained_models$heating$finalModel$meta$formula), "term.labels")
      
      df_result_heating_vars <- df_result_heating_trans$data[, grep("TempSP", fterms, value = TRUE, invert = TRUE), drop = FALSE]
      df_result_heating_vars <- cbind(df_result_heating_vars,time=df_heating$time)
      df_result_heating_vars <- df_result_heating_vars %>% select(-matches(names(df_result_vars)),time)
      df_result_heating_vars <- df_result_heating_vars %>% select(-matches("TempSP|AmbTemp"))
      df_result_heating_vars <- df_result_heating_vars %>%
        rename_with(
          ~ paste0(.x, "_", studied_space),   # nuevo nombre = nombre original + "_" + variable
          .cols = -matches("time|temperature")     # patrón a buscar
        )
      df_result_heating_vars <- df_result_heating_vars %>%
        rename_with(
          ~ sub("^temperature", paste0("AmbTemp_",studied_space), .x),   # reemplaza “edad” como palabra
          .cols = matches("temperature")               # columnas que contienen “edad”
        )
    }  else {df_result_heating_vars<- data.frame()}
    df_result_heating_coefs <- trained_models$heating$finalModel$coefficients[nrow(trained_models$heating$finalModel$coefficients),]
    df_result_heating_coefs <- df_result_heating_coefs %>%
      rename_with(
        ~ paste0(.x, "_", studied_space),   # nuevo nombre = nombre original + "_" + variable
        .cols = -contains(c(names(df_result_vars),"temperature","TempSP","AmbTemp"))     # patrón a buscar
      )
    df_result_heating_coefs <- df_result_heating_coefs %>%
      rename_with(
        ~ sub("^temperature", paste0("AmbTemp_",studied_space), .x),   # reemplaza “edad” como palabra
        .cols = matches("temperature")               # columnas que contienen “edad”
      )
    # df_result_heating$predicted <- as.numeric(df_result_heating_vars %*% df_result_heating_coefs)
    
    df_result_cooling <- df_cooling
    if (nrow(df_cooling)>0) {
      df_result_cooling_trans <- data_transformation_wrapper(data=df_result_cooling,
                                                             features=grep("TempSP", trained_models$cooling$finalModel$meta$features, value = TRUE, invert = TRUE),
                                                             transformationSentences = trained_models$cooling$finalModel$meta$transformationSentences,
                                                             param=trained_models$cooling$finalModel$meta$param)
      fterms <- attr(terms(trained_models$cooling$finalModel$meta$formula), "term.labels")
      
      df_result_cooling_vars <- df_result_cooling_trans$data[, grep("TempSP", fterms, value = TRUE, invert = TRUE), drop = FALSE]
      df_result_cooling_vars <- cbind(df_result_cooling_vars,time=df_cooling$time)
      df_result_cooling_vars <- df_result_cooling_vars %>% select(-matches(names(df_result_vars)),time)
      df_result_cooling_vars <- df_result_cooling_vars %>% select(-matches("TempSP|AmbTemp"))
      df_result_cooling_vars <- df_result_cooling_vars %>%
        rename_with(
          ~ paste0(.x, "_", studied_space),   # nuevo nombre = nombre original + "_" + variable
          .cols = -matches("time|temperature")     # patrón a buscar
        )
      df_result_cooling_vars <- df_result_cooling_vars %>%
        rename_with(
          ~ sub("^temperature", paste0("AmbTemp_",studied_space), .x),   # reemplaza “edad” como palabra
          .cols = matches("temperature")               # columnas que contienen “edad”
        )
    } else{df_result_cooling_vars<-  data.frame()}
    df_result_cooling_coefs <- trained_models$cooling$finalModel$coefficients[nrow(trained_models$cooling$finalModel$coefficients),]
    df_result_cooling_coefs <- df_result_cooling_coefs %>%
      rename_with(
        ~ paste0(.x, "_", studied_space),   # nuevo nombre = nombre original + "_" + variable
        .cols = -contains(c(names(df_result_vars),"temperature","TempSP","AmbTemp"))     # patrón a buscar
      )
    df_result_cooling_coefs <- df_result_cooling_coefs %>%
      rename_with(
        ~ sub("^temperature", paste0("AmbTemp_",studied_space), .x),   # reemplaza “edad” como palabra
        .cols = matches("temperature")               # columnas que contienen “edad”
      )
    # df_result_cooling$predicted <- as.numeric(df_result_cooling_vars %*% df_result_cooling_coefs)
    
    if (nrow(df_result_heating_vars)+nrow(df_result_cooling_vars)>0) {
      df_result_vars <- df_result_vars %>% full_join(rbind(df_result_heating_vars, df_result_cooling_vars), by="time")
      df_result_vars <- df_result_vars[order(df_result_vars$time), ]
      # #Compute the model residuals
      # df_result <- df_result %>% 
      #   mutate(residuals=temperature-predicted)
    }
    
    
    
    df_result <- list(heatingCoefs = df_result_heating_coefs, coolingCoefs = df_result_cooling_coefs, transformedData = df_result_vars)
  }
  
  plotsName <- paste0("Room",studied_space,"_")
  
  # Plot the predicted temperature and the real one for the evaluation period to evaluate the prediction model results
  if(plots){
    
    if (nrow(df_result_heating)>30*24) {
      df_eval_heating <- df_result_heating[(nrow(df_result_heating)-30*24):nrow(df_result_heating),]
    } else {
      df_eval_heating <- df_result_heating
    }
    pv <- ggplot(df_eval_heating %>% pad(.,by = "time")) + 
      geom_line(aes(time,temperature-273.15, col="Measured") ) + 
      geom_line(aes(time,predicted-273.15,col="Predicted"),alpha=0.5) +
      ylim(c(0,max(df_eval_heating$temperature-273.15,na.rm=T)*1.1)) +
      scale_colour_manual(
        name = NULL,
        values = c("Predicted" = "red", "Measured" = "black")) +
      theme_bw() + xlab("time") + ylab("temperature (ºC)") +
      theme(legend.position = "bottom")
    
    ggsave(paste(paste(settings$OutputDataDirectory,"plots",sep="/"),paste0(plotsName,"validation.png"),sep="/"),
           pv, width=7, height=3)
    
    if (nrow(df_result_cooling)>30*24) {
      df_eval_cooling <- df_result_cooling[(nrow(df_result_cooling)-30*24):nrow(df_result_cooling),]
    } else
    {
      df_eval_cooling <- df_result_cooling
    }
    # df_eval <- df_result[(nrow(df_result)-30*24):nrow(df_result),]
    
    pv <- ggplot(df_eval_cooling %>% pad(.,by = "time")) + 
      geom_line(aes(time,temperature-273.15, col="Measured") ) + 
      geom_line(aes(time,predicted-273.15,col="Predicted"),alpha=0.5) +
      ylim(c(0,max(df_eval_cooling$temperature-273.15,na.rm=T)*1.1)) +
      scale_colour_manual(
        name = NULL,
        values = c("Predicted" = "red", "Measured" = "black")) +
      theme_bw() + xlab("time") + ylab("temperature (ºC)") +
      theme(legend.position = "bottom")
    
    
    ggsave(paste(paste(settings$OutputDataDirectory,"plots",sep="/"),paste0(plotsName,"validation.png"),sep="/"),
           pv, width=7, height=3)
  }
  
  # Plot the predicted temperature and the real one to evaluate the prediction model results for the complete period
  if(plots){
    
    pr <- ggplot(df_result %>% pad(.,by = "time")) + 
      geom_line(aes(time,temperature-273.15, col="Measured") ) + 
      geom_line(aes(time,predicted-273.15,col="Predicted"),alpha=0.5) +
      ylim(c(0,max(df_result$temperature-273.15,na.rm=T)*1.1)) +
      scale_colour_manual(
        name = NULL,
        values = c("Predicted" = "red", "Measured" = "black")) +
      theme_bw() + xlab("time") + ylab("temperature (ºC)") +
      theme(legend.position = "bottom")
    ggsave(paste(paste(settings$OutputDataDirectory,"plots",sep="/"),paste0(plotsName,"RealvsPredicted.png"),sep="/"),
           pr, width=7, height=3)
  }
  #Show that estimation has end and return the results
  write("* Estimation finalised!",stderr())

  #Obtain manually the metrics of the model in case they are needed
  # metrics <-{}
  # 
  # metrics$MAEmetric_heating <- modelInfo$MAEmetric_heating
  # metrics$RMSEmetric_heating <- modelInfo$RMSEmetric_heating
  # metrics$CVRMSEmetric_heating <- modelInfo$CVRMSEmetric_heating
  # metrics$r2metric_heating <- modelInfo$r2metric_heating
  # 
  # metrics$MAEmetric_cooling <- modelInfo$MAEmetric_cooling
  # metrics$RMSEmetric_cooling <- modelInfo$RMSEmetric_cooling
  # metrics$CVRMSEmetric_cooling <- modelInfo$CVRMSEmetric_cooling
  # metrics$r2metric_cooling <- modelInfo$r2metric_cooling


  
  return(df_result)
  # return(list(df_result,metrics))
}