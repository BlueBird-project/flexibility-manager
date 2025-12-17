HourlyHVACEnergyModel <- function(df, buildingSpaces, lags_energy, lags_outdoorTemperature, 
                                  predictionHorizonInHours, modelId, plots=F, modelMode="train"){
  
  suppressMessages(suppressWarnings(library(ggplot2)))
  suppressMessages(suppressWarnings(library(htmlwidgets)))
  suppressMessages(suppressWarnings(library(plotly)))
  suppressMessages(suppressWarnings(library(carrier)))
  suppressMessages(suppressWarnings(library(digest)))
  suppressMessages(suppressWarnings(library(httr)))
  
  modelName <- "HourlyEnergyModel"
  manualplotsName <- sub("_Model","_", modelId)
  
  if (!dir.exists(paste0(settings$OutputDataDirectory))){dir.create(settings$OutputDataDirectory,F)}
  if (!dir.exists(paste0(settings$OutputDataDirectory,"/models"))){dir.create(paste0(settings$OutputDataDirectory,"/models"),F)}
  if(plots){
    dir.create(paste(settings$OutputDataDirectory,"plots",sep="/"),F)
  }
  
  
  ####
  # TRAIN AND PREDICT ----
  ####
  
  write("## Training process is starting",stderr())
  
  df <- df %>%
    dplyr::select(time, localtime, E_Act_2, airTemperature,OperationMode, GHI, WindSpeed, OperationMode,
                  azimuth,elevation,WindDir,
                  colnames(df)[Reduce(`|`, lapply(buildingSpaces, function(pat) {
                    grepl(paste0("\\AmbTemp_",pat,"\\b"), colnames(df))
                  }))],
                  colnames(df)[Reduce(`|`, lapply(buildingSpaces, function(pat) {
                    grepl(paste0("\\TempSP_",pat,"\\b"), colnames(df))
                  }))],
                  colnames(df)[Reduce(`|`, lapply(buildingSpaces, function(pat) {
                    grepl(paste0("\\OpMode_",pat,"\\b"), colnames(df))
                  }))],
                  colnames(df)[Reduce(`|`, lapply(buildingSpaces, function(pat) {
                    grepl(paste0("\\PW_",pat,"\\b"), colnames(df))
                  }))],
                  colnames(df)[Reduce(`|`, lapply(buildingSpaces, function(pat) {
                    grepl(paste0("\\TempPred_",pat,"\\b"), colnames(df))
                  }))]
    )

  
  # Set all temperatures in kelvins
  df <- df %>% mutate(across(all_of(colnames(df)[Reduce(`|`, lapply(buildingSpaces, function(pat) {
    grepl(paste0("\\AmbTemp_",pat,"\\b"), colnames(df))
  }))]), ~ . + 273.15)) %>% mutate(across(all_of(colnames(df)[Reduce(`|`, lapply(buildingSpaces, function(pat) {
    grepl(paste0("\\TempSP_",pat,"\\b"), colnames(df))
  }))]), ~ . + 273.15)) %>% mutate(across(all_of(colnames(df)[Reduce(`|`, lapply(buildingSpaces, function(pat) {
    grepl(paste0("\\TempPred_",pat,"\\b"), colnames(df))
  }))]), ~ . + 273.15))
  
  # Rename the studied temperature and the output temperature as their standard names
  df <- df %>%
    rename(outdoorTemperature = airTemperature)
  
  lags_temperature <- 1
  if (lags_temperature>0) {
    df <- df %>% bind_cols(map_dfc(colnames(df)[Reduce(`|`, lapply(buildingSpaces, function(pat) {
      grepl(paste0("\\AmbTemp_",pat,"\\b"), colnames(df))
    }))], function(col) {
      map_dfc(1:lags_temperature, function(lag_n) {
        lag(df[[col]], lag_n)
      }) %>%
        setNames(paste0(col, "_l", 1:lags_temperature))
    }))
  }
  
  
  
  #If cooling mode and setpoint < temp -> IncTemp = Setpoint, else 0
  #If heating mode and setpoint > temp -> Setpoint = Setpoint, else 0
  # 
  # df <- df %>%
  #   mutate(
  #     !!!setNames(
  #       map(buildingSpaces, function(pat) {
  #         if_else(
  #           (.[[paste0("TempSP_", pat)]] > .[[paste0("TempPred_", pat)]]-sensor_sensitivity) & (.[["OperationMode"]]<3),
  #           (.[[paste0("\\AmbTemp_",pat,"\\b")]]- .[[paste0("AmbTemp_", pat,"_l1")]])*.[[paste0("PW_", pat)]],
  #           if_else((.[[paste0("TempSP_", pat)]] < .[[paste0("TempPred_", pat)]]+sensor_sensitivity) & (.[["OperationMode"]]>2),
  #                   (.[[paste0("\\AmbTemp_",pat,"\\b")]]- .[[paste0("AmbTemp_", pat,"_l1")]])*.[[paste0("PW_", pat)]], 0)
  #         )
  #       }),
  #       paste0("TempPred_",buildingSpaces)
  #     )
  #   )
  
  
  # buildingSpaces <- "1"
  # df <- df %>%
  #   mutate(
  #     !!!setNames(
  #       map(buildingSpaces, function(pat) {
  #         # Select columns that will be used
  #         AmbtempL <- paste0("AmbTemp_", pat,"_l1")
  #         TempPred <- paste0("TempPred_", pat)
  #         TempSP <- paste0("TempSP_", pat)
  #         PW <- paste0("PW_", pat)
  #         OperationMode <- "OperationMode"
  #         
  #         # Calulate increment
  #         if_else(
  #           (.[[TempPred]] > .[[AmbtempL]]-sensor_sensitivity) & .[[OperationMode]] < 3 ,
  #           (.[[TempPred]] - .[[AmbtempL]]) * .[[PW]], 
  #           if_else(
  #             (.[[TempPred]] < .[[AmbtempL]]+sensor_sensitivity) & .[[OperationMode]] > 2 , 
  #             (.[[AmbtempL]] - .[[TempPred]]) * .[[PW]], 
  #             0
  #           )
  #         )
  #       }),
  #       paste0("IncTemp_", buildingSpaces)  
  #     )
  #   )
  # df <- df %>%
  #   mutate(
  #     !!!setNames(
  #       map(buildingSpaces, function(pat) {
  #         # Select columns that will be used
  #         AmbtempL <- paste0("AmbTemp_", pat,"_l1")
  #         TempPred <- paste0("TempPred_", pat)
  #         TempSP <- paste0("TempSP_", pat)
  #         PW <- paste0("PW_", pat)
  #         OperationMode <- "OperationMode"
  #         
  #         # Calulate increment
  #         if_else(
  #           (.[[TempSP]] > .[[TempPred]]-sensor_sensitivity) & .[[OperationMode]] < 3 ,
  #           (.[[TempPred]] - .[[AmbtempL]]) * .[[PW]], 
  #           if_else(
  #             (.[[TempSP]] < .[[TempPred]]+sensor_sensitivity) & .[[OperationMode]] > 2 , 
  #             (.[[AmbtempL]] - .[[TempPred]]) * .[[PW]], 
  #             0
  #           )
  #         )
  #       }),
  #       paste0("IncTemp_", buildingSpaces)  
  #     )
  #   )
  if (modelMode != "dynamicsEstimator") {
    df <- df %>%
      mutate(
        !!!setNames(
          map(buildingSpaces, function(pat) {
            # Select columns that will be used
            AmbtempL <- paste0("AmbTemp_", pat,"_l1")
            TempPred <- paste0("TempPred_", pat)
            TempSP <- paste0("TempSP_", pat)
            PW <- paste0("PW_", pat)
            OperationMode_Ind <- paste0("OpMode_", pat)
            OperationMode <- "OperationMode"
            
            # Calulate increment
            if_else(
              .[[OperationMode]] < 3 & (.[[OperationMode_Ind]]==.[[OperationMode]]),
              (.[[TempPred]] - .[[AmbtempL]]) * .[[PW]],
              if_else(
                .[[OperationMode]] > 2  & (.[[OperationMode_Ind]]==.[[OperationMode]]),
                (.[[AmbtempL]] - .[[TempPred]]) * .[[PW]],
                0
              )
            )
          }),
          paste0("IncTemp_", buildingSpaces)
        )
      )
    
    #Replace NA for 0 for temperature increment columns
    df <- df %>%
      mutate(
        across(
          .cols = matches("IncTemp_"),
          .fns = ~replace_na(.x, 0)   
        )
      )
  }
  
  
  # # lags_energy <- lags_energy
  # if (lags_energy>0) {
  #   for (i in 1:lags_energy) {
  #     df <- df %>%
  #       mutate(!!paste0("E_Act_2_l", i) := lag(E_Act_2, i))
  #   }
  # }
  
  # # lags_outdoorTemperature <- lags_outdoorTemperature
  # if (lags_outdoorTemperature>0){
  #   for (i in 1:lags_outdoorTemperature) {
  #     df <- df %>%
  #       mutate(!!paste0("outdoorTemperature_l", i) := lag(outdoorTemperature, i))
  #   }
  # }
  
  #Delete NA rows appeared due to the maximmum lag of the temperatures
  # df <- df[(max(lags_energy)+1):length(df$time),]
  
  
  #Set localtime column
  # df$localtime <- df$time
  
  #Don't consider the first 25% of the data sice the temperatres models are not yet enough accurate
  df <- df[round(nrow(df)*0.25):nrow(df),]
  
  #Extract outlier of the energy timeseries
  df$E_Act_2[df$E_Act_2 < 0] <- NA
  df <- df[!is.na(df$E_Act_2), ]
  
  
  #Add an intercept term
  df <- df %>%
    mutate(intercept = 1)
  
  #Sum of the working HVAC machines during the timestep
  df <- df %>%
    mutate(nhvac = rowSums(select(., matches("^PW_")))) 
  
  
  # df <- df %>%
  #   mutate(
  #     !!!setNames(
  #       map(buildingSpaces, function(pat) {
  #         # Select columns that will be used
  #         PW <- paste0("PW_", pat)
  #         OperationMode_Ind <- paste0("OpMode_", pat)
  #         OperationMode <- "OperationMode"
  #         
  #         # Calulate increment
  #         if_else(
  #           .[[OperationMode_Ind]]==.[[OperationMode]],
  #           .[[PW]],
  #           0
  #         )
  #       }),
  #       paste0("isoperative_", buildingSpaces)
  #     )
  #   )
  # 
  # df <- df %>%
  #   mutate(nhvacop = rowSums(select(., matches("^isoperative_")))) 
  # 
  
  # Estimate the moving average
  # df$E_ma <- zoo::rollmean(df$E_Act_2, k = 2, fill = NA, align = "right")
  # df <- df %>%
  #   mutate(E_error_l1 = lag(E_Act_2-E_ma),1)
  # #Delete NA rows appeared due to the maximmum lag of the errors
  # df <- df[3:length(df$time),]
  
  # Detect holidays
  # write("* Detecting the holidays",stderr())
  # holidaysDates <- holidaysNAGER(
  #   y = unique(format(as.Date(df$time, format="%d/%m/%Y"),"%Y")), # "2023"
  #   country = "ES",
  #   region=get_std_regioncode_from_buildings("ES",
  #                                            "Barcelona")
  # )
  
  # df$time <- as.POSIXct(as.character(df$time), format = "%Y-%m-%d %H:%M:%S", tz = "Europe/Madrid")
  
  # # Add the calendar components
  # df <- df %>% calendar_components(
  #   localTimeZone = "Europe/Madrid",
  #   holidays = holidaysDates,
  #   inplace=T
  # )
  # 
  
  
  
  #Separate between the data when the HVAC is in heating system and when it is in cooling system
  df_heating <- df[df$OperationMode<3,]
  df_cooling <- df[df$OperationMode>2,]
  
  
  if (modelMode!="train" && file.exists(paste0(settings$OutputDataDirectory,"/models/",modelId,".rds"))) {
    
    #Upload the existent models and their training period information
    loadModel <- readRDS(paste0(settings$OutputDataDirectory,"/models/",modelId,".rds"))
    trained_models <- loadModel$trained_models
    modelInfo <- loadModel$modelInfo
    
    write(sprintf("The existent model has been uploaded, the initial training date of the model is: %s and the final training date is: %s.", 
                  modelInfo$FirstDate, modelInfo$Lastdate),stderr())
    
  } else {
    
    # Assess the occupancy scenario only when a minimum number of days with non-outlier data are available  
    # inside the min-max range of days after the EEM (all of them, defined in settings/EEMAssessmentConditions)
    if(sum(!is.na(df$temperature)) <= 
       hourly_timesteps(settings$EEMAssessmentConditions$MinDaysWithoutOutliers$PT1H*24, detect_time_step(df$time)) & !is.null(df$outliers)) {
      
      write(sprintf(
        "* %s wasn't assessed because there are not enough valid days to make the estimation", buildingspaceId) ,stderr())
      return(NULL)
      
    } else {
      
      ###
      # Setting the model parameters and transformations to be done during the model training ----
      ###
      
      write("* Setting model parameters and transformations",stderr())
      
      # Outdoor temperature (and its lags) transformations that will be added to the model
      # outdoor_temperature_transformations <- setNames(
      #   lapply(colnames(df)[Reduce(`|`, lapply("outdoorTemperature", function(pat) {
      #     grepl(paste0(pat), colnames(df))
      #   }))], function(c) paste0("lpf_ts(...,featuresNames='",c,"',smoothingTimeScaleParameter=param$alpha_ot",", inplace=F)")),
      #   paste0(colnames(df)[Reduce(`|`, lapply("outdoorTemperature", function(pat) {
      #     grepl(paste0(pat), colnames(df))
      #   }))], "_lpf")
      # )
      
      # Studied space temperature lags transformations that will be added to the model
      # energy_transformations <- setNames(
      #   lapply(colnames(df)[Reduce(`|`, lapply("E_Act_2_l", function(pat) {
      #     grepl(paste0(pat), colnames(df))
      #   }))], function(c) paste0("lpf_ts(...,featuresNames='",c,"',smoothingTimeScaleParameter=param$alpha_e",", inplace=F)")),
      #   paste0(colnames(df)[Reduce(`|`, lapply("E_Act_2_l", function(pat) {
      #     grepl(paste0(pat), colnames(df))
      #   }))], "_lpf")
      # )
      
      # outdoor_temperature_params <- list(
      #   "alpha_ot"=list(
      #     "datatype"="discrete",
      #     "levels"=c(0.6,0.8,0.9,0.95,0.975)
      #   ))
      # 
      # energy_params <- list(
      #   "alpha_e"=list(
      #     "datatype"="discrete",
      #     "levels"=c(0.6,0.8,0.9,0.95,0.975)
      #   ))
      
      
      
      generalParams <- list(
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
        "tempref_cooling"=list(
          "datatype"="discrete",
          "levels"=seq(from = (19+273.15), to = (27+273.15), by = 0.5)
        ),
        "tempref_heating"=list(
          "datatype"="discrete",
          "levels"=seq(from = (0+273.15), to = (19+273.15), by = 0.5)
        )
      )
      
      tempcalc <- function (outdoortemp, Reftemp) {
        ifelse(outdoorTemperature>tempref,outdoorTemperature-tempref,tempref-outdoorTemperature)
      }
      
      generalTransformationSentences <- list(
        "ComfTempHeating" = "ifelse(outdoorTemperature<param$tempref_heating  & nhvac>0, (param$tempref_heating-outdoorTemperature), 0)",
        "ComfTempCooling" = "ifelse(outdoorTemperature>param$tempref_cooling & nhvac>0, (outdoorTemperature-param$tempref_cooling), 0)"
      )
      #List all the parameters for the model
      params <- c(generalParams)
      #List all the transformation sententences for the model
      transformationSentences <- c(generalTransformationSentences)
      
      predictors <- c("intercept", "ComfTempHeating", "ComfTempCooling", "nhvac",
                      grep("IncTemp_", names(df), value = TRUE))
      
      # x <- df_result %>% dplyr::select(grep("IncTemp_", names(df), value = TRUE),
      #               grep("E_Act_2_l", names(df), value = TRUE), E_Act_2, predicted)
      
      
      # trControl <- trainControl(method="none")
      
      formula <- (paste("E_Act_2", "~", paste(predictors, collapse = " + ")))
      
      # formula <- paste("temperature", "~", "tlpf + outdoorTemperature + wcos1")
      
      
      trControl <- trainControl(method = "repeatedcv",
                                number=2,
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
            expected <- df$E_Act_2
            obtained <- biggr::predict.train(
              object = mod,
              newdata = df,
              forceGlobalInputFeatures = NULL,
              predictionHorizonInHours = 0,
              modelWindow = NULL,
              modelSelection = NULL
            )
            RMSE(expected[(length(expected)*0.25):length(expected)], obtained[(length(obtained)*0.25):length(obtained)], na.rm = T)
          },
          features = params,
          maxiter = 4,
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
                                                   modelWindow="%Y-%m-%d", modelSelection="rmse"){
        mod <- !!trained_models
        biggr::predict.train(
          object = mod[["heating"]],
          newdata = x, 
          forceGlobalInputFeatures = forceGlobalInputFeatures,
          predictionHorizonInHours = predictionHorizonInHours
        )
      })
      predictor_cooling <- carrier::crate(function(x, forceGlobalInputFeatures = NULL,predictionHorizonInHours=predictionHorizonInHours,
                                                   modelWindow="%Y-%m-%d", modelSelection="rmse"){
        mod <- !!trained_models
        biggr::predict.train(
          object = mod[["cooling"]],
          newdata = x, 
          forceGlobalInputFeatures = forceGlobalInputFeatures,
          predictionHorizonInHours = predictionHorizonInHours
        )
      })
      
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
        df_eval_aux_heating <- df_aux_heating[(nrow(df_aux_heating)-30*24):nrow(df_aux_heating),]
      } else {
        df_eval_aux_heating <- df_aux_heating
      }
      
      modelInfo$MAEmetric_heating <- MAE(df_eval_aux_heating$E_Act_2,
                                         df_eval_aux_heating$predicted,na.rm = T)
      modelInfo$RMSEmetric_heating <- RMSE(df_eval_aux_heating$E_Act_2,
                                           df_eval_aux_heating$predicted,na.rm = T)
      modelInfo$CVRMSEmetric_heating <- modelInfo$RMSEmetric_heating / mean(df_eval_aux_heating$E_Act_2,na.rm=T)
      modelInfo$r2metric_heating <- cor(df_eval_aux_heating$E_Act_2,
                                        df_eval_aux_heating$predicted,
                                        use = "na.or.complete")^2
      
      if (nrow(df_aux_cooling)>30*24) {
        df_eval_aux_cooling <- df_aux_cooling[(nrow(df_aux_cooling)-30*24):nrow(df_aux_cooling),]
      } else
      {
        df_eval_aux_cooling <- df_aux_cooling
      }
      
      modelInfo$MAEmetric_cooling <- MAE(df_eval_aux_cooling$E_Act_2,
                                         df_eval_aux_cooling$predicted,na.rm = T)
      modelInfo$RMSEmetric_cooling <- RMSE(df_eval_aux_cooling$E_Act_2,
                                           df_eval_aux_cooling$predicted,na.rm = T)
      modelInfo$CVRMSEmetric_cooling <- modelInfo$RMSEmetric_cooling / mean(df_eval_aux_cooling$E_Act_2,na.rm=T)
      modelInfo$r2metric_cooling <- cor(df_eval_aux_cooling$E_Act_2,
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
                                                 modelWindow="%Y-%m-%d", modelSelection="rmse", forceOneStepPrediction=F){
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
                                                 modelWindow="%Y-%m-%d", modelSelection="rmse", forceOneStepPrediction=F){
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
    
    df_result$predicted[df_result$predicted < 0] <- 0
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
      df_result_heating$predicted <- df_result_heating_vars %*% df_result_heating_coefs
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
    
    df_result$predicted[df_result$predicted < 0] <- 0
  }
  
  if(modelMode=="dynamicsEstimator"){
    
    fterms <- attr(terms(trained_models$heating$finalModel$meta$formula), "term.labels")
    df_result_vars <- data_transformation_wrapper(data=df,
                                                  features=trained_models$heating$finalModel$meta$features,
                                                  transformationSentences = trained_models$heating$finalModel$meta$transformationSentences,
                                                  param=trained_models$heating$finalModel$meta$param)$data[, fterms, drop = FALSE]
    df_result_vars <- cbind(df_result_vars,time=df$time)
    df_result_vars <- df_result_vars %>% select(matches("nhvac"),time)
    df_result_vars <- df_result_vars[order(df_result_vars$time), ]
    
    
    df_result_heating <- df_heating
    if (nrow(df_heating)>0) {
      df_result_heating_trans <- data_transformation_wrapper(data=df_result_heating,
                                                             features=trained_models$heating$finalModel$meta$features,
                                                             transformationSentences = trained_models$heating$finalModel$meta$transformationSentences,
                                                             param=trained_models$heating$finalModel$meta$param)
      fterms <- attr(terms(trained_models$heating$finalModel$meta$formula), "term.labels")
      
      df_result_heating_vars <- df_result_heating_trans$data[, fterms, drop = FALSE]
      df_result_heating_vars <- cbind(df_result_heating_vars,time=df_heating$time)
      df_result_heating_vars <- df_result_heating_vars %>% select(-matches(names(df_result_vars)),time)
      df_result_heating_vars <- df_result_heating_vars %>% select(-matches("IncTemp"))
    }  else {df_result_heating_vars<- data.frame()}
    df_result_heating_coefs <- trained_models$heating$finalModel$coefficients[nrow(trained_models$heating$finalModel$coefficients),]

    
    df_result_cooling <- df_cooling
    if (nrow(df_cooling)>0) {
      df_result_cooling_trans <- data_transformation_wrapper(data=df_result_cooling,
                                                             features=trained_models$cooling$finalModel$meta$features,
                                                             transformationSentences = trained_models$cooling$finalModel$meta$transformationSentences,
                                                             param=trained_models$cooling$finalModel$meta$param)
      fterms <- attr(terms(trained_models$cooling$finalModel$meta$formula), "term.labels")
      
      df_result_cooling_vars <- df_result_cooling_trans$data[, fterms, drop = FALSE]
      df_result_cooling_vars <- cbind(df_result_cooling_vars,time=df_cooling$time)
      df_result_cooling_vars <- df_result_cooling_vars %>% select(-matches(names(df_result_vars)),time)
      df_result_cooling_vars <- df_result_cooling_vars %>% select(-matches("IncTemp"))
    } else{df_result_cooling_vars<-  data.frame()}
    df_result_cooling_coefs <- trained_models$cooling$finalModel$coefficients[nrow(trained_models$cooling$finalModel$coefficients),]

    
    if (nrow(df_result_heating_vars)+nrow(df_result_cooling_vars)>0) {
      df_result_vars <- df_result_vars %>% full_join(rbind(df_result_heating_vars, df_result_cooling_vars), by="time")
      df_result_vars <- df_result_vars[order(df_result_vars$time), ]
      # #Compute the model residuals
      # df_result <- df_result %>% 
      #   mutate(residuals=temperature-predicted)
    }
    
    
    
    df_result <- list(heatingCoefs = df_result_heating_coefs, coolingCoefs = df_result_cooling_coefs, transformedData = df_result_vars)
  }
  
  
  
  plotsName <- paste0("HVAC","Energy","_")
  
  # Plot the predicted temperature and the real one for the evaluation period to evaluate the prediction model results
  if(plots){
    
    df_eval <- df_result[(nrow(df_result)-30*24):nrow(df_result),]
    
    pv <- ggplot(df_eval %>% pad(.,by = "time")) + 
      geom_line(aes(time,E_Act_2, col="Measured") ) + 
      geom_line(aes(time,predicted,col="Predicted"),alpha=0.5) +
      # ylim(c(0,max(df_eval$E_Act_2,na.rm=T)*1.1)) +
      scale_colour_manual(
        name = NULL,
        values = c("Predicted" = "red", "Measured" = "black")) +
      theme_bw() + xlab("time") + ylab("E_Act_2 (kWh)") +
      theme(legend.position = "bottom")
    
    
    ggsave(paste(paste(settings$OutputDataDirectory,"plots",sep="/"),paste0(plotsName,"validation.png"),sep="/"),
           pv, width=7, height=3, dpi = 1000)
  }
  
  # Plot the predicted temperature and the real one to evaluate the prediction model results for the complete period
  if(plots){
    
    pr <- ggplot(df_result %>% pad(.,by = "time")) + 
      geom_line(aes(time,E_Act_2, col="Measured") ) + 
      geom_line(aes(time,predicted,col="Predicted"),alpha=0.5) +
      ylim(c(0,max(df_result$E_Act_2,na.rm=T)*1.1)) +
      scale_colour_manual(
        name = NULL,
        values = c("Predicted" = "red", "Measured" = "black")) +
      theme_bw() + xlab("time") + ylab("E_Act_2 (Wh)") +
      theme(legend.position = "bottom")
    ggsave(paste(paste(settings$OutputDataDirectory,"plots",sep="/"),paste0(plotsName,"RealvsPredicted.png"),sep="/"),
           pr, width=7, height=3, dpi = 1000)
    prt <- ggplot(df_result %>% pad(.,by = "time")) + 
      geom_line(aes(time,E_Act_2, col="Measured") ) + 
      geom_line(aes(time,predicted,col="Predicted"),alpha=0.5) +
      geom_line(aes(time,((outdoorTemperature-273.13)*1000),col="Outdoor Temperature (ºC*1000)"),alpha=0.5) +
      # ylim(c(0,max(df_result$,na.rm=T)*1.1)) +
      scale_colour_manual(
        name = NULL,
        values = c("Predicted" = "red", "Measured" = "black", "Outdoor Temperature (ºC*1000)" = "blue")) +
      theme_bw() + xlab("time") + ylab("Energy Consumption (Wh)") +
      theme(legend.position = "bottom")
    ggsave(paste(paste(settings$OutputDataDirectory,"plots",sep="/"),paste0(plotsName,"RealvsPredictedvsTemp.png"),sep="/"),
           prt, width=7, height=3, dpi = 1000)
    
    ggplotly(ggplot(df_result %>% pad(.,by = "time")) +
               geom_line(aes(time,E_Act_2, col="Measured") ) +
               geom_line(aes(time,predicted,col="Predicted"),alpha=0.5) +
               geom_line(aes(time,((outdoorTemperature-273.13)*1000),col="Outdoor Temperature (ºC*1000)"),alpha=0.5) +
               # ylim(c(0,max(df_result$E_Act_2,na.rm=T)*1.1)) +
               scale_colour_manual(
                 name = NULL,
                 values = c("Predicted" = "red", "Measured" = "black", "Outdoor Temperature (ºC*1000)" = "blue")) +
               theme_bw() + xlab("time") + ylab("E_Act_2 (Wh)") +
               theme(legend.position = "bottom"))
  }
  
  
  #Show that estimation has end and return the results
  write("* Estimation finalised!",stderr())
  
  
  return(df_result)
  # return(list(df_result,metrics))
}

