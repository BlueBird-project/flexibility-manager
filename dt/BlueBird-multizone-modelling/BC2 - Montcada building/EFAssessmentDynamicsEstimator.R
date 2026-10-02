run_EF_assessment_dynamics_estimator <- function(buildingSubject, df_predict, settings, libraryPath=".", modelMode="predict", plots=F){
  
  library(biggr)
  source(paste(libraryPath,"HourlyTemperatureModel.R", sep="/"))
  source(paste(libraryPath,"HourlyHVACEnergyModel.R", sep="/"))
  source(paste(libraryPath,"HourlyCalendarEnergyModel.R", sep="/"))
  
  # Load configuration paths and settings for the algorithm
  # settings <- jsonlite::fromJSON("Settings.json")
  # settings <- append(settings,jsonlite::fromJSON("config.json"))
  
  # dir.create(settings$OutputDataDirectory,showWarnings = F)
  
  
  # buildingId <- get_building_identifiers(buildingSubject)
  # namespaces <- get_building_namespaces_v2(buildingsRdf, buildingSubject)
  buildingSubject <- "Montcada Casa de la Vila"
  tz <- "Europe/Madrid"
  
  write("",stderr())
  write("#################################################################",stderr())
  write("####### Flexibilty Measures Assessment Dynamics Estimator #######",stderr())
  write("#################################################################",stderr())
  write(sprintf("   Building: %s", buildingSubject),stderr())
  write("",stderr())
  
  
  
  # results <- list()

  
  library(dplyr)
  library(rstudioapi)
  library(gridExtra)
  library(grid)
  library(ggplotify)
  library(readr)
  library(carrier)
  library(ggplot2)
  library(plotly)
  
  
  # plots <- T
  
  if (!dir.exists(paste0(settings$OutputDataDirectory))){dir.create(settings$OutputDataDirectory,F)}
  if (!dir.exists(paste0(settings$OutputDataDirectory,"/models"))){dir.create(paste0(settings$OutputDataDirectory,"/models"),F)}
  if(plots){
    dir.create(paste(settings$OutputDataDirectory,"plots",sep="/"),F)
  }
  if (!dir.exists(paste0(settings$OutputDataDirectory,"/results"))){dir.create(paste0(settings$OutputDataDirectory,"/results"),F)}
  
  
  
  
  # Rooms collindances definition
  Room1 <- c()
  Room2 <- c("4","6")
  Room3 <- c("4","5")
  Room4 <- c("2","3")
  Room5 <- c("3")
  Room6 <- c("2")
  Room7 <- c("9")
  Room8 <- c("11")
  Room9 <- c("7","10")
  Room10 <- c("9")
  Room11 <- c("8")
  Room12 <- c("14")
  Room13 <- c()
  Room14 <- c("12")
  Room15 <- c("23")
  Room16 <- c("17")
  Room17 <- c("16")
  Room18 <- c("16")
  Room19 <- c("17","18")
  Room20 <- c("21")
  Room21 <- c("20","23")
  Room23 <- c("21","15")
  Room24 <- c()
  Room25 <- c("26","27")
  Room26 <- c("25","27")
  Room27 <- c("25","26")
  Room28 <- c("29","30")
  Room29 <- c("28","30")
  Room30 <- c("28","29")
  Room76 <- c("77","78","79","80","97","98")
  Room77 <- c("76","98")
  Room78 <- c("76","97")
  Room79 <- c("76","80")
  Room80 <- c("76","79")
  Room97 <- c("76","78","98")
  Room98 <- c("76","77","97")
  
  #List of each room and its collindant spaces
  Rooms_list <- list("1" = Room1,
                     "2" = Room2,
                     "3" = Room3,
                     "4" = Room4,
                     "5" = Room5,
                     "6" = Room6,
                     "7" = Room7,
                     "8" = Room8,
                     "9" = Room9,
                     "10" = Room10,
                     "11" = Room11,
                     "12" = Room12,
                     "13" = Room13,
                     "14" = Room14,
                     "15" = Room15,
                     "16" = Room16,
                     "17" = Room17,
                     "18" = Room18,
                     "19" = Room19,
                     "20" = Room20,
                     "21" = Room21,
                     "23" = Room23,
                     "24" = Room24,
                     "25" = Room25,
                     "26" = Room26,
                     "27" = Room27,
                     "28" = Room28,
                     "29" = Room29,
                     "30" = Room30,
                     "76" = Room76,
                     "77" = Room77,
                     "78" = Room78,
                     "79" = Room79,
                     "80" = Room80,
                     "97" = Room97,
                     "98" = Room98)
  
  
  
  
  sensor_sensitivity <- 0.5
  predictionHorizonInHours <- 0
  buildingSpaces <- names(Rooms_list)
 
  lags_temperature <- 4
  lags_setpointTemperature <- 1
  lags_outdoorTemperature <- 5
  lags_collindant_temperature <- 4
  lags_collindant_setpoint_temperature <- 0
  
  maxlag <- max(lags_temperature,
                lags_setpointTemperature,
                lags_outdoorTemperature,
                lags_collindant_temperature,
                lags_collindant_setpoint_temperature)
  
  
  
  #Estimate wind speed and direction from the North and East Components
  df_predict <- df_predict %>% mutate(WindSpeed=sqrt(u^2 + v^2)) %>%
    mutate(WindDir=(180+(atan2(v, u)*(180/pi)))%%360)
  
  
  df_predict <- df_predict %>%
    rowwise() %>%
    mutate(OperationMode = {
      values <- c_across(starts_with("OpMode"))
      frecs <- table(values)
      as.numeric(names(frecs)[which.max(frecs)])
    }) %>%
    ungroup()
  
  list_results <- {}
  df_predict_aux <- data.frame(time=df_predict$time)
  CoefsTempHeating <- NULL   # final heating coefs matrix
  CoefsTempCooling <- NULL   # final cooling coefs matrix
  
  coefsupdate <- function(total, coef, iter_name) {
    
    # First iteration
    if (is.null(total)) {
      total <- matrix(coef, nrow = 1)
      colnames(total) <- names(coef)
      rownames(total) <- iter_name
      return(total)
    }
    
    # Detect new coeffs
    new <- setdiff(names(coef), colnames(total))
    
    # Aggregate new columns if necessary
    if (length(new) > 0) {
      total <- cbind(total,
                     matrix(NA, nrow(total), length(new),
                            dimnames = list(NULL, new)))
    }
    
    # Build rows aligned by names
    row <- rep(NA, ncol(total))
    names(row) <- colnames(total)
    row[names(coef)] <- coef
    
    # Add row
    total <- rbind(total, row)
    rownames(total)[nrow(total)] <- iter_name
    
    return(total)
  }
  

  if (modelMode=="dynamicsEstimator") {
    # for (n in 1:nhorisons) {
    # Obtain transformations and dynamics matrix of temperatures models
    for (l in names(Rooms_list)) {
      # n=1
      # l="1"
      studied_space <- l
      collindant_spaces <- Rooms_list[[l]]
      # print(studied_space)
      write(
        c("* Modelling temperature of Room:",
          sprintf(" %s ",  studied_space)),
        stderr())
      if (!is.null(collindant_spaces)) {
        write(
          c("* Its adjoining spaces are:",
            sprintf(" %s ",  collindant_spaces)),
          stderr())
      } else{
        write(
          c("* The room has no adjoining spaces"),
          stderr())
      }
      
      # studied_space <- "4"
      # collindant_spaces <- c("2","3")
      TempmodelId <- paste0("Room",studied_space,"_TemperatureModel")
      list_aux <- HourlyTemperatureModel(df=df_predict,
                                         studied_space=studied_space,
                                         collindant_spaces=collindant_spaces,
                                         lags_temperature=lags_temperature,
                                         lags_setpointTemperature=lags_setpointTemperature,
                                         lags_collindant_setpoint_temperature=lags_collindant_setpoint_temperature,
                                         lags_outdoorTemperature=lags_outdoorTemperature,
                                         lags_collindant_temperature=lags_collindant_temperature,
                                         sensor_sensitivity=sensor_sensitivity,
                                         predictionHorizonInHours = predictionHorizonInHours,
                                         modelId=TempmodelId,
                                         modelMode="dynamicsEstimator",
                                         plots=F)
     
      df_predict_aux <- df_predict_aux %>% full_join(list_aux$transformedData[, !names(list_aux$transformedData ) %in% setdiff(names(df_predict_aux), "time")],"time")
      CoefsTempHeating <- coefsupdate(CoefsTempHeating, list_aux$heatingCoefs , paste0("AmbTemp_", studied_space))
      CoefsTempCooling <- coefsupdate(CoefsTempCooling, list_aux$coolingCoefs, paste0("AmbTemp_", studied_space))
      
    }
    CoefsTempHeating[is.na(CoefsTempHeating)] <- 0
    CoefsTempHeating <- as.matrix(CoefsTempHeating)
    CoefsTempCooling[is.na(CoefsTempCooling)] <- 0
    CoefsTempCooling <- as.matrix(CoefsTempCooling)
    CoefsTempHeatingColNames <- colnames(CoefsTempHeating)
    CoefsTempHeatingRowNames <- rownames(CoefsTempHeating)
    CoefsTempCoolingColNames <- colnames(CoefsTempCooling)
    CoefsTempCoolingRowNames <- rownames(CoefsTempCooling)
    # Obtain start and end variables for the transformations dataframe and convert them in ISO 8601 format
    TransformedInputsTemperature <- df_predict_aux
    TransformedInputsTemperature$start <- TransformedInputsTemperature$time

    TransformedInputsTemperature <- TransformedInputsTemperature %>%
      mutate(end = start + hours(1))

    TransformedInputsTemperature$start <- sub("(\\+|\\-)(\\d{2})(\\d{2})", "\\1\\2:\\3",
                        format(lubridate::ymd_hms(TransformedInputsTemperature$start, tz = "UTC"), "%Y-%m-%dT%H:%M:%S%z")
    )
    TransformedInputsTemperature$end <- sub("(\\+|\\-)(\\d{2})(\\d{2})", "\\1\\2:\\3",
                               format(TransformedInputsTemperature$end, "%Y-%m-%dT%H:%M:%S%z")
    )
    TransformedInputsTemperature <- TransformedInputsTemperature[, !names(TransformedInputsTemperature) %in% "time"]
    


    #Obtain transformations and dynamics matrix of HVAC energy consumption model
    HVACEnergymodelId <- paste0("HVACEnergy","Model")
    
    list_predict_aux <- HourlyHVACEnergyModel(df=data.frame(df_predict, stringsAsFactors = FALSE),
                                            buildingSpaces=buildingSpaces,
                                            lags_outdoorTemperature=lags_outdoorTemperature, 
                                            predictionHorizonInHours= predictionHorizonInHours,
                                            modelId=HVACEnergymodelId,
                                            modelMode="dynamicsEstimator",
                                            plots=F)
    
    CoefsHVACEnergyHeating <- as.matrix(list_predict_aux$heatingCoefs)
    CoefsHVACEnergyCooling <- as.matrix(list_predict_aux$coolingCoefs)
    rownames(CoefsHVACEnergyHeating) <- "HVACEnergy"
    rownames(CoefsHVACEnergyCooling) <- "HVACEnergy"
    CoefsHVACEnergyHeatingColNames <- colnames(CoefsHVACEnergyHeating)
    CoefsHVACEnergyHeatingRowNames <- rownames(CoefsHVACEnergyHeating)
    CoefsHVACEnergyCoolingColNames <- colnames(CoefsHVACEnergyCooling)
    CoefsHVACEnergyCoolingRowNames <- rownames(CoefsHVACEnergyCooling)
    
    CoefsHVACEnergyHeating[is.na(CoefsHVACEnergyHeating)] <- 0
    CoefsHVACEnergyCooling[is.na(CoefsHVACEnergyCooling)] <- 0
    
    # Obtain start and end variables for the transformations dataframe and convert them in ISO 8601 format
    TransformedInputsHVACEnergy <- list_predict_aux$transformedData
    TransformedInputsHVACEnergy$start <- TransformedInputsHVACEnergy$time
    TransformedInputsHVACEnergy <- TransformedInputsHVACEnergy %>%
      mutate(end = start + hours(1))
    TransformedInputsHVACEnergy$start <- sub("(\\+|\\-)(\\d{2})(\\d{2})", "\\1\\2:\\3",
                                              format(lubridate::ymd_hms(TransformedInputsHVACEnergy$start, tz = "UTC"), "%Y-%m-%dT%H:%M:%S%z")
    )
    TransformedInputsHVACEnergy$end <- sub("(\\+|\\-)(\\d{2})(\\d{2})", "\\1\\2:\\3",
                                            format(TransformedInputsHVACEnergy$end, "%Y-%m-%dT%H:%M:%S%z")
    )
    TransformedInputsHVACEnergy <- TransformedInputsHVACEnergy[, !names(TransformedInputsHVACEnergy) %in% "time"]
    
    #Train non HVAC energy consumption model
    CalendarEnergymodelId <- paste0("CalendarEnergy","Model")
    NonHVACEnergyPredicted <- HourlyCalendarEnergyModel(df=data.frame(df_predict %>% mutate(E_Act_1=E_Act_1-E_Act_2) %>% rename(Qe=E_Act_1), stringsAsFactors = FALSE), 
                                                                   modelId=CalendarEnergymodelId,
                                                                   tz=tz, 
                                                                   settings=settings,
                                                                   modelMode="predict",
                                                                   plots=F) %>% select(predicted,time) %>% rename(NonHVACEnergy=predicted)
    NonHVACEnergyPredicted <- NonHVACEnergyPredicted[(maxlag+1):nrow(NonHVACEnergyPredicted),]
    # Obtain start and end variables for the transformations dataframe and convert them in ISO 8601 format
    NonHVACEnergyPredicted$start <- NonHVACEnergyPredicted$time
    NonHVACEnergyPredicted <- NonHVACEnergyPredicted %>%
      mutate(end = start + hours(1))
    NonHVACEnergyPredicted$start <- sub("(\\+|\\-)(\\d{2})(\\d{2})", "\\1\\2:\\3",
                                             format(lubridate::ymd_hms(NonHVACEnergyPredicted$start, tz = "UTC"), "%Y-%m-%dT%H:%M:%S%z")
    )
    NonHVACEnergyPredicted$end <- sub("(\\+|\\-)(\\d{2})(\\d{2})", "\\1\\2:\\3",
                                           format(NonHVACEnergyPredicted$end, "%Y-%m-%dT%H:%M:%S%z")
    )
    NonHVACEnergyPredicted <- NonHVACEnergyPredicted[, !names(NonHVACEnergyPredicted) %in% "time"]
    
    
    #Include HVAC status in results
    HVACStatus <- df_predict %>% select(time, colnames(df_predict)[grepl("PW", colnames(df_predict))], colnames(df_predict)[grepl("OpMode", colnames(df_predict))])
    HVACStatus$start <- HVACStatus$time
    
    HVACStatus <- HVACStatus %>%
      mutate(end = start + hours(1))
    
    HVACStatus$start <- sub("(\\+|\\-)(\\d{2})(\\d{2})", "\\1\\2:\\3",
                                              format(lubridate::ymd_hms(HVACStatus$start, tz = "UTC"), "%Y-%m-%dT%H:%M:%S%z")
    )
    HVACStatus$end <- sub("(\\+|\\-)(\\d{2})(\\d{2})", "\\1\\2:\\3",
                                            format(HVACStatus$end, "%Y-%m-%dT%H:%M:%S%z")
    )
    HVACStatus <- HVACStatus[, !names(HVACStatus) %in% "time"]

    
    results <- list(
      TransformedInputsTemperature = TransformedInputsTemperature,
      CoefsTempHeating=CoefsTempHeating,
      CoefsTempHeatingColNames=CoefsTempHeatingColNames,
      CoefsTempHeatingRowNames=CoefsTempHeatingRowNames,
      CoefsTempCooling=CoefsTempCooling,
      CoefsTempCoolingColNames=CoefsTempCoolingColNames,
      CoefsTempCoolingRowNames=CoefsTempCoolingRowNames,
      TransformedInputsHVACEnergy=TransformedInputsHVACEnergy,
      CoefsHVACEnergyHeating=CoefsHVACEnergyHeating,
      CoefsHVACEnergyHeatingColNames=CoefsHVACEnergyHeatingColNames,
      CoefsHVACEnergyHeatingRowNames=CoefsHVACEnergyHeatingRowNames,
      CoefsHVACEnergyCooling=CoefsHVACEnergyCooling,
      CoefsHVACEnergyCoolingColNames=CoefsHVACEnergyCoolingColNames,
      CoefsHVACEnergyCoolingRowNames=CoefsHVACEnergyCoolingRowNames,
      NonHVACEnergyPredicted = NonHVACEnergyPredicted,
      HVACStatus=HVACStatus
    )
    
  } else {results <- NULL}
  
 
  return(results)

}




