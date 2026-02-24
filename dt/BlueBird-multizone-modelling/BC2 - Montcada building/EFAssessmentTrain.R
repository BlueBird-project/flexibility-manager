run_EF_assessment_train <- function(buildingSubject, df_import, settings, libraryPath=".", modelMode="train", plots=F){
  
  library(biggr)
  source(paste(libraryPath,"HourlyTemperatureModel.R", sep="/"))
  source(paste(libraryPath,"HourlyHVACEnergyModel.R", sep="/"))
  source(paste(libraryPath,"HourlyCalendarEnergyModel.R", sep="/"))
  
  # Load configuration paths and settings for the algorithm
  # settings <- jsonlite::fromJSON("Settings.json")
  # settings <- append(settings,jsonlite::fromJSON("config.json"))
  # 
  # dir.create(settings$OutputDataDirectory,showWarnings = F)
  
  
  # buildingId <- get_building_identifiers(buildingSubject)
  # namespaces <- get_building_namespaces_v2(buildingsRdf, buildingSubject)
  # buildingSubject <- "Montcada Casa de la Vila"
  tz <- "Europe/Madrid"
  
  write("",stderr())
  write("#######################################################",stderr())
  write("####### Flexibilty Measures Assessment Training #######",stderr())
  write("#######################################################",stderr())
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
  # current_file_path <- getSourceEditorContext()$path
  # path <- dirname(current_file_path)
  # files <- dir(, pattern = "*.json")
  # timeseriesObject <- files %>%
  #   map_df(~fromJSON(file.path(path, .), flatten = TRUE))
  # 
  # 
  
  
  
  # plots <- T
  
  if (!dir.exists(paste0(settings$OutputDataDirectory))){dir.create(settings$OutputDataDirectory,F)}
  if (!dir.exists(paste0(settings$OutputDataDirectory,"/models"))){dir.create(paste0(settings$OutputDataDirectory,"/models"),F)}
  if(plots){
    dir.create(paste(settings$OutputDataDirectory,"plots",sep="/"),F)
  }
  if (!dir.exists(paste0(settings$OutputDataDirectory,"/results"))){dir.create(paste0(settings$OutputDataDirectory,"/results"),F)}
  
  
  #Estimate wind speed and direction from the North and East Components
  df_import <- df_import %>% mutate(WindSpeed=sqrt(u^2 + v^2)) %>%
    mutate(WindDir=(180+(atan2(v, u)*(180/pi)))%%360)
  
  
  df_import <- df_import %>%
    rowwise() %>%
    mutate(OperationMode = {
      values <- c_across(starts_with("OpMode"))
      frecs <- table(values)
      as.numeric(names(frecs)[which.max(frecs)])
    }) %>%
    ungroup()
  
  
  
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
  
  # modelMode <- T
  
  # horisons_list <- {}
  # for (h in seq(0, 24, 4)) {
  #   predictionHorizonInHours <- 0
  # 
  # 
  # df_predict_test <-  df_import %>%
  #   filter(time >= "2025-07-10 23:00:00 CEST", time <= "2025-07-25 23:00:00 CEST")
  # 
  # 
  # df_predict_test <- df_predict_test %>%
  #   mutate(
  #     !!!setNames(
  #       map(buildingSpaces, function(pat) {
  #         # Select columns that will be used
  #         AmbtempL <- paste0("AmbTemp_", pat,"_l1")
  #         TempPred <- paste0("TempPred_", pat)
  #         TempSP <- paste0("TempSP_", pat)
  #         PW <- paste0("PW_", pat)
  #         OperationMode_Ind <- paste0("OpMode_", pat)
  #         OperationMode <- "OperationMode"
  # 
  #         # Calulate increment
  #         if_else(
  #           .[[PW]] < 1, 0, .[[PW]]
  #         )
  #       }),
  #       paste0("PW_", buildingSpaces)
  #     )
  #   )
  # 
  # 
  # df_predict_test <- df_predict_test %>%
  #   mutate(
  #     !!!setNames(
  #       map(buildingSpaces, function(pat) {
  #         # Select columns that will be used
  #         Ambtemp <- paste0("AmbTemp_", pat)
  #         TempPred <- paste0("TempPred_", pat)
  #         TempSP <- paste0("TempSP_", pat)
  #         PW <- paste0("PW_", pat)
  #         OperationMode_Ind <- paste0("OpMode_", pat)
  #         OperationMode <- "OperationMode"
  # 
  #         # Calulate increment
  #         if_else(
  #           .[[PW]] < 1, NA, .[[Ambtemp]]
  #         )
  #       }),
  #       paste0("AmbTemp_", buildingSpaces)
  #     )
  #   )
  # 
  # # library(zoo)
  # df_predict_test <- df_predict_test %>%
  #   mutate(
  #     across(matches("AmbTemp_"), ~ na.locf(.x, na.rm = FALSE))
  #   )
  # 
  # df_predict_test <- df_predict_test %>%
  #   mutate(
  #     !!!setNames(
  #       map(buildingSpaces, function(pat) {
  #         # Select columns that will be used
  #         AmbtempL <- paste0("AmbTemp_", pat,"_l1")
  #         TempPred <- paste0("TempPred_", pat)
  #         TempSP <- paste0("TempSP_", pat)
  #         PW <- paste0("PW_", pat)
  #         OperationMode_Ind <- paste0("OpMode_", pat)
  #         OperationMode <- "OperationMode"
  # 
  #         # Calulate increment
  #         if_else(
  #           .[[PW]] < 1, 1, .[[PW]]
  #         )
  #       }),
  #       paste0("PW_", buildingSpaces)
  #     )
  #   )
  # 
  # df_predict_test$AmbTemp_7 <- df_predict_test$AmbTemp_9
  # df_predict_test$AmbTemp_10 <- df_predict_test$AmbTemp_9
  # df_predict_test$AmbTemp_24 <- df_predict_test$AmbTemp_26
  # df_predict_test$AmbTemp_25 <- df_predict_test$AmbTemp_26
  # 
  # 
  # 
  # df_predict_test <- df_predict_test[98:nrow(df_predict_test), ]
  # # df_predict_test <- df_predict_test[complete.cases(df_predict_test), ]
  # 
  # df_predict_test$start <- df_predict_test$time
  # # df_predict_test$end <- df_predict_test$start + hours(1)
  # 
  # df_predict_test <- df_predict_test %>%
  #   mutate(end = start + hours(1))
  # 
  # # df_predict_test$end <- as.POSIXct(df_predict_test$end, tz="UTC")
  # 
  # df_predict_test$start <- sub("(\\+|\\-)(\\d{2})(\\d{2})", "\\1\\2:\\3",
  #                     format(lubridate::ymd_hms(df_predict_test$start, tz = "UTC"), "%Y-%m-%dT%H:%M:%S%z")
  # )
  # 
  # df_predict_test$end <- sub("(\\+|\\-)(\\d{2})(\\d{2})", "\\1\\2:\\3",
  #                            format(df_predict_test$end, "%Y-%m-%dT%H:%M:%S%z")
  # )
  # 
  # 
  # df_predict_test <- df_predict_test %>% select(-localtime, -WindSpeed,-WindDir,-atmosphericPressure,-cloudCoverHighLevels,-cloudCoverLowLevels,
  #                                               -cloudCoverMidLevels,-relativeHumidity,-totalPrecipitation, -OperationMode,-visibility,-latitude,-longitude, -time)
  # 
  # write_json(df_predict_test, paste0(settings$InputDataDirectory,"/df_predict.json"), pretty = TRUE, dataframe = "rows")
  
  lags_temperature <- 4
  lags_setpointTemperature <- 1
  lags_outdoorTemperature <- 5
  lags_collindant_temperature <- 4
  lags_collindant_setpoint_temperature <- 0
  
  if (modelMode=="train") {
    for (l in names(Rooms_list)) {
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
      # floor1 <- c("1","2","3","4","5","6")
      #Train room's temperature model
      TempmodelId <- paste0("Room",studied_space,"_TemperatureModel")
      df_import <- df_import %>%  full_join(HourlyTemperatureModel(df=df_import,
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
                                                                   modelMode="train",
                                                                   plots=plots) %>% select(predicted,time) %>% mutate(predicted=predicted-273.15) %>% rename(!!paste0("TempPred_",studied_space):=predicted))
    }
  #Train HVAC energy consumption model
  HVACEnergymodelId <- paste0("HVACEnergy","Model")
  
  df_import <- df_import %>%  full_join(HourlyHVACEnergyModel(df=data.frame(df_import, stringsAsFactors = FALSE), 
                                                              buildingSpaces=buildingSpaces,
                                                              lags_outdoorTemperature=lags_outdoorTemperature, 
                                                              predictionHorizonInHours= predictionHorizonInHours, 
                                                              modelId=HVACEnergymodelId,
                                                              modelMode="train",
                                                              plots=plots) %>% select(predicted,time) %>% rename(Qe_hvac=predicted))
  #Train non HVAC energy consumption model
  CalendarEnergymodelId <- paste0("CalendarEnergy","Model")
  df_import <- df_import %>% full_join(HourlyCalendarEnergyModel(df=data.frame(df_import %>% mutate(E_Act_1=E_Act_1-E_Act_2) %>% rename(Qe=E_Act_1), stringsAsFactors = FALSE), 
                                        modelId=CalendarEnergymodelId,
                                        tz=tz, 
                                        settings=settings,
                                        modelMode="train",
                                        plots=plots) %>% select(predicted,time) %>% rename(Qe_calendar=predicted))
  
  df_import <- df_import %>% mutate(Qe=Qe_hvac+Qe_calendar)
  
  df_result <- df_import %>%
    select(time, matches("AmbTemp_|TempSP_|OpMode_|PW_|TempPred_"), airTemperature, u, v, azimuth, elevation, GHI, Qe) %>% rename(EnergyPred = Qe)
  
  write_json(df_result, paste0(settings$OutputDataDirectory,"/trainResults/train_results.json"), pretty = TRUE, dataframe = "rows")
  
  }
  # 
  # 
  # 
  # #Import JSON
  # df_predict <- fromJSON(paste(settings$InputDataDirectory,"df_predict.json",sep="/"))
  # df_predict$time <- lubridate::ymd_hms(df_predict$time)
  # df_predict$localtime <- with_tz(df_predict$time, "Europe/Madrid")
  # df_predict <- df_predict[1:9,]
  # 
  # 
  # #Estimate wind speed and direction from the North and East Components
  # df_predict <- df_predict %>% mutate(WindSpeed=sqrt(u^2 + v^2)) %>%
  #   mutate(WindDir=(180+(atan2(v, u)*(180/pi)))%%360)
  # 
  # 
  # df_predict <- df_predict %>%
  #   rowwise() %>%
  #   mutate(OperationMode = {
  #     values <- c_across(starts_with("OpMode"))
  #     frecs <- table(values)
  #     as.numeric(names(frecs)[which.max(frecs)])
  #   }) %>%
  #   ungroup()
  # 
  # maxlag <- max(lags_temperature,
  #               lags_setpointTemperature,
  #               lags_outdoorTemperature,
  #               lags_collindant_temperature,
  #               lags_collindant_setpoint_temperature)
  # 
  # 
  # nhorisons <- nrow(df_predict)-maxlag
  # if (modelMode=="predict") {
  #   lags_temperature <- 4
  #   lags_setpointTemperature <- 1
  #   lags_outdoorTemperature <- 5
  #   lags_collindant_temperature <- 4
  #   lags_collindant_setpoint_temperature <- 0
  #   
  #   for (n in 1:nhorisons) {
  #     for (l in names(Rooms_list)) {
  #       # n=1
  #       # l="1"
  #       studied_space <- l
  #       collindant_spaces <- Rooms_list[[l]]
  #       # print(studied_space)
  #       write(
  #         c("* Modelling temperature of Room:",
  #           sprintf(" %s ",  studied_space)),
  #         stderr())
  #       if (!is.null(collindant_spaces)) {
  #         write(
  #           c("* Its adjoining spaces are:",
  #             sprintf(" %s ",  collindant_spaces)),
  #           stderr())
  #       } else{
  #         write(
  #           c("* The room has no adjoining spaces"),
  #           stderr())
  #       }
  #       
  #       # studied_space <- "4"
  #       # collindant_spaces <- c("2","3")
  #       TempmodelId <- paste0("Room",studied_space,"_TemperatureModel")
  #       df_predict_aux <- HourlyTemperatureModel(df=df_predict,studied_space=studied_space,
  #                                                                            collindant_spaces=collindant_spaces,
  #                                                                            lags_temperature=lags_temperature,
  #                                                                                       lags_setpointTemperature=lags_setpointTemperature,
  #                                                                                       lags_collindant_setpoint_temperature=lags_collindant_setpoint_temperature,
  #                                                                                       lags_outdoorTemperature=lags_outdoorTemperature,
  #                                                                                       lags_collindant_temperature=lags_collindant_temperature,
  #                                                                                       sensor_sensitivity=sensor_sensitivity,
  #                                                                                       predictionHorizonInHours = predictionHorizonInHours,
  #                                                                                       modelId=TempmodelId,
  #                                                                                       modelMode="predict",
  #                                                                                       plots=F) %>% select(predicted,time) %>% mutate(predicted=predicted-273.15) %>% rename(!!paste0("TempPred_",studied_space):=predicted)
  #       df_predict[(maxlag+n),paste0("TempPred_",studied_space)] <- df_predict_aux[(n),paste0("TempPred_",studied_space)]
  #           
  #       if ((maxlag+n)<nrow(df_predict)) {
  #         df_predict[(maxlag+n+1),paste0("AmbTemp_",studied_space)] <- df_predict[(maxlag+n),paste0("TempPred_",studied_space)]
  #       }
  #     }
  #   }
  #   #Train HVAC energy consumption model
  #   HVACEnergymodelId <- paste0("HVACEnergy","Model")
  #   
  #   df_predict <- df_predict %>%  full_join(HourlyHVACEnergyModel(df=data.frame(df_predict, stringsAsFactors = FALSE), 
  #                                                               buildingSpaces=buildingSpaces,
  #                                                               lags_outdoorTemperature=lags_outdoorTemperature, 
  #                                                               predictionHorizonInHours= predictionHorizonInHours, 
  #                                                               modelId=HVACEnergymodelId,
  #                                                               modelMode="predict",
  #                                                               plots=F) %>% select(predicted,time) %>% rename(Qe_hvac=predicted))
  #   #Train non HVAC energy consumption model
  #   CalendarEnergymodelId <- paste0("CalendarEnergy","Model")
  #   df_predict <- df_predict %>% full_join(HourlyCalendarEnergyModel(df=data.frame(df_predict %>% mutate(E_Act_1=E_Act_1-E_Act_2) %>% rename(Qe=E_Act_1), stringsAsFactors = FALSE), 
  #                                                                  modelId=CalendarEnergymodelId,
  #                                                                  tz=tz, 
  #                                                                  settings=settings,
  #                                                                  modelMode="predict",
  #                                                                  plots=F) %>% select(predicted,time) %>% rename(Qe_calendar=predicted))
  #   
  #   df_predict <- df_predict %>% mutate(Qe=Qe_hvac+Qe_calendar)
  #   
  #   df_result <- df_predict %>%
  #     select(time, Qe) %>% rename(EnergyPred = Qe)
  #   
  #   df_result <- df_result[(maxlag+1):nrow(df_result),]
  #   
  #   # Convert time to ISO 8601 format
  #   df_result$time <- sub("(\\+|\\-)(\\d{2})(\\d{2})", "\\1\\2:\\3",
  #                       format(lubridate::ymd_hms(df_result$time, tz = "UTC"), "%Y-%m-%dT%H:%M:%S%z")
  #   )
  #   # 
  #   
  #   write_json(df_result, paste0(settings$OutputDataDirectory,"/trainResults/predict_results.json"), pretty = TRUE, dataframe = "rows")
  #   
  #   
  # }
  # 
# 
# 
  # list_results <- {}
  # for (l in names(Rooms_list)) {
  #   studied_space <- l
  #   collindant_spaces <- Rooms_list[[l]]
  #   # print(studied_space)
  #   write(
  #     c("* Modelling temperature of Room:",
  #       sprintf(" %s ",  studied_space)),
  #     stderr())
  #   if (!is.null(collindant_spaces)) {
  #     write(
  #       c("* Its adjoining spaces are:",
  #         sprintf(" %s ",  collindant_spaces)),
  #       stderr())
  #   } else{
  #     write(
  #       c("* The room has no adjoining spaces"),
  #       stderr())
  #   }
  # 
  #   # print(collindant_spaces)
  # 
  # 
  #   # studied_space <- "4"
  #   # collindant_spaces <- c("2","3")
  #   # floor1 <- c("1","2","3","4","5","6")
  #   TempmodelId <- paste0("Room",studied_space,"_TemperatureModel")
  # 
  #   list_results[[paste0("Room_", l)]] <- HourlyTemperatureModel(df=df_import,
  #                                                                studied_space=studied_space,
  #                                                                collindant_spaces=collindant_spaces,
  #                                                                lags_temperature=lags_temperature,
  #                                                                lags_setpointTemperature=lags_setpointTemperature,
  #                                                                lags_collindant_setpoint_temperature=lags_collindant_setpoint_temperature,
  #                                                                lags_outdoorTemperature=lags_outdoorTemperature,
  #                                                                lags_collindant_temperature=lags_collindant_temperature,
  #                                                                sensor_sensitivity=sensor_sensitivity,
  #                                                                predictionHorizonInHours = predictionHorizonInHours,
  #                                                                modelId=TempmodelId,
  #                                                                modelMode="predict",
  #                                                                plots=F)
  # 
  # }


  # metric_results <- data.frame(
  #   # Room = names(list_results),
  #   MAE_heating = sapply(list_results, function(x) round(x[[2]]$MAEmetric_heating,4)),
  #   MAE_cooling = sapply(list_results, function(x) round(x[[2]]$MAEmetric_cooling,4)),
  #   RMSE_heating = sapply(list_results, function(x) round(x[[2]]$RMSEmetric_heating,4)),
  #   RMSE_cooling = sapply(list_results, function(x) round(x[[2]]$RMSEmetric_cooling,4)),
  #   CVRMSE_heating = sapply(list_results, function(x) round(x[[2]]$CVRMSEmetric_heating,4)),
  #   CVRMSE_cooling = sapply(list_results, function(x) round(x[[2]]$CVRMSEmetric_cooling,4)),
  #   R2_heating = sapply(list_results, function(x) round(x[[2]]$r2metric_heating,4)),
  #   R2_cooling = sapply(list_results, function(x) round(x[[2]]$r2metric_cooling,4))
  # )
  # 
  # plot_model_results <- F
  # # plot_model_results <- T
  # if (plot_model_results == T) {
  # 
  #   #Plot a table with all the metrics for the models results
  # 
  #   library(gridExtra)
  #   # Construct the metrics results table
  #   metrics_table <- tableGrob(metric_results)
  # 
  #   # Show table as graph
  #   grid.arrange(metrics_table)
  # 
  #   metrics_table$widths[1] <- unit(3, "cm")
  # 
  #   # Set the title of the table
  #   titulo <- textGrob("Results metrics for the thermal model", gp = gpar(fontsize = 16, fontface = "bold"))
  # 
  #   # Plot table and title
  #   p_var <- grid.arrange(titulo, metrics_table,
  #                         nrow = 2,
  #                         heights = c(0.1, 1))  # espacio del título vs tabla
  #   ggsave(paste(settings$OutputDataDirectory,"plots",sep="/","temperature_metrics_table.png"), p_var, width = 12, height = 11.5)
  # 
  #   #Boxplot for eahc one of the metrics
  #   png(paste(settings$OutputDataDirectory,"plots",sep="/","temperature_metrics_boxplot.png"), width = 800, height = 1200)
  # 
  #   par(mfrow = c(4, 2))  # 2x2 grid of plots
  # 
  #   for (col in names(metric_results)) {
  #     boxplot(metric_results[[col]], main = paste("Boxplot of", col), col = "lightblue")
  #   }
  # 
  #   par(mfrow = c(1, 1))  # reset layout
  # 
  #   dev.off()
  # 
  # }
  
  return(df_result)

}




