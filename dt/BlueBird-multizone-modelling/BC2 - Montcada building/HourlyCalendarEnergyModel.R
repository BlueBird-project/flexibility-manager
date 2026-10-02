HourlyCalendarEnergyModel <- function(df, settings, tz, modelId, modelMode="train", plots=T){
  
  suppressMessages(suppressWarnings(library(ggplot2)))
  suppressMessages(suppressWarnings(library(htmlwidgets)))
  suppressMessages(suppressWarnings(library(plotly)))
  suppressMessages(suppressWarnings(library(carrier)))
  suppressMessages(suppressWarnings(library(digest)))
  suppressMessages(suppressWarnings(library(httr)))
  
  modelName <- "HourlyCalendarEnergyModel"
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
  
  ####
  # Calendar features and filtering of holidays and special periods ----
  ####
  
  # data = df
  # consumptionColumn = "Qe"
  # timeColumn = "time"
  # tz=tz
  # ignoreDates = NULL
  # plotDensity = F
  
  
  
  holidaysNAGER_v2 <- function (y, country, region = NULL) 
  {
    df <- sapply(y, get_holidays_NAGER_v2, country, region)
    dates <- as.Date(c("2000-03-02"))
    for (i in 1:length(y)) {
      for (ii in 1:length(df["date", i][[1]])) {
        d <- as.Date(df[["date", i]][[ii]])
        dates <- append(dates, d)
      }
    }
    return(dates[-1])
  }
  
  
  
  get_holidays_NAGER_v2 <- function (year, country, region) 
  {
    res = GET("https://date.nager.at", path = list("api/v3/publicholidays", 
                                                   year, country))
    data = fromJSON(rawToChar(res$content))
    if (length(region) != 0) {
      data_region <- data[sapply(as.character(data$counties), function(x) {
        names <- strsplit(x, ",\\s*")[[1]]
        any(list(paste(country, 
                       region, sep = "-"), NULL) %in% names)
      }),
      ]
      for (el in 1:nrow(data)) if (typeof(data$counties[el]) == 
                                   "list" && paste(year, region, sep = "-") %in% data$counties[[el]]) 
        data_region = rbind(data_region, data[el, ])
    }
    else {
      data_region <- data
    }
    return(data_region)
  }
  
  
  # Detect holidays
  write("* Detecting the holidays",stderr())
  holidaysDates <- holidaysNAGER_v2(
    y = unique(format(as.Date(df$time, format="%d/%m/%Y"),"%Y")), # "2023"
    country = "ES",
    region=get_std_regioncode_from_buildings("ES",
                                             "Barcelona")
  )
  
  # Add the calendar components
  df <- df %>% calendar_components(
    localTimeZone = tz,
    holidays = holidaysDates,
    inplace=T
  )
  
  if(plots){
    h <- ggplot(
        df %>% select(time, Qe, isHolidays) %>% 
          group_by(date=as.Date(time,tz=tz)) %>% 
          summarise(Qe=sum(Qe),isHolidays=any(as.logical(isHolidays)))
      ) + 
      geom_line(aes(date,Qe),size=0.1,alpha=0.5) +
      geom_point(aes(date,Qe,col=isHolidays), size=0.1) +
      scale_color_manual("",values=c("TRUE"="red","FALSE"="black"),labels=c("TRUE"="Holiday","FALSE"="Not holiday")) +
      theme_bw() + theme(axis.text.x = element_text(hjust=1), legend.position = "top") +
      ylab("consumption (kWh)") + xlab("time")
    ggsave(paste(paste(settings$OutputDataDirectory,"plots",sep="/"),"holidays.pdf",sep="/"),
           h, width=7, height=3)
  }
  
  ####
  # Outliers detection ----
  ####
  
  write("* Detecting the outliers",stderr())
  if(all(c("value","window") %in% colnames(df)))
    df <- df %>% select(-value, -window)
  df <- 
    df %>%
    select(!(contains("outliers") | contains("upperPredCalendarModel") | 
               contains("lowerPredCalendarModel"))) %>%
    left_join(
      detect_ts_calendar_model_outliers(
        data = .,
        localTimeColumn = "localtime",
        valueColumn = "Qe", 
        calendarFeatures = settings$DataCleaning$OutliersDetection$calendarFeatures$PT1H,
        mode = settings$DataCleaning$OutliersDetection$mode,
        upperModelPercentile = settings$DataCleaning$OutliersDetection$upperModelPercentile$PT1H,
        lowerModelPercentile = settings$DataCleaning$OutliersDetection$lowerModelPercentile$PT1H,
        upperPercentualThreshold = settings$DataCleaning$OutliersDetection$upperPercentualThreshold$PT1H,
        lowerPercentualThreshold = settings$DataCleaning$OutliersDetection$lowerPercentualThreshold$PT1H,
        window = settings$DataCleaning$OutliersDetection$window$PT1H,
        outputPredictors = T,
        holidaysCalendar = holidaysDates,
        daysThatAreOutliers = NULL,
        logValueColumn = F,
        autoDetectProfiled = T),
      by = "localtime"
    )
  abnormalDays <- sort(df %>% 
                         group_by(date) %>% summarise(out=sum(outliers)) %>%
                         filter(out>0) %>% select(date) %>% unlist(.) %>% as.Date(.))
  df$abnormalDay <- df$date %in% abnormalDays
  
  if(plots){
    g <- ggplot(df %>% select(localtime, Qe, outliers) %>% 
                  group_by(date=as.Date(localtime,tz=tz)) %>% 
                  summarise(Qe=sum(Qe),outliers=any(as.logical(outliers))) ) +
      geom_line(aes(date,Qe),size=0.1,alpha=0.5) +
    theme_bw() + theme(axis.text.x = element_text(hjust=1), legend.position = "top") +
      ylab("consumption (kWh)") + xlab("time")
    if(!all(is.na(df$outliers) | df$outliers==F)){
      g <- g + geom_point(aes(date,Qe,col=outliers),size=0.1) + 
        scale_color_manual("",values=c("TRUE"="red","FALSE"="black"),labels=c("TRUE"="Day with outliers","FALSE"="Normal day"))
    }
    ggsave(paste(paste(settings$OutputDataDirectory,"plots",sep="/"),"outliers_plot.pdf",sep="/"),
           g, width=7, height=3)
  }
  
  #Sum of the working HVAC machines during the timestep
  df <- df %>%
    mutate(nhvac = rowSums(select(., matches("^PW_")))) 
  
  # Rename the studied temperature and the output temperature as their standard names
  df <- df %>%
    rename(outdoorTemperature = airTemperature)
  
  if (modelMode=="predict" && file.exists(paste0(settings$OutputDataDirectory,"/models/",modelId,".rds"))) {
    
    #Upload the existent models and their training period information
    loadModel <- readRDS(paste0(settings$OutputDataDirectory,"/models/",modelId,".rds"))
    trained_models <- loadModel$trained_models
    modelInfo <- loadModel$modelInfo
    
    write(sprintf("The existent model has been uploaded, the initial training date of the model is: %s and the final training date is: %s.", 
                  modelInfo$FirstDate, modelInfo$Lastdate),stderr())
    
  } else {
    
    # Assess the occupancy scenario only when a minimum number of days with non-outlier data are available  
    # inside the min-max range of days after the EEM (all of them, defined in settings/EEMAssessmentConditions)
    if(sum(!is.na(df$Qe)) <= 
       hourly_timesteps(settings$EEMAssessmentConditions$MinDaysWithoutOutliers$PT1H*24, detect_time_step(df$time)) & !is.null(df$outliers)) {
      
      write(sprintf(
        "* %s wasn't assessed because there are not enough valid days to make the estimation") ,stderr())
      return(NULL)
      
    } else if (modelMode=="train") {
      
      ###
      # Setting the model parameters and transformations to be done during the model training ----
      ###
      
      write("* Setting model parameters and transformations",stderr())
      
      generalTransformationSentences <- list(
        # Cast to numeric of the holidays feature
        "holidays" = "ifelse(isHolidays==T,2,1)",
        
        # Fill some gaps in the outdoor temperature time series.
        "outdoorTemperature" = "na.locf(
                              na.locf(
                                na.approx(outdoorTemperature,na.rm = F),
                                fromLast = T, na.rm = F
                              ),
                              na.rm=F)"
      )
      if(length(unique(df$isHolidays))==1){
        generalTransformationSentences <- generalTransformationSentences
      }
      
      trControl <- trainControl(method="none")
      
      
      ###
      # Model training ----
      ###
      
      write("* Training of the model",stderr())
      
      
      model <- function(df, generalTransformationSentences){
        HC_model <- function(df, ...) {
          args <- list(...)
          if(length(unique(df$isHolidays))==1){
            train(
              Qe ~ hour + dayYear + weekdayNum + nhvac,
              data=df,
              method = RandomForest(NULL),
              trControl = trControl,
              maxPredictionValue = max(df$Qe,na.rm=T) * 1.1,
              minPredictionValue = 0,
              transformationSentences = args$transformationSentences)
            } else {
              train(
                Qe ~ hour + dayYear + weekdayNum + nhvac + holidays,
                data=df,
                method = RandomForest(NULL),
                trControl = trControl,
                maxPredictionValue = max(df$Qe,na.rm=T) * 1.1,
                minPredictionValue = 0,
                transformationSentences = args$transformationSentences)
            }
        }
        mod <- HC_model(df = df,
                        transformationSentences = generalTransformationSentences)
        return(mod)
      }
      
      df_train <- df[1:(nrow(df)-(31*24)),]
      
      trained_models <- list(
        "baseline" = model(
          df = df_train[df_train$outliers==F & !is.na(df_train$outliers),],
          generalTransformationSentences = generalTransformationSentences)
      )

      # Generate the predictor objects
      predictor <- carrier::crate(function(x, baseline, forceGlobalInputFeatures = NULL, predictionIntervals = F){
        mod <- !!trained_models
        biggr::predict.train(
          object = mod[["baseline"]],
          newdata = x, 
          forceGlobalInputFeatures = forceGlobalInputFeatures,
          predictionIntervals = predictionIntervals
        )
      })
      
      #Store information about the model
      modelInfo <- {}
      
      #Training period
      modelInfo$FirstDate = parsedate::format_iso_8601(min(df$time))
      modelInfo$Lastdate = parsedate::format_iso_8601(max(df$time))
      
      # Store the errors
      df_aux <- df
      
      df_aux$predicted <- predictor(df_aux)
      
      # df_eval_aux <- df_aux[(nrow(df_aux)-30*24):nrow(df_aux),]
      
      df_eval_aux <- df_aux[(nrow(df_aux)-30*24):nrow(df_aux),]
      
      modelInfo$MAEmetric <- MAE(df_eval_aux$Qe,
                                         df_eval_aux$predicted,na.rm = T)
      modelInfo$RMSEmetric <- RMSE(df_eval_aux$Qe,
                                           df_eval_aux$predicted,na.rm = T)
      modelInfo$CVRMSEmetric <- modelInfo$RMSEmetric / mean(df_eval_aux$Qe,na.rm=T)
      modelInfo$r2metric <- cor(df_eval_aux$Qe,
                                        df_eval_aux$predicted,
                                        use = "na.or.complete")^2
      
      
      # Store the final model parameters obtained from hyperparameter tunning
      modelInfo$HeatingParameters <- list()
      for (i in 1:ncol(trained_models[["baseline"]]$bestTune)){
        modelInfo$HeatingParameters[[colnames(trained_models[["baseline"]]$bestTune)[i]]] <-
          as.character(trained_models[["baseline"]]$bestTune[1,i])
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
  
  # Generate the predictor objects
  predictor <- carrier::crate(function(x, baseline, forceGlobalInputFeatures = NULL, predictionIntervals = F){
    mod <- !!trained_models
    biggr::predict.train(
      object = mod[["baseline"]],
      newdata = x, 
      forceGlobalInputFeatures = forceGlobalInputFeatures,
      predictionIntervals = predictionIntervals
    )
  })
  
  df_result <- df
  df_result$predicted <- predictor(df_result)
  
  
  
  if(plots){
    plotsName <- paste0("Calendar","Energy","_")
    
    df_eval <- df_result[(nrow(df_result)-30*24):nrow(df_result),]
    
    pv <- ggplot(df_eval[df_eval$outliers==FALSE,] %>% pad(.,by = "time")) + 
      geom_line(aes(time,Qe, col="Measured") ) + 
      geom_line(aes(time,predicted,col="Predicted"),alpha=0.5) +
      ylim(c(0,max(df_eval[df_eval$outliers==FALSE,]$Qe,na.rm=T)*1.1)) +
      scale_colour_manual(
        name = NULL,
        values = c("Predicted" = "red", "Measured" = "black")) +
      theme_bw() + xlab("time") + ylab("Qe (kWh)") +
      theme(legend.position = "bottom")
    
    
    ggsave(paste(paste(settings$OutputDataDirectory,"plots",sep="/"),paste0(plotsName,"validation.png"),sep="/"),
           pv, width=7, height=3, dpi = 1000)
  }
  
  
  
  # Plot the predicted temperature and the real one to evaluate the prediction model results for the complete period
  if(plots){
    plotsName <- paste0("Calendar","Energy","_")
    
    pr <- ggplot(df_result[df_result$outliers==FALSE,] %>% pad(.,by = "time")) + 
      geom_line(aes(time,Qe, col="Measured") ) + 
      geom_line(aes(time,predicted,col="Predicted"),alpha=0.5) +
      ylim(c(0,max(df_result[df_result$outliers==FALSE,]$Qe,na.rm=T)*1.1)) +
      scale_colour_manual(
        name = NULL,
        values = c("Predicted" = "red", "Measured" = "black")) +
      theme_bw() + xlab("time") + ylab("Qe (Wh)") +
      theme(legend.position = "bottom")
    ggsave(paste(paste(settings$OutputDataDirectory,"plots",sep="/"),paste0(plotsName,"RealvsPredicted.png"),sep="/"),
           pr, width=7, height=3, dpi = 1000)
    
  }
  
  
  write("* Estimation finalised!",stderr())
  
  return(df_result)
}

