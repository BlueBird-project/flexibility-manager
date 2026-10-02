# source("Utils.R")
source("EFAssessmentPredict.R") #Import Energy Efficiency Occupancy Measures Assesment code


#Load biggr package
suppressMessages(suppressWarnings(library(biggr)))

# Load configuration paths and settings for the algorithm
settings <- jsonlite::fromJSON("Settings.json")
settings <- append(settings,jsonlite::fromJSON("config.json"))

# Unzip input data if is needed

buildingSubject <- "Montcada Casa de la Vila"


#Import JSON
df_predict <- fromJSON(paste(settings$InputDataDirectory,"df_predict.json",sep="/"))
df_predict$start <- lubridate::ymd_hms(df_predict$start)
df_predict$end <- lubridate::ymd_hms(df_predict$end)
df_predict <- df_predict %>% mutate(time=start)
df_predict <- df_predict %>% select(-start,-end)
# df_predict$time <- lubridate::ymd_hms(df_predict$time)
df_predict$localtime <- with_tz(df_predict$time, "Europe/Madrid")
df_predict <- df_predict[10:18,]

# manualResults=F
# plots=F
results <- run_EF_assessment_predict(buildingSubject, df_predict, settings, libraryPath=".", modelMode="predict", plots=F)


write_json(results, paste0(settings$OutputDataDirectory,"/results/predict_results.json"), pretty = TRUE, dataframe = "rows")