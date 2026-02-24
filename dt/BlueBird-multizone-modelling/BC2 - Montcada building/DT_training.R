# source("Utils.R")
source("EFAssessmentTrain.R") #Import Energy Efficiency Occupancy Measures Assesment code


#Load biggr package
suppressMessages(suppressWarnings(library(biggr)))

# Load configuration paths and settings for the algorithm
settings <- jsonlite::fromJSON("Settings.json")
settings <- append(settings,jsonlite::fromJSON("config.json"))

# Unzip input data if is needed

buildingSubject <- "Montcada Casa de la Vila"


#Import CSV
df_import<- read_csv(paste(settings$InputDataDirectory,"df_one_hour.csv",sep="/"))

#Format time variables
df_import$start <- as.POSIXct(df_import$start,, format = "%Y-%m-%d %H:%M:%S", tz = "UTC" )
df_import$end <- as.POSIXct(df_import$end,, format = "%Y-%m-%d %H:%M:%S", tz = "UTC" )
df_import <- df_import %>% mutate(time=start)
df_import <- df_import %>% select(-start,-end)
df_import$localtime <- with_tz(df_import$time, "Europe/Madrid")


# manualResults=F
# plots=F
results <- run_EF_assessment_train(buildingSubject, df_import, settings, libraryPath=".", modelMode="train", plots=F)

write_json(results, paste0(settings$OutputDataDirectory,"/results/predict_results.json"), pretty = TRUE, dataframe = "rows")