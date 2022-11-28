##  ---------------------------
##  02-Validate Locomotion
##
##  Following script will use the cleaned FMP data from 01-DataCleaning
##  and validate the locomotion predictions (not the intensity) from FMP.
##  
##  Author: "Nicki lentz"
##  Date: "15/3/2022"
##
##  Email: nickilentz@hotmail.com
## ---------------------------

  
library(tidyverse)

## -- Import FMP Data -- ##

#Import all the data and split the intensity from the movement type
#e.g., Dynamic medium intensity = Dynamic or known as non-linear in the paper.

FMPTuesdayNIU2 <- data.table::fread("Data/ProcessedData/FMPTuesdayNIU2.csv") %>% 
  #split movement type to get locomotion category and rename
  mutate(locomotion = sub(' .*', '', movement_type),
         locomotion = case_when(
           locomotion == "Low" ~ "Walking",
           locomotion == "Very" ~ "No locomotion",
           TRUE ~ as.character(locomotion)
         ))

FMPTuesdayNIU3 <- data.table::fread("Data/ProcessedData/FMPTuesdayNIU3.csv") %>% 
  mutate(locomotion = sub(' .*', '', movement_type),
         locomotion = case_when(
           locomotion == "Low" ~ "Walking",
           locomotion == "Very" ~ "No locomotion",
           TRUE ~ as.character(locomotion)
         ))


FMPWednesdayNIU1 <- data.table::fread("Data/ProcessedData/FMPWednesdayNIU1.csv") %>% 
  mutate(locomotion = sub(' .*', '', movement_type),
         locomotion = case_when(
           locomotion == "Low" ~ "Walking",
           locomotion == "Very" ~ "No locomotion",
           TRUE ~ as.character(locomotion)
         ))

FMPThursdayNIU2 <- data.table::fread("Data/ProcessedData/FMPThursdayNIU2.csv") %>% 
  mutate(locomotion = sub(' .*', '', movement_type),
         locomotion = case_when(
           locomotion == "Low" ~ "Walking",
           locomotion == "Very" ~ "No locomotion",
           TRUE ~ as.character(locomotion)
         ))

FMPThursdayNIU3 <- data.table::fread("Data/ProcessedData/FMPThursdayNIU3.csv") %>% 
  mutate(locomotion = sub(' .*', '', movement_type),
         locomotion = case_when(
           locomotion == "Low" ~ "Walking",
           locomotion == "Very" ~ "No locomotion",
           TRUE ~ as.character(locomotion)
         ))

## -- function to insert truth column -- ##


locomotionFMPtruth <- function(FMPdata, eventIntensity, eventType, athleteID = NULL, groupID, startTime, endTime){
  
  # -----------------------------
  # Function to insert event Intensity and type to compare with the locomotion category
  # with respect to each performed drill.
  
  # Args: 
  #   FMPdata: Tibble or Dataframe 
  #   eventIntensity: String, c("low","medium","high","highhalf"). Indicates the intensity of the drill. 
  #   eventType: String, c("Walking", "Running", "Dynamic"). Indicates the event type.
  #   athleteID: String. Individual athleteID, only used when eventType == "Dynamic".
  #   groupID: String. Which of the five groups was practicing
  #   StartTime: datetime yyyy/mm/dd h/m/s . Beginning of the practice
  #   endTime: datetime yyyy/mm/dd h/m/s. End of the practice
  #
  # Returns: Tibble
  #
  # -----------------------------
  #securing capital first letter
  eventIntensity <- tools::toTitleCase(eventIntensity)
  eventType <- tools::toTitleCase(eventType)
  
  if (is.null(athleteID)) {
    athleteID = FMPdata$athlete
  }
  
  syncFMPData <- FMPdata %>% 
    filter(athlete == athleteID,
           unixTime >= startTime,
           unixTime <= endTime) %>% 
    arrange(athlete, unixTime)
  
  FMPtruth <- syncFMPData %>% 
    mutate(truth = eventType,
           intensity = eventIntensity,
           athlete = paste(groupID, athlete, sep = "_"))
  
  FMPtruth$truth <- factor(FMPtruth$truth, levels = c("Dynamic", "Walking", "Running", "No locomotion"))
  FMPtruth$locomotion <- factor(FMPtruth$locomotion, levels = c("Dynamic", "Walking", "Running", "No locomotion"))
  
  return(FMPtruth)
}

## -- Insert truth for TuesdayNIU2 Walk -- ##

Tuesday_NIU2_Walk2 <- locomotionFMPtruth(FMPTuesdayNIU2, eventIntensity = "low", eventType = "Walking", groupID = "TuesdayNIU2",
                                         startTime = as.numeric(lubridate::as_datetime("2022-02-15 08:31:10 UTC")),
                                         endTime = as.numeric(lubridate::as_datetime("2022-02-15 08:31:53 UTC")))

Tuesday_NIU2_Walk3 <- locomotionFMPtruth(FMPTuesdayNIU2, eventIntensity = "low", eventType = "Walking", groupID = "TuesdayNIU2",
                                         startTime = lubridate::as_datetime("2022-02-15 08:32:07 UTC"),
                                         endTime = lubridate::as_datetime("2022-02-15 08:32:49 UTC"))

Tuesday_NIU2_Walk4 <- locomotionFMPtruth(FMPTuesdayNIU2, eventIntensity = "low", eventType = "Walking", groupID = "TuesdayNIU2",
                                         startTime = lubridate::as_datetime("2022-02-15 08:33:04 UTC"),
                                         endTime = lubridate::as_datetime("2022-02-15 08:33:47 UTC"))

Tuesday_NIU2_Walk5 <- locomotionFMPtruth(FMPTuesdayNIU2, eventIntensity = "low", eventType = "Walking", groupID = "TuesdayNIU2",
                                         startTime = lubridate::as_datetime("2022-02-15 08:34:02 UTC"),
                                         endTime = lubridate::as_datetime("2022-02-15 08:34:43 UTC"))


Tuesday_NIU2_Walk <- rbind(Tuesday_NIU2_Walk2,
                           Tuesday_NIU2_Walk3,
                           Tuesday_NIU2_Walk4,
                           Tuesday_NIU2_Walk5)

## -- Insert truth for TuesdayNIU2 Run 10m/3sec -- ##

Tuesday_NIU2_Run10_2 <- locomotionFMPtruth(FMPTuesdayNIU2, eventIntensity = "low", eventType = "Running", groupID = "TuesdayNIU2",
                                           startTime = lubridate::as_datetime("2022-02-15 08:37:37 UTC"),
                                           endTime = lubridate::as_datetime("2022-02-15 08:37:55 UTC"))

Tuesday_NIU2_Run10_3  <- locomotionFMPtruth(FMPTuesdayNIU2, eventIntensity = "low", eventType = "Running", groupID = "TuesdayNIU2",
                                            startTime = lubridate::as_datetime("2022-02-15 08:38:10 UTC"),
                                            endTime = lubridate::as_datetime("2022-02-15 08:38:28 UTC"))

Tuesday_NIU2_Run10_4  <- locomotionFMPtruth(FMPTuesdayNIU2, eventIntensity = "low", eventType = "Running", groupID = "TuesdayNIU2",
                                            startTime = lubridate::as_datetime("2022-02-15 08:38:43 UTC"),
                                            endTime = lubridate::as_datetime("2022-02-15 08:39:01 UTC"))

Tuesday_NIU2_Run10_5  <- locomotionFMPtruth(FMPTuesdayNIU2, eventIntensity = "low", eventType = "Running", groupID = "TuesdayNIU2",
                                            startTime = lubridate::as_datetime("2022-02-15 08:39:16 UTC"),
                                            endTime = lubridate::as_datetime("2022-02-15 08:39:34 UTC"))


Tuesday_NIU2_Run10 <- rbind(Tuesday_NIU2_Run10_2,
                            Tuesday_NIU2_Run10_3,
                            Tuesday_NIU2_Run10_4,
                            Tuesday_NIU2_Run10_5)

## -- Insert truth for TuesdayNIU2 Run 15m/3sec -- ##

Tuesday_NIU2_Run15_2 <- locomotionFMPtruth(FMPTuesdayNIU2, eventIntensity = "low", eventType = "Running", groupID = "TuesdayNIU2",
                                           startTime = lubridate::as_datetime("2022-02-15 08:45:29 UTC"),
                                           endTime = lubridate::as_datetime("2022-02-15 08:45:40 UTC"))

Tuesday_NIU2_Run15_3  <- locomotionFMPtruth(FMPTuesdayNIU2, eventIntensity = "low", eventType = "Running", groupID = "TuesdayNIU2",
                                            startTime = lubridate::as_datetime("2022-02-15 08:45:56 UTC"),
                                            endTime = lubridate::as_datetime("2022-02-15 08:46:07 UTC"))

Tuesday_NIU2_Run15_4  <- locomotionFMPtruth(FMPTuesdayNIU2, eventIntensity = "low", eventType = "Running", groupID = "TuesdayNIU2",
                                            startTime = lubridate::as_datetime("2022-02-15 08:46:23 UTC"),
                                            endTime = lubridate::as_datetime("2022-02-15 08:46:34 UTC"))

Tuesday_NIU2_Run15_5  <- locomotionFMPtruth(FMPTuesdayNIU2, eventIntensity = "low", eventType = "Running", groupID = "TuesdayNIU2",
                                            startTime = lubridate::as_datetime("2022-02-15 08:46:49 UTC"),
                                            endTime = lubridate::as_datetime("2022-02-15 08:47:00 UTC"))


Tuesday_NIU2_Run15 <- rbind(Tuesday_NIU2_Run15_2,
                            Tuesday_NIU2_Run15_3,
                            Tuesday_NIU2_Run15_4,
                            Tuesday_NIU2_Run15_5)

## -- Insert truth for TuesdayNIU2 Medium Dynamic -- ##

Tuesday_NIU2_medium_Yellow2 <- locomotionFMPtruth(FMPTuesdayNIU2, eventIntensity = "medium", eventType = "Dynamic",
                                                  athleteID = "Yellow-2", groupID = "TuesdayNIU2",
                                                  startTime = as.numeric(lubridate::as_datetime("2022-02-15 09:02:40 UTC")),
                                                  endTime = as.numeric(lubridate::as_datetime("2022-02-15 09:03:22 UTC")))

Tuesday_NIU2_medium_Yellow3 <- locomotionFMPtruth(FMPTuesdayNIU2, eventIntensity = "medium", eventType = "Dynamic",
                                                  athleteID = "Yellow-3", groupID = "TuesdayNIU2",
                                                  startTime = as.numeric(lubridate::as_datetime("2022-02-15 09:02:48 UTC")),
                                                  endTime = as.numeric(lubridate::as_datetime("2022-02-15 09:03:35 UTC")))

Tuesday_NIU2_medium_Yellow5 <- locomotionFMPtruth(FMPTuesdayNIU2, eventIntensity = "medium", eventType = "Dynamic",
                                                  athleteID = "Yellow-5", groupID = "TuesdayNIU2",
                                                  startTime = as.numeric(lubridate::as_datetime("2022-02-15 09:02:57 UTC")),
                                                  endTime = as.numeric(lubridate::as_datetime("2022-02-15 09:03:39 UTC")))

Tuesday_NIU2_medium_Yellow6 <- locomotionFMPtruth(FMPTuesdayNIU2, eventIntensity = "medium", eventType = "Dynamic",
                                                  athleteID = "Yellow-6", groupID = "TuesdayNIU2",
                                                  startTime = as.numeric(lubridate::as_datetime("2022-02-15 09:03:09 UTC")),
                                                  endTime = as.numeric(lubridate::as_datetime("2022-02-15 09:03:51 UTC")))

Tuesday_NIU2_medium_Yellow7 <- locomotionFMPtruth(FMPTuesdayNIU2, eventIntensity = "medium", eventType = "Dynamic",
                                                  athleteID = "Yellow-7", groupID = "TuesdayNIU2",
                                                  startTime = as.numeric(lubridate::as_datetime("2022-02-15 09:03:18 UTC")),
                                                  endTime = as.numeric(lubridate::as_datetime("2022-02-15 09:03:57 UTC")))

Tuesday_NIU2_medium_Yellow8 <- locomotionFMPtruth(FMPTuesdayNIU2, eventIntensity = "medium", eventType = "Dynamic",
                                                  athleteID = "Yellow-8", groupID = "TuesdayNIU2",
                                                  startTime = as.numeric(lubridate::as_datetime("2022-02-15 09:03:27 UTC")),
                                                  endTime = as.numeric(lubridate::as_datetime("2022-02-15 09:04:07 UTC")))

Tuesday_NIU2_medium_Yellow9 <- locomotionFMPtruth(FMPTuesdayNIU2, eventIntensity = "medium", eventType = "Dynamic",
                                                  athleteID = "Yellow-9", groupID = "TuesdayNIU2",
                                                  startTime = as.numeric(lubridate::as_datetime("2022-02-15 09:03:37 UTC")),
                                                  endTime = as.numeric(lubridate::as_datetime("2022-02-15 09:04:17 UTC")))

Tuesday_NIU2_medium_Yellow10 <- locomotionFMPtruth(FMPTuesdayNIU2, eventIntensity = "medium", eventType = "Dynamic",
                                                   athleteID = "Yellow-10", groupID = "TuesdayNIU2",
                                                   startTime = as.numeric(lubridate::as_datetime("2022-02-15 09:03:42 UTC")),
                                                   endTime = as.numeric(lubridate::as_datetime("2022-02-15 09:04:28 UTC")))

Tuesday_NIU2_medium_Orange2 <- locomotionFMPtruth(FMPTuesdayNIU2, eventIntensity = "medium", eventType = "Dynamic",
                                                  athleteID = "Orange-2", groupID = "TuesdayNIU2",
                                                  startTime = as.numeric(lubridate::as_datetime("2022-02-15 09:03:55 UTC")),
                                                  endTime = as.numeric(lubridate::as_datetime("2022-02-15 09:04:40 UTC")))

Tuesday_NIU2_medium_Orange3 <- locomotionFMPtruth(FMPTuesdayNIU2, eventIntensity = "medium", eventType = "Dynamic",
                                                  athleteID = "Orange-3", groupID = "TuesdayNIU2",
                                                  startTime = as.numeric(lubridate::as_datetime("2022-02-15 09:04:01 UTC")),
                                                  endTime = as.numeric(lubridate::as_datetime("2022-02-15 09:04:41 UTC")))

Tuesday_NIU2_medium_Orange4 <- locomotionFMPtruth(FMPTuesdayNIU2, eventIntensity = "medium", eventType = "Dynamic",
                                                  athleteID = "Orange-4", groupID = "TuesdayNIU2",
                                                  startTime = as.numeric(lubridate::as_datetime("2022-02-15 09:04:12 UTC")),
                                                  endTime = as.numeric(lubridate::as_datetime("2022-02-15 09:04:57 UTC")))

Tuesday_NIU2_medium_Orange5 <- locomotionFMPtruth(FMPTuesdayNIU2, eventIntensity = "medium", eventType = "Dynamic",
                                                  athleteID = "Orange-5", groupID = "TuesdayNIU2",
                                                  startTime = as.numeric(lubridate::as_datetime("2022-02-15 09:04:24 UTC")),
                                                  endTime = as.numeric(lubridate::as_datetime("2022-02-15 09:05:11 UTC")))

Tuesday_NIU2_medium_Orange6 <- locomotionFMPtruth(FMPTuesdayNIU2, eventIntensity = "medium", eventType = "Dynamic",
                                                  athleteID = "Orange-6", groupID = "TuesdayNIU2",
                                                  startTime = as.numeric(lubridate::as_datetime("2022-02-15 09:04:34 UTC")),
                                                  endTime = as.numeric(lubridate::as_datetime("2022-02-15 09:05:14 UTC")))

Tuesday_NIU2_medium_Orange7 <- locomotionFMPtruth(FMPTuesdayNIU2, eventIntensity = "medium", eventType = "Dynamic",
                                                  athleteID = "Orange-8", groupID = "TuesdayNIU2",
                                                  startTime = as.numeric(lubridate::as_datetime("2022-02-15 09:04:40 UTC")),
                                                  endTime = as.numeric(lubridate::as_datetime("2022-02-15 09:05:19 UTC")))

Tuesday_NIU2_medium_Orange8 <- locomotionFMPtruth(FMPTuesdayNIU2, eventIntensity = "medium", eventType = "Dynamic",
                                                  athleteID = "Orange-8", groupID = "TuesdayNIU2",
                                                  startTime = as.numeric(lubridate::as_datetime("2022-02-15 09:04:49 UTC")),
                                                  endTime = as.numeric(lubridate::as_datetime("2022-02-15 09:05:26 UTC")))

Tuesday_NIU2_mediumDynamic <- rbind(Tuesday_NIU2_medium_Yellow2, 
                                    Tuesday_NIU2_medium_Yellow3, 
                                    Tuesday_NIU2_medium_Yellow5, 
                                    Tuesday_NIU2_medium_Yellow6, 
                                    Tuesday_NIU2_medium_Yellow7,
                                    Tuesday_NIU2_medium_Yellow8, 
                                    Tuesday_NIU2_medium_Yellow9, 
                                    Tuesday_NIU2_medium_Yellow10, 
                                    Tuesday_NIU2_medium_Orange2, 
                                    Tuesday_NIU2_medium_Orange3,
                                    Tuesday_NIU2_medium_Orange4, 
                                    Tuesday_NIU2_medium_Orange5, 
                                    Tuesday_NIU2_medium_Orange6, 
                                    Tuesday_NIU2_medium_Orange7, 
                                    Tuesday_NIU2_medium_Orange8)

## -- Insert truth for TuesdayNIU2 High Dynamic -- ##

Tuesday_NIU2_high_Yellow2 <- locomotionFMPtruth(FMPTuesdayNIU2, eventIntensity = "high", eventType = "Dynamic",
                                                athleteID = "Yellow-2", groupID = "TuesdayNIU2",
                                                startTime = lubridate::as_datetime("2022-02-15 09:06:55 UTC"),
                                                endTime = lubridate::as_datetime("2022-02-15 09:07:31 UTC"))

Tuesday_NIU2_high_Yellow3 <- locomotionFMPtruth(FMPTuesdayNIU2, eventIntensity = "high", eventType = "Dynamic",
                                                athleteID = "Yellow-3", groupID = "TuesdayNIU2",
                                                startTime = lubridate::as_datetime("2022-02-15 09:07:04 UTC"),
                                                endTime = lubridate::as_datetime("2022-02-15 09:07:40 UTC"))

Tuesday_NIU2_high_Yellow5 <- locomotionFMPtruth(FMPTuesdayNIU2, eventIntensity = "high", eventType = "Dynamic",
                                                athleteID = "Yellow-5", groupID = "TuesdayNIU2",
                                                startTime = lubridate::as_datetime("2022-02-15 09:07:13 UTC"),
                                                endTime = lubridate::as_datetime("2022-02-15 09:07:49"))

Tuesday_NIU2_high_Yellow6 <- locomotionFMPtruth(FMPTuesdayNIU2, eventIntensity = "high", eventType = "Dynamic",
                                                athleteID = "Yellow-6", groupID = "TuesdayNIU2",
                                                startTime = lubridate::as_datetime("2022-02-15 09:07:22 UTC"),
                                                endTime = lubridate::as_datetime("2022-02-15 09:07:53 UTC"))

Tuesday_NIU2_high_Yellow7 <- locomotionFMPtruth(FMPTuesdayNIU2, eventIntensity = "high", eventType = "Dynamic",
                                                athleteID = "Yellow-7", groupID = "TuesdayNIU2",
                                                startTime = lubridate::as_datetime("2022-02-15 09:07:30 UTC"),
                                                endTime = lubridate::as_datetime("2022-02-15 09:08:01 UTC"))

Tuesday_NIU2_high_Yellow8 <- locomotionFMPtruth(FMPTuesdayNIU2, eventIntensity = "high", eventType = "Dynamic",
                                                athleteID = "Yellow-8", groupID = "TuesdayNIU2",
                                                startTime = lubridate::as_datetime("2022-02-15 09:07:38 UTC"),
                                                endTime = lubridate::as_datetime("2022-02-15 09:08:07 UTC"))

Tuesday_NIU2_high_Yellow9 <- locomotionFMPtruth(FMPTuesdayNIU2, eventIntensity = "high", eventType = "Dynamic",
                                                athleteID = "Yellow-9", groupID = "TuesdayNIU2",
                                                startTime = lubridate::as_datetime("2022-02-15 09:07:46 UTC"),
                                                endTime = lubridate::as_datetime("2022-02-15 09:08:17 UTC"))

Tuesday_NIU2_high_Yellow10 <- locomotionFMPtruth(FMPTuesdayNIU2, eventIntensity = "high", eventType = "Dynamic",
                                                 athleteID = "Yellow-10", groupID = "TuesdayNIU2",
                                                 startTime = lubridate::as_datetime("2022-02-15 09:07:53 UTC"),
                                                 endTime = lubridate::as_datetime("2022-02-15 09:08:21 UTC"))

Tuesday_NIU2_high_Orange2 <- locomotionFMPtruth(FMPTuesdayNIU2, eventIntensity = "high", eventType = "Dynamic",
                                                athleteID = "Orange-2", groupID = "TuesdayNIU2",
                                                startTime = lubridate::as_datetime("2022-02-15 09:08:00 UTC"),
                                                endTime = lubridate::as_datetime("2022-02-15 09:08:33 UTC"))

Tuesday_NIU2_high_Orange3 <- locomotionFMPtruth(FMPTuesdayNIU2, eventIntensity = "high", eventType = "Dynamic",
                                                athleteID = "Orange-3", groupID = "TuesdayNIU2",
                                                startTime = lubridate::as_datetime("2022-02-15 09:08:13 UTC"),
                                                endTime = lubridate::as_datetime("2022-02-15 09:08:44 UTC"))

Tuesday_NIU2_high_Orange4 <- locomotionFMPtruth(FMPTuesdayNIU2, eventIntensity = "high", eventType = "Dynamic",
                                                athleteID = "Orange-4", groupID = "TuesdayNIU2",
                                                startTime = lubridate::as_datetime("2022-02-15 09:08:21 UTC"),
                                                endTime = lubridate::as_datetime("2022-02-15 09:08:53 UTC"))

Tuesday_NIU2_high_Orange5 <- locomotionFMPtruth(FMPTuesdayNIU2, eventIntensity = "high", eventType = "Dynamic",
                                                athleteID = "Orange-5", groupID = "TuesdayNIU2",
                                                startTime = lubridate::as_datetime("2022-02-15 09:08:28 UTC"),
                                                endTime = lubridate::as_datetime("2022-02-15 09:09:02 UTC"))

Tuesday_NIU2_high_Orange6 <- locomotionFMPtruth(FMPTuesdayNIU2, eventIntensity = "high", eventType = "Dynamic",
                                                athleteID = "Orange-6", groupID = "TuesdayNIU2",
                                                startTime = lubridate::as_datetime("2022-02-15 09:08:36 UTC"),
                                                endTime = lubridate::as_datetime("2022-02-15 09:09:03 UTC"))

Tuesday_NIU2_high_Orange7 <- locomotionFMPtruth(FMPTuesdayNIU2, eventIntensity = "high", eventType = "Dynamic",
                                                athleteID = "Orange-7", groupID = "TuesdayNIU2",
                                                startTime = lubridate::as_datetime("2022-02-15 09:08:43 UTC"),
                                                endTime = lubridate::as_datetime("2022-02-15 09:09:07 UTC"))

Tuesday_NIU2_high_Orange8 <- locomotionFMPtruth(FMPTuesdayNIU2, eventIntensity = "high", eventType = "Dynamic",
                                                athleteID = "Orange-8", groupID = "TuesdayNIU2",
                                                startTime = lubridate::as_datetime("2022-02-15 09:08:49 UTC"),
                                                endTime = lubridate::as_datetime("2022-02-15 09:09:19 UTC"))

Tuesday_NIU2_highDynamic <- rbind(Tuesday_NIU2_high_Yellow2, 
                                  Tuesday_NIU2_high_Yellow3, 
                                  Tuesday_NIU2_high_Yellow5, 
                                  Tuesday_NIU2_high_Yellow6, 
                                  Tuesday_NIU2_high_Yellow7,
                                  Tuesday_NIU2_high_Yellow8,
                                  Tuesday_NIU2_high_Yellow9,
                                  Tuesday_NIU2_high_Yellow10,
                                  Tuesday_NIU2_high_Orange2,
                                  Tuesday_NIU2_high_Orange3,
                                  Tuesday_NIU2_high_Orange4, 
                                  Tuesday_NIU2_high_Orange5, 
                                  Tuesday_NIU2_high_Orange6, 
                                  Tuesday_NIU2_high_Orange7, 
                                  Tuesday_NIU2_high_Orange8)


## -- Insert truth for TuesdayNIU2 high half Dynamic -- ##

Tuesday_NIU2_highHalf_Yellow2 <- locomotionFMPtruth(FMPTuesdayNIU2, eventIntensity = "highhalf", eventType = "Dynamic",
                                                    athleteID = "Yellow-2", groupID = "TuesdayNIU2",
                                                    startTime = as.numeric(lubridate::as_datetime("2022-02-15 09:10:19 UTC")),
                                                    endTime = as.numeric(lubridate::as_datetime("2022-02-15 09:10:35 UTC")))

Tuesday_NIU2_highHalf_Yellow3 <- locomotionFMPtruth(FMPTuesdayNIU2, eventIntensity = "highhalf", eventType = "Dynamic",
                                                    athleteID = "Yellow-3", groupID = "TuesdayNIU2",
                                                    startTime = as.numeric(lubridate::as_datetime("2022-02-15 09:10:24 UTC")),
                                                    endTime = as.numeric(lubridate::as_datetime("2022-02-15 09:10:41 UTC")))

Tuesday_NIU2_highHalf_Yellow5 <- locomotionFMPtruth(FMPTuesdayNIU2, eventIntensity = "highhalf", eventType = "Dynamic",
                                                    athleteID = "Yellow-5", groupID = "TuesdayNIU2",
                                                    startTime = as.numeric(lubridate::as_datetime("2022-02-15 09:10:30 UTC")),
                                                    endTime = as.numeric(lubridate::as_datetime("2022-02-15 09:10:47 UTC")))

Tuesday_NIU2_highHalf_Yellow6 <- locomotionFMPtruth(FMPTuesdayNIU2, eventIntensity = "highhalf", eventType = "Dynamic",
                                                    athleteID = "Yellow-6", groupID = "TuesdayNIU2",
                                                    startTime = as.numeric(lubridate::as_datetime("2022-02-15 09:10:36 UTC")),
                                                    endTime = as.numeric(lubridate::as_datetime("2022-02-15 09:10:51 UTC")))

Tuesday_NIU2_highHalf_Yellow7 <- locomotionFMPtruth(FMPTuesdayNIU2, eventIntensity = "highhalf", eventType = "Dynamic",
                                                    athleteID = "Yellow-7", groupID = "TuesdayNIU2",
                                                    startTime = as.numeric(lubridate::as_datetime("2022-02-15 09:10:43 UTC")),
                                                    endTime = as.numeric(lubridate::as_datetime("2022-02-15 09:10:59 UTC")))

Tuesday_NIU2_highHalf_Yellow8 <- locomotionFMPtruth(FMPTuesdayNIU2, eventIntensity = "highhalf", eventType = "Dynamic",
                                                    athleteID = "Yellow-8", groupID = "TuesdayNIU2",
                                                    startTime = as.numeric(lubridate::as_datetime("2022-02-15 09:10:55 UTC")),
                                                    endTime = as.numeric(lubridate::as_datetime("2022-02-15 09:11:10 UTC")))

Tuesday_NIU2_highHalf_Yellow9 <- locomotionFMPtruth(FMPTuesdayNIU2, eventIntensity = "highhalf", eventType = "Dynamic",
                                                    athleteID = "Yellow-9", groupID = "TuesdayNIU2",
                                                    startTime = as.numeric(lubridate::as_datetime("2022-02-15 09:11:01 UTC")),
                                                    endTime = as.numeric(lubridate::as_datetime("2022-02-15 09:11:16 UTC")))

Tuesday_NIU2_highHalf_Yellow10 <- locomotionFMPtruth(FMPTuesdayNIU2, eventIntensity = "highhalf", eventType = "Dynamic",
                                                     athleteID = "Yellow-10", groupID = "TuesdayNIU2",
                                                     startTime = as.numeric(lubridate::as_datetime("2022-02-15 09:11:07 UTC")),
                                                     endTime = as.numeric(lubridate::as_datetime("2022-02-15 09:11:22 UTC")))

Tuesday_NIU2_highHalf_Orange2 <- locomotionFMPtruth(FMPTuesdayNIU2, eventIntensity = "highhalf", eventType = "Dynamic",
                                                    athleteID = "Orange-2", groupID = "TuesdayNIU2",
                                                    startTime = as.numeric(lubridate::as_datetime("2022-02-15 09:11:14 UTC")),
                                                    endTime = as.numeric(lubridate::as_datetime("2022-02-15 09:11:30 UTC")))

Tuesday_NIU2_highHalf_Orange3 <- locomotionFMPtruth(FMPTuesdayNIU2, eventIntensity = "highhalf", eventType = "Dynamic",
                                                    athleteID = "Orange-3", groupID = "TuesdayNIU2",
                                                    startTime = as.numeric(lubridate::as_datetime("2022-02-15 09:11:22 UTC")),
                                                    endTime = as.numeric(lubridate::as_datetime("2022-02-15 09:11:38 UTC")))

Tuesday_NIU2_highHalf_Orange4 <- locomotionFMPtruth(FMPTuesdayNIU2, eventIntensity = "highhalf", eventType = "Dynamic",
                                                    athleteID = "Orange-4", groupID = "TuesdayNIU2",
                                                    startTime = as.numeric(lubridate::as_datetime("2022-02-15 09:11:28 UTC")),
                                                    endTime = as.numeric(lubridate::as_datetime("2022-02-15 09:11:44 UTC")))

Tuesday_NIU2_highHalf_Orange5 <- locomotionFMPtruth(FMPTuesdayNIU2, eventIntensity = "highhalf", eventType = "Dynamic",
                                                    athleteID = "Orange-5", groupID = "TuesdayNIU2",
                                                    startTime = as.numeric(lubridate::as_datetime("2022-02-15 09:11:35 UTC")),
                                                    endTime = as.numeric(lubridate::as_datetime("2022-02-15 09:11:51 UTC")))

Tuesday_NIU2_highHalf_Orange6 <- locomotionFMPtruth(FMPTuesdayNIU2, eventIntensity = "highhalf", eventType = "Dynamic",
                                                    athleteID = "Orange-6", groupID = "TuesdayNIU2",
                                                    startTime = as.numeric(lubridate::as_datetime("2022-02-15 09:11:42 UTC")),
                                                    endTime = as.numeric(lubridate::as_datetime("2022-02-15 09:11:57 UTC")))

Tuesday_NIU2_highHalf_Orange7 <- locomotionFMPtruth(FMPTuesdayNIU2, eventIntensity = "highhalf", eventType = "Dynamic",
                                                    athleteID = "Orange-7", groupID = "TuesdayNIU2",
                                                    startTime = as.numeric(lubridate::as_datetime("2022-02-15 09:11:48 UTC")),
                                                    endTime = as.numeric(lubridate::as_datetime("2022-02-15 09:12:03 UTC")))

Tuesday_NIU2_highHalf_Orange8 <- locomotionFMPtruth(FMPTuesdayNIU2, eventIntensity = "highhalf", eventType = "Dynamic",
                                                    athleteID = "Orange-8", groupID = "TuesdayNIU2",
                                                    startTime = as.numeric(lubridate::as_datetime("2022-02-15 09:11:54 UTC")),
                                                    endTime = as.numeric(lubridate::as_datetime("2022-02-15 09:12:10 UTC")))

Tuesday_NIU2_highHalf <- rbind(Tuesday_NIU2_highHalf_Yellow2, 
                               Tuesday_NIU2_highHalf_Yellow3, 
                               Tuesday_NIU2_highHalf_Yellow5, 
                               Tuesday_NIU2_highHalf_Yellow6, 
                               Tuesday_NIU2_highHalf_Yellow7,
                               Tuesday_NIU2_highHalf_Yellow8, 
                               Tuesday_NIU2_highHalf_Yellow9, 
                               Tuesday_NIU2_highHalf_Yellow10, 
                               Tuesday_NIU2_highHalf_Orange2, 
                               Tuesday_NIU2_highHalf_Orange3,
                               Tuesday_NIU2_highHalf_Orange4,
                               Tuesday_NIU2_highHalf_Orange5, 
                               Tuesday_NIU2_highHalf_Orange6, 
                               Tuesday_NIU2_highHalf_Orange7, 
                               Tuesday_NIU2_highHalf_Orange8)


## -- Insert truth for TuesdayNIU3 Walk -- ##

Tuesday_NIU3_Walk2 <- locomotionFMPtruth(FMPTuesdayNIU3, eventIntensity = "low", eventType = "Walking", groupID = "TuesdayNIU3",
                                         startTime = lubridate::as_datetime("2022-02-15 10:20:10 UTC"),
                                         endTime = lubridate::as_datetime("2022-02-15 10:20:50 UTC"))

Tuesday_NIU3_Walk3 <- locomotionFMPtruth(FMPTuesdayNIU3, eventIntensity = "low", eventType = "Walking", groupID = "TuesdayNIU3",
                                         startTime = lubridate::as_datetime("2022-02-15 10:21:03 UTC"),
                                         endTime = lubridate::as_datetime("2022-02-15 10:21:43 UTC"))

Tuesday_NIU3_Walk4 <- locomotionFMPtruth(FMPTuesdayNIU3, eventIntensity = "low", eventType = "Walking", groupID = "TuesdayNIU3",
                                         startTime = lubridate::as_datetime("2022-02-15 10:21:57 UTC"),
                                         endTime = lubridate::as_datetime("2022-02-15 10:22:37 UTC"))

Tuesday_NIU3_Walk5 <- locomotionFMPtruth(FMPTuesdayNIU3, eventIntensity = "low", eventType = "Walking", groupID = "TuesdayNIU3",
                                         startTime = lubridate::as_datetime("2022-02-15 10:22:50 UTC"),
                                         endTime = lubridate::as_datetime("2022-02-15 10:23:30 UTC"))


Tuesday_NIU3_Walk <- rbind(Tuesday_NIU3_Walk2,
                           Tuesday_NIU3_Walk3,
                           Tuesday_NIU3_Walk4,
                           Tuesday_NIU3_Walk5)


## -- Insert truth for TuesdayNIU3 Run 10m/3sec -- ##

Tuesday_NIU3_Run10_3 <- locomotionFMPtruth(FMPTuesdayNIU3, eventIntensity = "medium", eventType = "Running", groupID = "TuesdayNIU3",
                                           startTime = as.numeric(lubridate::as_datetime("2022-02-15 10:27:12 UTC")),
                                           endTime = as.numeric(lubridate::as_datetime("2022-02-15 10:27:30 UTC")))

Tuesday_NIU3_Run10_4  <- locomotionFMPtruth(FMPTuesdayNIU3, eventIntensity = "medium", eventType = "Running", groupID = "TuesdayNIU3",
                                            startTime = as.numeric(lubridate::as_datetime("2022-02-15 10:27:42 UTC")),
                                            endTime = as.numeric(lubridate::as_datetime("2022-02-15 10:28:00 UTC")))

Tuesday_NIU3_Run10_5  <- locomotionFMPtruth(FMPTuesdayNIU3, eventIntensity = "medium", eventType = "Running", groupID = "TuesdayNIU3",
                                            startTime = as.numeric(lubridate::as_datetime("2022-02-15 10:28:12 UTC")),
                                            endTime = as.numeric(lubridate::as_datetime("2022-02-15 10:28:30 UTC")))

Tuesday_NIU3_Run10_6  <- locomotionFMPtruth(FMPTuesdayNIU3, eventIntensity = "medium", eventType = "Running", groupID = "TuesdayNIU3",
                                            startTime = as.numeric(lubridate::as_datetime("2022-02-15 10:28:43 UTC")),
                                            endTime = as.numeric(lubridate::as_datetime("2022-02-15 10:29:01 UTC")))


Tuesday_NIU3_Run10 <- rbind(Tuesday_NIU3_Run10_3,
                            Tuesday_NIU3_Run10_4,
                            Tuesday_NIU3_Run10_5,
                            Tuesday_NIU3_Run10_6)


## -- Insert truth for TuesdayNIU3 Run 15m/3sec -- ##

Tuesday_NIU3_Run15_2 <- locomotionFMPtruth(FMPTuesdayNIU3, eventIntensity = "high", eventType = "Running", groupID = "TuesdayNIU3",
                                           startTime = as.numeric(lubridate::as_datetime("2022-02-15 10:39:35 UTC")),
                                           endTime = as.numeric(lubridate::as_datetime("2022-02-15 10:39:41 UTC")))

Tuesday_NIU3_Run15_3  <- locomotionFMPtruth(FMPTuesdayNIU3, eventIntensity = "high", eventType = "Running", groupID = "TuesdayNIU3",
                                            startTime = as.numeric(lubridate::as_datetime("2022-02-15 10:40:02 UTC")),
                                            endTime = as.numeric(lubridate::as_datetime("2022-02-15 10:40:08 UTC")))

Tuesday_NIU3_Run15_4  <- locomotionFMPtruth(FMPTuesdayNIU3, eventIntensity = "high", eventType = "Running", groupID = "TuesdayNIU3",
                                            startTime = as.numeric(lubridate::as_datetime("2022-02-15 10:40:29 UTC")),
                                            endTime = as.numeric(lubridate::as_datetime("2022-02-15 10:40:35 UTC")))

Tuesday_NIU3_Run15_5  <- locomotionFMPtruth(FMPTuesdayNIU3, eventIntensity = "high", eventType = "Running", groupID = "TuesdayNIU3",
                                            startTime = as.numeric(lubridate::as_datetime("2022-02-15 10:40:58 UTC")),
                                            endTime = as.numeric(lubridate::as_datetime("2022-02-15 10:41:04 UTC")))


Tuesday_NIU3_Run15 <- rbind(Tuesday_NIU3_Run15_2,
                            Tuesday_NIU3_Run15_3,
                            Tuesday_NIU3_Run15_4,
                            Tuesday_NIU3_Run15_5)



## -- Insert truth for TuesdayNIU3 Medium Dynamic 1 -- ##

Tuesday_NIU3_medium_Orange2 <- locomotionFMPtruth(FMPTuesdayNIU3, eventIntensity = "medium", eventType = "Dynamic",
                                                  athleteID = "Orange-2", groupID = "TuesdayNIU3",
                                                  startTime = as.numeric(lubridate::as_datetime("2022-02-15 10:48:35 UTC")),
                                                  endTime = as.numeric(lubridate::as_datetime("2022-02-15 10:49:20 UTC")))

Tuesday_NIU3_medium_Orange4 <- locomotionFMPtruth(FMPTuesdayNIU3, eventIntensity = "medium", eventType = "Dynamic",
                                                  athleteID = "Orange-4", groupID = "TuesdayNIU3",
                                                  startTime = as.numeric(lubridate::as_datetime("2022-02-15 10:48:49 UTC")),
                                                  endTime = as.numeric(lubridate::as_datetime("2022-02-15 10:49:34 UTC")))

Tuesday_NIU3_medium_Orange5 <- locomotionFMPtruth(FMPTuesdayNIU3, eventIntensity = "medium", eventType = "Dynamic",
                                                  athleteID = "Orange-5", groupID = "TuesdayNIU3",
                                                  startTime = as.numeric(lubridate::as_datetime("2022-02-15 10:49:01 UTC")),
                                                  endTime = as.numeric(lubridate::as_datetime("2022-02-15 10:49:47 UTC")))

Tuesday_NIU3_medium_Yellow2 <- locomotionFMPtruth(FMPTuesdayNIU3, eventIntensity = "medium", eventType = "Dynamic",
                                                  athleteID = "Yellow-2", groupID = "TuesdayNIU3",
                                                  startTime = as.numeric(lubridate::as_datetime("2022-02-15 10:49:17 UTC")),
                                                  endTime = as.numeric(lubridate::as_datetime("2022-02-15 10:50:04 UTC")))

Tuesday_NIU3_medium_Yellow3 <- locomotionFMPtruth(FMPTuesdayNIU3, eventIntensity = "medium", eventType = "Dynamic",
                                                  athleteID = "Yellow-3", groupID = "TuesdayNIU3",
                                                  startTime = as.numeric(lubridate::as_datetime("2022-02-15 10:49:31 UTC")),
                                                  endTime = as.numeric(lubridate::as_datetime("2022-02-15 10:50:19 UTC")))

Tuesday_NIU3_medium_Yellow5 <- locomotionFMPtruth(FMPTuesdayNIU3, eventIntensity = "medium", eventType = "Dynamic",
                                                  athleteID = "Yellow-5", groupID = "TuesdayNIU3",
                                                  startTime = as.numeric(lubridate::as_datetime("2022-02-15 10:49:46 UTC")),
                                                  endTime = as.numeric(lubridate::as_datetime("2022-02-15 10:50:31 UTC")))

Tuesday_NIU3_medium_Yellow6 <- locomotionFMPtruth(FMPTuesdayNIU3, eventIntensity = "medium", eventType = "Dynamic",
                                                  athleteID = "Yellow-6", groupID = "TuesdayNIU3",
                                                  startTime = as.numeric(lubridate::as_datetime("2022-02-15 10:50:02 UTC")),
                                                  endTime = as.numeric(lubridate::as_datetime("2022-02-15 10:50:48 UTC")))



Tuesday_NIU3_mediumDynamic <- rbind(Tuesday_NIU3_medium_Orange2, 
                                    Tuesday_NIU3_medium_Orange4, 
                                    Tuesday_NIU3_medium_Orange5,
                                    Tuesday_NIU3_medium_Yellow2, 
                                    Tuesday_NIU3_medium_Yellow3, 
                                    Tuesday_NIU3_medium_Yellow5, 
                                    Tuesday_NIU3_medium_Yellow6)





## -- Insert truth for TuesdayNIU3 Medium Dynamic 2 -- ##


Tuesday_NIU3_medium2_Orange2 <- locomotionFMPtruth(FMPTuesdayNIU3, eventIntensity = "medium", eventType = "Dynamic",
                                                   athleteID = "Orange-2", groupID = "TuesdayNIU3",
                                                   startTime = as.numeric(lubridate::as_datetime("2022-02-15 10:52:11 UTC")),
                                                   endTime = as.numeric(lubridate::as_datetime("2022-02-15 10:52:57 UTC")))

Tuesday_NIU3_medium2_Orange4 <- locomotionFMPtruth(FMPTuesdayNIU3, eventIntensity = "medium", eventType = "Dynamic",
                                                   athleteID = "Orange-4", groupID = "TuesdayNIU3",
                                                   startTime = as.numeric(lubridate::as_datetime("2022-02-15 10:52:26 UTC")),
                                                   endTime = as.numeric(lubridate::as_datetime("2022-02-15 10:53:11 UTC")))

Tuesday_NIU3_medium2_Orange5 <- locomotionFMPtruth(FMPTuesdayNIU3, eventIntensity = "medium", eventType = "Dynamic",
                                                   athleteID = "Orange-5", groupID = "TuesdayNIU3",
                                                   startTime = as.numeric(lubridate::as_datetime("2022-02-15 10:52:37 UTC")),
                                                   endTime = as.numeric(lubridate::as_datetime("2022-02-15 10:53:22 UTC")))

Tuesday_NIU3_medium2_Yellow2 <- locomotionFMPtruth(FMPTuesdayNIU3, eventIntensity = "medium", eventType = "Dynamic",
                                                   athleteID = "Yellow-2", groupID = "TuesdayNIU3",
                                                   startTime = as.numeric(lubridate::as_datetime("2022-02-15 10:52:53 UTC")),
                                                   endTime = as.numeric(lubridate::as_datetime("2022-02-15 10:53:40 UTC")))

Tuesday_NIU3_medium2_Yellow3 <- locomotionFMPtruth(FMPTuesdayNIU3, eventIntensity = "medium", eventType = "Dynamic",
                                                   athleteID = "Yellow-3", groupID = "TuesdayNIU3",
                                                   startTime = as.numeric(lubridate::as_datetime("2022-02-15 10:53:08 UTC")),
                                                   endTime = as.numeric(lubridate::as_datetime("2022-02-15 10:53:54 UTC")))

Tuesday_NIU3_medium2_Yellow5 <- locomotionFMPtruth(FMPTuesdayNIU3, eventIntensity = "medium", eventType = "Dynamic",
                                                   athleteID = "Yellow-5", groupID = "TuesdayNIU3",
                                                   startTime = as.numeric(lubridate::as_datetime("2022-02-15 10:53:20 UTC")),
                                                   endTime = as.numeric(lubridate::as_datetime("2022-02-15 10:54:05 UTC")))

Tuesday_NIU3_medium2_Yellow6 <- locomotionFMPtruth(FMPTuesdayNIU3, eventIntensity = "medium", eventType = "Dynamic",
                                                   athleteID = "Yellow-6", groupID = "TuesdayNIU3",
                                                   startTime = as.numeric(lubridate::as_datetime("2022-02-15 10:53:37 UTC")),
                                                   endTime = as.numeric(lubridate::as_datetime("2022-02-15 10:54:23 UTC")))


Tuesday_NIU3_mediumDynamic2 <- rbind(Tuesday_NIU3_medium2_Orange2, 
                                     Tuesday_NIU3_medium2_Orange4, 
                                     Tuesday_NIU3_medium2_Orange5,
                                     Tuesday_NIU3_medium2_Yellow2, 
                                     Tuesday_NIU3_medium2_Yellow3, 
                                     Tuesday_NIU3_medium2_Yellow5, 
                                     Tuesday_NIU3_medium2_Yellow6)



## -- Insert truth for TuesdayNIU3 High dynamic -- ##


Tuesday_NIU3_high_Orange2 <- locomotionFMPtruth(FMPTuesdayNIU3, eventIntensity = "high", eventType = "Dynamic",
                                                athleteID = "Orange-2", groupID = "TuesdayNIU3",
                                                startTime = as.numeric(lubridate::as_datetime("2022-02-15 10:58:33 UTC")),
                                                endTime = as.numeric(lubridate::as_datetime("2022-02-15 10:59:12 UTC")))

Tuesday_NIU3_high_Orange4 <- locomotionFMPtruth(FMPTuesdayNIU3, eventIntensity = "high", eventType = "Dynamic",
                                                athleteID = "Orange-4", groupID = "TuesdayNIU3",
                                                startTime = as.numeric(lubridate::as_datetime("2022-02-15 10:58:53 UTC")),
                                                endTime = as.numeric(lubridate::as_datetime("2022-02-15 10:59:24 UTC")))

Tuesday_NIU3_high_Orange5 <- locomotionFMPtruth(FMPTuesdayNIU3, eventIntensity = "high", eventType = "Dynamic",
                                                athleteID = "Orange-5", groupID = "TuesdayNIU3",
                                                startTime = as.numeric(lubridate::as_datetime("2022-02-15 10:59:05 UTC")),
                                                endTime = as.numeric(lubridate::as_datetime("2022-02-15 10:59:37 UTC")))

Tuesday_NIU3_high_Yellow2 <- locomotionFMPtruth(FMPTuesdayNIU3, eventIntensity = "high", eventType = "Dynamic",
                                                athleteID = "Yellow-2", groupID = "TuesdayNIU3",
                                                startTime = as.numeric(lubridate::as_datetime("2022-02-15 10:59:17 UTC")),
                                                endTime = as.numeric(lubridate::as_datetime("2022-02-15 10:59:52 UTC")))

Tuesday_NIU3_high_Yellow3 <- locomotionFMPtruth(FMPTuesdayNIU3, eventIntensity = "high", eventType = "Dynamic",
                                                athleteID = "Yellow-3", groupID = "TuesdayNIU3",
                                                startTime = as.numeric(lubridate::as_datetime("2022-02-15 10:59:32 UTC")),
                                                endTime = as.numeric(lubridate::as_datetime("2022-02-15 11:00:10 UTC")))

Tuesday_NIU3_high_Yellow5 <- locomotionFMPtruth(FMPTuesdayNIU3, eventIntensity = "high", eventType = "Dynamic",
                                                athleteID = "Yellow-5", groupID = "TuesdayNIU3",
                                                startTime = as.numeric(lubridate::as_datetime("2022-02-15 10:59:47 UTC")),
                                                endTime = as.numeric(lubridate::as_datetime("2022-02-15 11:00:19 UTC")))

Tuesday_NIU3_high_Yellow6 <- locomotionFMPtruth(FMPTuesdayNIU3, eventIntensity = "high", eventType = "Dynamic",
                                                athleteID = "Yellow-6", groupID = "TuesdayNIU3",
                                                startTime = as.numeric(lubridate::as_datetime("2022-02-15 11:00:03 UTC")),
                                                endTime = as.numeric(lubridate::as_datetime("2022-02-15 11:00:33 UTC")))



Tuesday_NIU3_highDynamic <- rbind(Tuesday_NIU3_high_Orange2, 
                                  Tuesday_NIU3_high_Orange4, 
                                  Tuesday_NIU3_high_Orange5,
                                  Tuesday_NIU3_high_Yellow2, 
                                  Tuesday_NIU3_high_Yellow3, 
                                  Tuesday_NIU3_high_Yellow5, 
                                  Tuesday_NIU3_high_Yellow6)


## -- Insert truth for TuesdayNIU3 high half dynamic -- ##


Tuesday_NIU3_highHalf_Orange2 <- locomotionFMPtruth(FMPTuesdayNIU3, eventIntensity = "highhalf", eventType = "Dynamic",
                                                    athleteID = "Orange-2", groupID = "TuesdayNIU3", 
                                                    startTime = as.numeric(lubridate::as_datetime("2022-02-15 11:06:14 UTC")),
                                                    endTime = as.numeric(lubridate::as_datetime("2022-02-15 11:06:32 UTC")))

Tuesday_NIU3_highHalf_Orange4 <- locomotionFMPtruth(FMPTuesdayNIU3, eventIntensity = "highhalf", eventType = "Dynamic",
                                                    athleteID = "Orange-4", groupID = "TuesdayNIU3", 
                                                    startTime = as.numeric(lubridate::as_datetime("2022-02-15 11:06:52 UTC")),
                                                    endTime = as.numeric(lubridate::as_datetime("2022-02-15 11:07:07 UTC")))

Tuesday_NIU3_highHalf_Orange5 <- locomotionFMPtruth(FMPTuesdayNIU3, eventIntensity = "highhalf", eventType = "Dynamic",
                                                    athleteID = "Orange-5", groupID = "TuesdayNIU3", 
                                                    startTime = as.numeric(lubridate::as_datetime("2022-02-15 11:07:23 UTC")),
                                                    endTime = as.numeric(lubridate::as_datetime("2022-02-15 11:07:39 UTC")))

Tuesday_NIU3_highHalf_Yellow2 <- locomotionFMPtruth(FMPTuesdayNIU3, eventIntensity = "highhalf", eventType = "Dynamic",
                                                    athleteID = "Yellow-2", groupID = "TuesdayNIU3", 
                                                    startTime = as.numeric(lubridate::as_datetime("2022-02-15 11:07:51 UTC")),
                                                    endTime = as.numeric(lubridate::as_datetime("2022-02-15 11:08:07 UTC")))

Tuesday_NIU3_highHalf_Yellow3 <- locomotionFMPtruth(FMPTuesdayNIU3, eventIntensity = "highhalf", eventType = "Dynamic",
                                                    athleteID = "Yellow-3", groupID = "TuesdayNIU3",
                                                    startTime = as.numeric(lubridate::as_datetime("2022-02-15 11:08:25 UTC")),
                                                    endTime = as.numeric(lubridate::as_datetime("2022-02-15 11:08:42 UTC")))

Tuesday_NIU3_highHalf_Yellow5 <- locomotionFMPtruth(FMPTuesdayNIU3, eventIntensity = "highhalf", eventType = "Dynamic",
                                                    athleteID = "Yellow-5", groupID = "TuesdayNIU3",
                                                    startTime = as.numeric(lubridate::as_datetime("2022-02-15 11:09:05 UTC")),
                                                    endTime = as.numeric(lubridate::as_datetime("2022-02-15 11:09:19 UTC")))

Tuesday_NIU3_highHalf_Yellow6 <- locomotionFMPtruth(FMPTuesdayNIU3, eventIntensity = "highhalf", eventType = "Dynamic",
                                                    athleteID = "Yellow-6", groupID = "TuesdayNIU3",
                                                    startTime = as.numeric(lubridate::as_datetime("2022-02-15 11:09:41 UTC")),
                                                    endTime = as.numeric(lubridate::as_datetime("2022-02-15 11:09:56 UTC")))



Tuesday_NIU3_highHalf <- rbind(Tuesday_NIU3_highHalf_Orange2, 
                               Tuesday_NIU3_highHalf_Orange4, 
                               Tuesday_NIU3_highHalf_Orange5,
                               Tuesday_NIU3_highHalf_Yellow2, 
                               Tuesday_NIU3_highHalf_Yellow3, 
                               Tuesday_NIU3_highHalf_Yellow5, 
                               Tuesday_NIU3_highHalf_Yellow6)


## -- Insert truth for wednesdayNIU3 Run 10m/3sec -- ##

Wednesday_NIU1_Run10_1 <- locomotionFMPtruth(FMPWednesdayNIU1, eventIntensity = "medium", eventType = "Running", groupID = "WednesdayNIU1",
                                             startTime = as.numeric(lubridate::as_datetime("2022-02-16 08:32:34 UTC")),
                                             endTime = as.numeric(lubridate::as_datetime("2022-02-16 08:32:46 UTC")))

Wednesday_NIU1_Run10_2  <- locomotionFMPtruth(FMPWednesdayNIU1, eventIntensity = "medium", eventType = "Running", groupID = "WednesdayNIU1",
                                              startTime = as.numeric(lubridate::as_datetime("2022-02-16 08:33:04 UTC")),
                                              endTime = as.numeric(lubridate::as_datetime("2022-02-16 08:33:16 UTC")))

Wednesday_NIU1_Run10_3  <- locomotionFMPtruth(FMPWednesdayNIU1, eventIntensity = "medium", eventType = "Running", groupID = "WednesdayNIU1",
                                              startTime = as.numeric(lubridate::as_datetime("2022-02-16 08:33:32 UTC")),
                                              endTime = as.numeric(lubridate::as_datetime("2022-02-16 08:33:44 UTC")))

Wednesday_NIU1_Run10_4  <- locomotionFMPtruth(FMPWednesdayNIU1, eventIntensity = "medium", eventType = "Running", groupID = "WednesdayNIU1",
                                              startTime = as.numeric(lubridate::as_datetime("2022-02-16 08:33:58 UTC")),
                                              endTime = as.numeric(lubridate::as_datetime("2022-02-16 08:34:10 UTC")))


Wednesday_NIU1_Run10 <- rbind(Wednesday_NIU1_Run10_1,
                              Wednesday_NIU1_Run10_2,
                              Wednesday_NIU1_Run10_3,
                              Wednesday_NIU1_Run10_4)



## -- Insert truth for wednesdayNIU3 Run 15m/3sec -- ##

Wednesday_NIU1_Run15_1 <- locomotionFMPtruth(FMPWednesdayNIU1, eventIntensity = "high", eventType = "Running", groupID = "WednesdayNIU1",
                                             startTime = as.numeric(lubridate::as_datetime("2022-02-16 08:40:30 UTC")),
                                             endTime = as.numeric(lubridate::as_datetime("2022-02-16 08:40:36 UTC")))

Wednesday_NIU1_Run15_2  <- locomotionFMPtruth(FMPWednesdayNIU1, eventIntensity = "high", eventType = "Running", groupID = "WednesdayNIU1",
                                              startTime = as.numeric(lubridate::as_datetime("2022-02-16 08:40:54 UTC")),
                                              endTime = as.numeric(lubridate::as_datetime("2022-02-16 08:41:00 UTC")))

Wednesday_NIU1_Run15_3  <- locomotionFMPtruth(FMPWednesdayNIU1, eventIntensity = "high", eventType = "Running", groupID = "WednesdayNIU1",
                                              startTime = as.numeric(lubridate::as_datetime("2022-02-16 08:41:21 UTC")),
                                              endTime = as.numeric(lubridate::as_datetime("2022-02-16 08:41:27 UTC")))

Wednesday_NIU1_Run15_4  <- locomotionFMPtruth(FMPWednesdayNIU1, eventIntensity = "high", eventType = "Running", groupID = "WednesdayNIU1",
                                              startTime = as.numeric(lubridate::as_datetime("2022-02-16 08:41:45 UTC")),
                                              endTime = as.numeric(lubridate::as_datetime("2022-02-16 08:41:51 UTC")))


Wednesday_NIU1_Run15 <- rbind(Wednesday_NIU1_Run15_1,
                              Wednesday_NIU1_Run15_2,
                              Wednesday_NIU1_Run15_3,
                              Wednesday_NIU1_Run15_4)


## -- Insert truth for wednesdayNIU3 Run 16.5m/3sec -- ##

Wednesday_NIU1_Run16_1 <- locomotionFMPtruth(FMPWednesdayNIU1, eventIntensity = "high", eventType = "Running", groupID = "WednesdayNIU1",
                                             startTime = as.numeric(lubridate::as_datetime("2022-02-16 08:47:09 UTC")),
                                             endTime = as.numeric(lubridate::as_datetime("2022-02-16 08:47:15 UTC")))

Wednesday_NIU1_Run16_2  <- locomotionFMPtruth(FMPWednesdayNIU1, eventIntensity = "high", eventType = "Running", groupID = "WednesdayNIU1",
                                              startTime = as.numeric(lubridate::as_datetime("2022-02-16 08:47:34 UTC")),
                                              endTime = as.numeric(lubridate::as_datetime("2022-02-16 08:47:40 UTC")))

Wednesday_NIU1_Run16_3  <- locomotionFMPtruth(FMPWednesdayNIU1, eventIntensity = "high", eventType = "Running", groupID = "WednesdayNIU1",
                                              startTime = as.numeric(lubridate::as_datetime("2022-02-16 08:48:01 UTC")),
                                              endTime = as.numeric(lubridate::as_datetime("2022-02-16 08:48:07 UTC")))

Wednesday_NIU1_Run16_4  <- locomotionFMPtruth(FMPWednesdayNIU1, eventIntensity = "high", eventType = "Running", groupID = "WednesdayNIU1",
                                              startTime = as.numeric(lubridate::as_datetime("2022-02-16 08:48:28 UTC")),
                                              endTime = as.numeric(lubridate::as_datetime("2022-02-16 08:48:34 UTC")))


Wednesday_NIU1_Run16 <- rbind(Wednesday_NIU1_Run16_1,
                              Wednesday_NIU1_Run16_2,
                              Wednesday_NIU1_Run16_3,
                              Wednesday_NIU1_Run16_4)


## -- Insert truth for wednesdayNIU3 Run 18m/3sec -- ##

Wednesday_NIU1_Run18_1 <- locomotionFMPtruth(FMPWednesdayNIU1, eventIntensity = "high", eventType = "Running", groupID = "WednesdayNIU1",
                                             startTime = as.numeric(lubridate::as_datetime("2022-02-16 08:54:27 UTC")),
                                             endTime = as.numeric(lubridate::as_datetime("2022-02-16 08:54:33 UTC")))

Wednesday_NIU1_Run18_2  <- locomotionFMPtruth(FMPWednesdayNIU1, eventIntensity = "high", eventType = "Running", groupID = "WednesdayNIU1",
                                              startTime = as.numeric(lubridate::as_datetime("2022-02-16 08:54:58 UTC")),
                                              endTime = as.numeric(lubridate::as_datetime("2022-02-16 08:55:04 UTC")))

Wednesday_NIU1_Run18_3  <- locomotionFMPtruth(FMPWednesdayNIU1, eventIntensity = "high", eventType = "Running", groupID = "WednesdayNIU1",
                                              startTime = as.numeric(lubridate::as_datetime("2022-02-16 08:55:30 UTC")),
                                              endTime = as.numeric(lubridate::as_datetime("2022-02-16 08:55:36 UTC")))

Wednesday_NIU1_Run18_4  <- locomotionFMPtruth(FMPWednesdayNIU1, eventIntensity = "high", eventType = "Running", groupID = "WednesdayNIU1",
                                              startTime = as.numeric(lubridate::as_datetime("2022-02-16 08:56:02 UTC")),
                                              endTime = as.numeric(lubridate::as_datetime("2022-02-16 08:56:08 UTC")))


Wednesday_NIU1_Run18 <- rbind(Wednesday_NIU1_Run18_1,
                              Wednesday_NIU1_Run18_2,
                              Wednesday_NIU1_Run18_3,
                              Wednesday_NIU1_Run18_4)


## -- Insert truth for ThursdayNIU2 Run 15m/3sec -- ##

Thursday_NIU2_Run15_1 <- locomotionFMPtruth(FMPThursdayNIU2, eventIntensity = "high", eventType = "Running", groupID = "ThursdayNIU2",
                                            startTime = as.numeric(lubridate::as_datetime("2022-02-17 08:26:07 UTC")),
                                            endTime = as.numeric(lubridate::as_datetime("2022-02-17 08:26:13 UTC")))

Thursday_NIU2_Run15_2  <- locomotionFMPtruth(FMPThursdayNIU2, eventIntensity = "high", eventType = "Running", groupID = "ThursdayNIU2",
                                             startTime = as.numeric(lubridate::as_datetime("2022-02-17 08:26:35 UTC")),
                                             endTime = as.numeric(lubridate::as_datetime("2022-02-17 08:26:41 UTC")))

Thursday_NIU2_Run15_3  <- locomotionFMPtruth(FMPThursdayNIU2, eventIntensity = "high", eventType = "Running", groupID = "ThursdayNIU2",
                                             startTime = as.numeric(lubridate::as_datetime("2022-02-17 08:27:01 UTC")),
                                             endTime = as.numeric(lubridate::as_datetime("2022-02-17 08:27:07 UTC")))

Thursday_NIU2_Run15 <- rbind(Thursday_NIU2_Run15_1,
                             Thursday_NIU2_Run15_2,
                             Thursday_NIU2_Run15_3)



## -- Insert truth for ThursdayNIU2 Run 16.5m/3sec -- ##

Thursday_NIU2_Run16_1 <- locomotionFMPtruth(FMPThursdayNIU2, eventIntensity = "high", eventType = "Running", groupID = "ThursdayNIU2",
                                            startTime = as.numeric(lubridate::as_datetime("2022-02-17 08:29:39 UTC")),
                                            endTime = as.numeric(lubridate::as_datetime("2022-02-17 08:29:45 UTC")))

Thursday_NIU2_Run16_2  <- locomotionFMPtruth(FMPThursdayNIU2, eventIntensity = "high", eventType = "Running", groupID = "ThursdayNIU2",
                                             startTime = as.numeric(lubridate::as_datetime("2022-02-17 08:30:18 UTC")),
                                             endTime = as.numeric(lubridate::as_datetime("2022-02-17 08:30:24 UTC")))

Thursday_NIU2_Run16_3  <- locomotionFMPtruth(FMPThursdayNIU2, eventIntensity = "high", eventType = "Running", groupID = "ThursdayNIU2",
                                             startTime = as.numeric(lubridate::as_datetime("2022-02-17 08:30:52 UTC")),
                                             endTime = as.numeric(lubridate::as_datetime("2022-02-17 08:30:58 UTC")))

Thursday_NIU2_Run16 <- rbind(Thursday_NIU2_Run16_1,
                             Thursday_NIU2_Run16_2,
                             Thursday_NIU2_Run16_3)



## -- Insert truth for ThursdayNIU2 Run 18m/3sec -- ##

Thursday_NIU2_Run18_1 <- locomotionFMPtruth(FMPThursdayNIU2, eventIntensity = "high", eventType = "Running", groupID = "ThursdayNIU2",
                                            startTime = as.numeric(lubridate::as_datetime("2022-02-17 08:32:55 UTC")),
                                            endTime = as.numeric(lubridate::as_datetime("2022-02-17 08:33:01 UTC")))

Thursday_NIU2_Run18_2  <- locomotionFMPtruth(FMPThursdayNIU2, eventIntensity = "high", eventType = "Running", groupID = "ThursdayNIU2",
                                             startTime = as.numeric(lubridate::as_datetime("2022-02-17 08:33:31 UTC")),
                                             endTime = as.numeric(lubridate::as_datetime("2022-02-17 08:33:37 UTC")))

Thursday_NIU2_Run18_3  <- locomotionFMPtruth(FMPThursdayNIU2, eventIntensity = "high", eventType = "Running", groupID = "ThursdayNIU2",
                                             startTime = as.numeric(lubridate::as_datetime("2022-02-17 08:34:07 UTC")),
                                             endTime = as.numeric(lubridate::as_datetime("2022-02-17 08:34:13 UTC")))

Thursday_NIU2_Run18 <- rbind(Thursday_NIU2_Run18_1,
                             Thursday_NIU2_Run18_2,
                             Thursday_NIU2_Run18_3)


## -- Insert truth for ThursdayNIU3 Run 15m/3sec -- ##

Thursday_NIU3_Run15_1 <- locomotionFMPtruth(FMPThursdayNIU3, eventIntensity = "high", eventType = "Running", groupID = "ThursdayNIU3",
                                            startTime = as.numeric(lubridate::as_datetime("2022-02-17 11:10:16 UTC")),
                                            endTime = as.numeric(lubridate::as_datetime("2022-02-17 11:10:22 UTC")))

Thursday_NIU3_Run15_2  <- locomotionFMPtruth(FMPThursdayNIU3, eventIntensity = "high", eventType = "Running", groupID = "ThursdayNIU3",
                                             startTime = as.numeric(lubridate::as_datetime("2022-02-17 11:10:49 UTC")),
                                             endTime = as.numeric(lubridate::as_datetime("2022-02-17 11:10:55 UTC")))

Thursday_NIU3_Run15_3  <- locomotionFMPtruth(FMPThursdayNIU3, eventIntensity = "high", eventType = "Running", groupID = "ThursdayNIU3",
                                             startTime = as.numeric(lubridate::as_datetime("2022-02-17 11:11:18 UTC")),
                                             endTime = as.numeric(lubridate::as_datetime("2022-02-17 11:11:24 UTC")))


Thursday_NIU3_Run15 <- rbind(Thursday_NIU3_Run15_1,
                             Thursday_NIU3_Run15_2,
                             Thursday_NIU3_Run15_3)


## -- Insert truth for ThursdayNIU3 Run 16.5m/3sec -- ##

Thursday_NIU3_Run16_1 <- locomotionFMPtruth(FMPThursdayNIU3, eventIntensity = "high", eventType = "Running", groupID = "ThursdayNIU3",
                                            startTime = as.numeric(lubridate::as_datetime("2022-02-17 11:13:48 UTC")),
                                            endTime = as.numeric(lubridate::as_datetime("2022-02-17 11:13:54 UTC")))

Thursday_NIU3_Run16_2  <- locomotionFMPtruth(FMPThursdayNIU3, eventIntensity = "high", eventType = "Running", groupID = "ThursdayNIU3",
                                             startTime = as.numeric(lubridate::as_datetime("2022-02-17 11:14:25 UTC")),
                                             endTime = as.numeric(lubridate::as_datetime("2022-02-17 11:14:31 UTC")))

Thursday_NIU3_Run16_3  <- locomotionFMPtruth(FMPThursdayNIU3, eventIntensity = "high", eventType = "Running", groupID = "ThursdayNIU3",
                                             startTime = as.numeric(lubridate::as_datetime("2022-02-17 11:15:10 UTC")),
                                             endTime = as.numeric(lubridate::as_datetime("2022-02-17 11:15:16 UTC")))


Thursday_NIU3_Run16 <- rbind(Thursday_NIU3_Run16_1,
                             Thursday_NIU3_Run16_2,
                             Thursday_NIU3_Run16_3)


## -- Insert truth for ThursdayNIU3 Run 18m/3sec -- ##

Thursday_NIU3_Run18_1 <- locomotionFMPtruth(FMPThursdayNIU3, eventIntensity = "high", eventType = "Running", groupID = "ThursdayNIU3",
                                            startTime = as.numeric(lubridate::as_datetime("2022-02-17 11:17:31 UTC")),
                                            endTime = as.numeric(lubridate::as_datetime("2022-02-17 11:17:37 UTC")))

Thursday_NIU3_Run18_2  <- locomotionFMPtruth(FMPThursdayNIU3, eventIntensity = "high", eventType = "Running", groupID = "ThursdayNIU3",
                                             startTime = as.numeric(lubridate::as_datetime("2022-02-17 11:18:10 UTC")),
                                             endTime = as.numeric(lubridate::as_datetime("2022-02-17 11:18:16 UTC")))

Thursday_NIU3_Run18_3  <- locomotionFMPtruth(FMPThursdayNIU3, eventIntensity = "high", eventType = "Running", groupID = "ThursdayNIU3",
                                             startTime = as.numeric(lubridate::as_datetime("2022-02-17 11:18:53 UTC")),
                                             endTime = as.numeric(lubridate::as_datetime("2022-02-17 11:18:59 UTC")))


Thursday_NIU3_Run18 <- rbind(Thursday_NIU3_Run18_1,
                             Thursday_NIU3_Run18_2,
                             Thursday_NIU3_Run18_3)



## -- All locomotions collected -- ##

#binding all the respective locomotion types across data collections, to calculate the perc distribution
# and the percentage agreement test.
Walk <- rbind(Tuesday_NIU2_Walk,
              Tuesday_NIU3_Walk)

Walk_perc <- Walk %>% 
  count(locomotion) %>% 
  mutate(perc = n/sum(n)*100)

WalkAgree <- Walk %>% 
  select(truth, locomotion) %>% 
  irr::agree(.)

Run10 <- rbind(Tuesday_NIU2_Run10,
               Tuesday_NIU3_Run10,
               Wednesday_NIU1_Run10)

Run10_perc <- Run10 %>% 
  count(locomotion) %>% 
  mutate(perc = n/sum(n)*100)

Run10Agree <- Run10 %>% 
  select(truth, locomotion) %>% 
  irr::agree(.)

Run15 <- rbind(Tuesday_NIU2_Run15,
               Tuesday_NIU3_Run15,
               Wednesday_NIU1_Run15,
               Thursday_NIU2_Run15,
               Thursday_NIU3_Run15)

Run15_perc <- Run15 %>% 
  count(locomotion) %>% 
  mutate(perc = n/sum(n)*100)

Run15Agree <- Run15 %>% 
  select(truth, locomotion) %>% 
  irr::agree(.)

Run16 <- rbind(Wednesday_NIU1_Run16,
               Thursday_NIU2_Run16,
               Thursday_NIU3_Run16)

Run16_perc <- Run16 %>% 
  count(locomotion) %>% 
  mutate(perc = n/sum(n)*100)

Run16Agree <- Run16 %>% 
  select(truth, locomotion) %>% 
  irr::agree(.)

Run18 <- rbind(Wednesday_NIU1_Run18,
               Thursday_NIU2_Run18,
               Thursday_NIU3_Run18)

Run18_perc <- Run18 %>% 
  count(locomotion) %>% 
  mutate(perc = n/sum(n)*100)

Run18Agree <- Run18 %>% 
  select(truth, locomotion) %>% 
  irr::agree(.)

mediumDynamic <- rbind(Tuesday_NIU2_mediumDynamic,
                       Tuesday_NIU3_mediumDynamic,
                       Tuesday_NIU3_mediumDynamic2)

mediumDynamic_perc <- mediumDynamic %>% 
  count(locomotion) %>% 
  mutate(perc = n/sum(n)*100)

mediumDynamicAgree <- mediumDynamic %>% 
  select(truth, locomotion) %>% 
  irr::agree(.)

highDynamic <- rbind(Tuesday_NIU2_highDynamic,
                     Tuesday_NIU3_highDynamic)

highDynamic_perc <- highDynamic %>% 
  count(locomotion) %>% 
  mutate(perc = n/sum(n)*100)

highDynamicAgree <- highDynamic %>% 
  select(truth, locomotion) %>% 
  irr::agree(.)

highhalfDynamic <- rbind(Tuesday_NIU2_highHalf,
                         Tuesday_NIU3_highHalf)

highhalfDynamic_perc <- highhalfDynamic %>% 
  count(locomotion) %>% 
  mutate(perc = n/sum(n)*100)

highhalfAgree <- highhalfDynamic %>% 
  select(truth, locomotion) %>% 
  irr::agree(.)


## -- Overall agreeemnt accuracy -- ##
totalAgreement <- 
  data.frame(
    event = c("Slow","Linear","Linear","Linear","Linear","Non-linear","Non-linear","Non-linear"),
    velocity = c("5.4","12","18","19.6","21.6","3 sec per cone","All out (full circle)","All out (half circle)"),
    total_time = c(WalkAgree$subjects, Run10Agree$subjects, Run15Agree$subjects,
                   Run16Agree$subjects, Run18Agree$subjects, mediumDynamicAgree$subjects,
                   highDynamicAgree$subjects, highhalfAgree$subjects),
    agreement = c(WalkAgree$value, Run10Agree$value, Run15Agree$value,
                  Run16Agree$value, Run18Agree$value, mediumDynamicAgree$value,
                  highDynamicAgree$value, highhalfAgree$value)
  )

#data.table::fwrite(totalAgreement, "Data/ProcessedData/totalAgreement.csv")

## -- Prepare data for a scatter pie plot -- ## 

#Preparing the data to create a scatter pie plot
#The drill will be used for the x-axis of the plot
Tuesday_NIU2_Walk <- Tuesday_NIU2_Walk %>% mutate(drill = "Slow")
Tuesday_NIU2_Run10 <- Tuesday_NIU2_Run10 %>% mutate(drill = "Linear10")
Tuesday_NIU2_Run15 <- Tuesday_NIU2_Run15 %>% mutate(drill = "Linear15")
Thursday_NIU2_Run15 <- Thursday_NIU2_Run15 %>% mutate(drill = "Linear15")
Thursday_NIU2_Run16 <- Thursday_NIU2_Run16 %>% mutate(drill = "Linear16")
Thursday_NIU2_Run18 <- Thursday_NIU2_Run18 %>% mutate(drill = "Linear18") 
Tuesday_NIU2_mediumDynamic <- Tuesday_NIU2_mediumDynamic %>% mutate(drill = "NonLinearSlow")
Tuesday_NIU2_highDynamic <- Tuesday_NIU2_highDynamic %>% mutate(drill = "NonLinearFast")
Tuesday_NIU2_highHalf <- Tuesday_NIU2_highHalf %>% mutate(drill = "NonLinearHalf")

Tuesday_NIU3_Walk <- Tuesday_NIU3_Walk %>% mutate(drill = "Slow")
Tuesday_NIU3_Run10 <- Tuesday_NIU3_Run10 %>% mutate(drill = "Linear10")
Tuesday_NIU3_Run15 <- Tuesday_NIU3_Run15 %>% mutate(drill = "Linear15")
Thursday_NIU3_Run15 <- Thursday_NIU3_Run15 %>% mutate(drill = "Linear15")
Thursday_NIU3_Run16 <- Thursday_NIU3_Run16 %>% mutate(drill = "Linear16")
Thursday_NIU3_Run18 <- Thursday_NIU3_Run18 %>% mutate(drill = "Linear18") 
Tuesday_NIU3_mediumDynamic <- Tuesday_NIU3_mediumDynamic %>% mutate(drill = "NonLinearSlow")
Tuesday_NIU3_highDynamic <- Tuesday_NIU3_highDynamic %>% mutate(drill = "NonLinearFast")
Tuesday_NIU3_highHalf <- Tuesday_NIU3_highHalf %>% mutate(drill = "NonLinearHalf")

Wednesday_NIU1_Run10 <- Wednesday_NIU1_Run10 %>% mutate(drill = "Linear10")
Wednesday_NIU1_Run15 <- Wednesday_NIU1_Run15 %>% mutate(drill = "Linear15")
Wednesday_NIU1_Run16 <- Wednesday_NIU1_Run16 %>% mutate(drill = "Linear16")
Wednesday_NIU1_Run18 <- Wednesday_NIU1_Run18 %>% mutate(drill = "Linear18")

controlledDrills <- rbind(Tuesday_NIU2_Walk, Tuesday_NIU2_Run10, Tuesday_NIU2_Run15, Thursday_NIU2_Run15, 
                          Thursday_NIU2_Run16, Thursday_NIU2_Run18, Tuesday_NIU2_mediumDynamic, Tuesday_NIU2_highDynamic,
                          Tuesday_NIU2_highHalf, Tuesday_NIU3_Walk, Tuesday_NIU3_Run10, Tuesday_NIU3_Run15, 
                          Thursday_NIU3_Run15, Thursday_NIU3_Run16, Thursday_NIU3_Run18, Tuesday_NIU3_mediumDynamic,
                          Tuesday_NIU3_highDynamic, Tuesday_NIU3_highHalf, Wednesday_NIU1_Run10, Wednesday_NIU1_Run15,
                          Wednesday_NIU1_Run16, Wednesday_NIU1_Run18)


#Calculating the relative distribution of prediction for each drill for each athlete.
#Additionally creating an x variable of random numbers between two values, to create some jitter
#to seperate each pieplot on the x-axis. 
set.seed(123)
plotData <- controlledDrills %>% 
  group_by(athlete, drill) %>% 
  count(locomotion) %>% 
  mutate(percen = (n / sum(n)*100)) %>% 
  ungroup() %>% 
  pivot_wider(id_cols = c(athlete, drill), names_from = locomotion, values_from = percen, values_fill = 0L) %>%
  mutate(
    #Creating random yitter for the x axis relative to the drill of interest so it doesn't overlap 
    #in the scatterpie plot
    x = case_when(
      drill == "Slow" ~ runif(nrow(.), 0, 1),
      drill == "Linear10" ~ runif(nrow(.), 1.1, 2),
      drill == "Linear15" ~ runif(nrow(.), 2.1, 3),
      drill == "Linear16" ~ runif(nrow(.), 3.1, 4),
      drill == "Linear18" ~ runif(nrow(.), 4.1, 5),
      drill == "NonLinearSlow" ~ runif(nrow(.), 5.1, 6),
      drill == "NonLinearFast" ~ runif(nrow(.), 6.1, 7),
      drill == "NonLinearHalf" ~ runif(nrow(.), 7.1, 8)
    )) %>% 
  group_by(athlete) %>% 
  mutate(y = case_when(
    drill == "Slow" ~ Walking/100,
    drill == "Linear10" ~ Running/100,
    drill == "Linear15" ~ Running/100,
    drill == "Linear16" ~ Running/100,
    drill == "Linear18" ~ Running/100,
    drill == "NonLinearSlow" ~ Dynamic/100,
    drill == "NonLinearFast" ~ Dynamic/100,
    drill == "NonLinearHalf" ~ Dynamic/100)) %>% 
  ungroup() %>% 
  janitor::clean_names()


ggthemr::ggthemr("greyscale")
textSize = 8
ggplot() +
  scatterpie::geom_scatterpie(aes(x = x, y = y, group = athlete), data = plotData, alpha = 0.9,
                              cols = c("dynamic","running","walking","no_locomotion"), 
                              pie_scale = 0.5) +
  geom_vline(xintercept = c(0,1,2,3,4,5,6,7,8), linetype = "dashed", color = "#838383", linewidth = 2, alpha = 0.75) + 
  annotate("text", x = 0.5, y = 0, label = "5.4 km/h", size = textSize) +
  annotate("text", x = 1.5, y = 0, label = "12 km/h", size = textSize) +
  annotate("text", x = 2.5, y = 0, label = "18 km/h", size = textSize) +
  annotate("text", x = 3.5, y = 0, label = "19.8 km/h", size = textSize) +
  annotate("text", x = 4.5, y = 0, label = "21.6 km/h", size = textSize) +
  annotate("text", x = 5.5, y = 0, label = "3 sec per cone", size = textSize) +
  annotate("text", x = 6.5, y = 0, label = "All out", size = textSize) +
  annotate("text", x = 7.5, y = 0, label = "All out - Half", size = textSize) +
  coord_equal(ratio = 1.75) + 
  scale_fill_manual(name = "Locomotion type",
                    values=c("no_locomotion"="#313131", "walking"="#0f3698", 
                             "dynamic"="#94281b", "running"="#3c8001"),
                    labels = c("No Locomotion", "Slow Locomotion", "Non-linear Locomotion", "Linear Locomotion")) +
  scale_y_continuous(breaks = seq(0,100,20) / 100,
                     labels = c("0","20","40","60","80","100")) +
  theme(legend.position = "top",
        text = element_text(size = 33),
        axis.text.x = element_blank(),
        axis.ticks.x = element_blank()) + 
  xlab("Controlled Drill") + 
  ylab("Correct prediction [%]")

#ggsave("scatterPiePlot.png", width = 25, height = 15, dpi = 500)



