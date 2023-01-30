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


Tuesday_NIU2_Walk <- rbind(Tuesday_NIU2_Walk2 %>% mutate(iteration = 1),
                           Tuesday_NIU2_Walk3 %>% mutate(iteration = 2),
                           Tuesday_NIU2_Walk4 %>% mutate(iteration = 3),
                           Tuesday_NIU2_Walk5 %>% mutate(iteration = 4))


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


Tuesday_NIU2_Run10 <- rbind(Tuesday_NIU2_Run10_2 %>% mutate(iteration = 1),
                            Tuesday_NIU2_Run10_3 %>% mutate(iteration = 2),
                            Tuesday_NIU2_Run10_4 %>% mutate(iteration = 3),
                            Tuesday_NIU2_Run10_5 %>% mutate(iteration = 4))

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


Tuesday_NIU2_Run15 <- rbind(Tuesday_NIU2_Run15_2 %>% mutate(iteration = 1),
                            Tuesday_NIU2_Run15_3 %>% mutate(iteration = 2),
                            Tuesday_NIU2_Run15_4 %>% mutate(iteration = 3),
                            Tuesday_NIU2_Run15_5 %>% mutate(iteration = 4))

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


Tuesday_NIU3_Walk <- rbind(Tuesday_NIU3_Walk2 %>% mutate(iteration = 1),
                           Tuesday_NIU3_Walk3 %>% mutate(iteration = 2),
                           Tuesday_NIU3_Walk4 %>% mutate(iteration = 3),
                           Tuesday_NIU3_Walk5 %>% mutate(iteration = 4))


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


Tuesday_NIU3_Run10 <- rbind(Tuesday_NIU3_Run10_3 %>% mutate(iteration = 1),
                            Tuesday_NIU3_Run10_4 %>% mutate(iteration = 2),
                            Tuesday_NIU3_Run10_5 %>% mutate(iteration = 3),
                            Tuesday_NIU3_Run10_6 %>% mutate(iteration = 4))


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


Tuesday_NIU3_Run15 <- rbind(Tuesday_NIU3_Run15_2 %>% mutate(iteration = 1),
                            Tuesday_NIU3_Run15_3 %>% mutate(iteration = 2),
                            Tuesday_NIU3_Run15_4 %>% mutate(iteration = 3),
                            Tuesday_NIU3_Run15_5 %>% mutate(iteration = 4))



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


Wednesday_NIU1_Run10 <- rbind(Wednesday_NIU1_Run10_1 %>% mutate(iteration = 1),
                              Wednesday_NIU1_Run10_2 %>% mutate(iteration = 2),
                              Wednesday_NIU1_Run10_3 %>% mutate(iteration = 3),
                              Wednesday_NIU1_Run10_4 %>% mutate(iteration = 4))



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


Wednesday_NIU1_Run15 <- rbind(Wednesday_NIU1_Run15_1 %>% mutate(iteration = 1),
                              Wednesday_NIU1_Run15_2 %>% mutate(iteration = 2),
                              Wednesday_NIU1_Run15_3 %>% mutate(iteration = 3),
                              Wednesday_NIU1_Run15_4 %>% mutate(iteration = 4))


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


Wednesday_NIU1_Run16 <- rbind(Wednesday_NIU1_Run16_1 %>% mutate(iteration = 1),
                              Wednesday_NIU1_Run16_2 %>% mutate(iteration = 2),
                              Wednesday_NIU1_Run16_3 %>% mutate(iteration = 3),
                              Wednesday_NIU1_Run16_4 %>% mutate(iteration = 4))


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


Wednesday_NIU1_Run18 <- rbind(Wednesday_NIU1_Run18_1 %>% mutate(iteration = 1),
                              Wednesday_NIU1_Run18_2 %>% mutate(iteration = 2),
                              Wednesday_NIU1_Run18_3 %>% mutate(iteration = 3),
                              Wednesday_NIU1_Run18_4 %>% mutate(iteration = 4))


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

Thursday_NIU2_Run15 <- rbind(Thursday_NIU2_Run15_1 %>% mutate(iteration = 1),
                             Thursday_NIU2_Run15_2 %>% mutate(iteration = 2),
                             Thursday_NIU2_Run15_3 %>% mutate(iteration = 3))



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

Thursday_NIU2_Run16 <- rbind(Thursday_NIU2_Run16_1 %>% mutate(iteration = 1),
                             Thursday_NIU2_Run16_2 %>% mutate(iteration = 2),
                             Thursday_NIU2_Run16_3 %>% mutate(iteration = 3))



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

Thursday_NIU2_Run18 <- rbind(Thursday_NIU2_Run18_1 %>% mutate(iteration = 1),
                             Thursday_NIU2_Run18_2 %>% mutate(iteration = 2),
                             Thursday_NIU2_Run18_3 %>% mutate(iteration = 3))


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


Thursday_NIU3_Run15 <- rbind(Thursday_NIU3_Run15_1 %>% mutate(iteration = 1),
                             Thursday_NIU3_Run15_2 %>% mutate(iteration = 2),
                             Thursday_NIU3_Run15_3 %>% mutate(iteration = 3))


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


Thursday_NIU3_Run16 <- rbind(Thursday_NIU3_Run16_1 %>% mutate(iteration = 1),
                             Thursday_NIU3_Run16_2 %>% mutate(iteration = 2),
                             Thursday_NIU3_Run16_3 %>% mutate(iteration = 3))


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


Thursday_NIU3_Run18 <- rbind(Thursday_NIU3_Run18_1 %>% mutate(iteration = 1),
                             Thursday_NIU3_Run18_2 %>% mutate(iteration = 2),
                             Thursday_NIU3_Run18_3 %>% mutate(iteration = 3))



Walk <- rbind(Tuesday_NIU2_Walk,
              Tuesday_NIU3_Walk) %>% 
  mutate(drill = "Walking")


Run10 <- rbind(Tuesday_NIU2_Run10,
               Tuesday_NIU3_Run10,
               Wednesday_NIU1_Run10) %>% 
  mutate(drill = "LinearRun10")

Run15 <- rbind(Tuesday_NIU2_Run15,
               Tuesday_NIU3_Run15,
               Wednesday_NIU1_Run15,
               Thursday_NIU2_Run15,
               Thursday_NIU3_Run15) %>% 
  mutate(drill = "LinearRun15")

Run16 <- rbind(Wednesday_NIU1_Run16,
               Thursday_NIU2_Run16,
               Thursday_NIU3_Run16) %>% 
  mutate(drill = "LinearRun16")

Run18 <- rbind(Wednesday_NIU1_Run18,
               Thursday_NIU2_Run18,
               Thursday_NIU3_Run18) %>% 
  mutate(drill = "LinearRun18")

mediumDynamic <- rbind(Tuesday_NIU2_mediumDynamic,
                       Tuesday_NIU3_mediumDynamic,
                       Tuesday_NIU3_mediumDynamic2) %>% 
  mutate(drill = "DynamicMedium",
         iteration = 1)

highDynamic <- rbind(Tuesday_NIU2_highDynamic,
                     Tuesday_NIU3_highDynamic) %>% 
  mutate(drill = "DynamicHigh",
         iteration = 1) 

highhalfDynamic <- rbind(Tuesday_NIU2_highHalf,
                         Tuesday_NIU3_highHalf) %>% 
  mutate(drill = "DynamicHighHalf",
         iteration = 1)

mappedData <- list(Walk, Run10, Run15, Run16, Run18, mediumDynamic, highDynamic, highhalfDynamic)

saveRDS(mappedData, file = "Data/ProcessedData/mappedData.Rds")
