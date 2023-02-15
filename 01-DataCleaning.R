##  ---------------------------
##  01-DataCleaning
##
##  Following script will import the "Football Movement Profile" (FMP) dataset
##  and clean it to be a 1 Hz tabular dataset.
##
##  Author: "Nicki lentz"
##  Date: "15/3/2022"
##
##  Email: nickilentz@hotmail.com
## ---------------------------


library(tidyverse)
source("Scripts/cleanFMPdata.R")
source("Scripts/extractAthleteName.R")

## -- Import FMP data -- ##

# the gsub pattern is looking for any character zero or more times (.*) up until the first space,
# and then capturing the one or more #characters ((.+)) after that first space.
# The ? after .* makes it "lazy" rather than "greedy" and is what makes it stop at the first #space found.
# So, the .*? matches everything before the first space, the space matches the first space found.
# Jota & Wiktor Stribizew https://stackoverflow.com/questions/32767164/use-gsub-remove-all-string-before-first-white-space-in-r

FMPTuesday <- data.table::fread("Data/RawData/FMP_Tuesday.csv") %>%
  janitor::clean_names() %>%
  mutate(athlete = sub(".*? ", "", athlete))

FMPTuesday$movement_type <- factor(FMPTuesday$movement_type, levels = c(
  "Very Low Intensity",
  "Low Intensity",
  "Dynamic Medium Intensity",
  "Dynamic High Intensity",
  "Running Medium Intensity",
  "Running High Intensity"
))

FMPWednesday <- data.table::fread("Data/RawData/FMP_Wednesday.csv") %>%
  janitor::clean_names() %>%
  mutate(
    athlete = sub(".*? ", "", athlete),
    athlete = as.factor(athlete)
  )

FMPWednesday$movement_type <- factor(FMPWednesday$movement_type, levels = c(
  "Very Low Intensity",
  "Low Intensity",
  "Dynamic Medium Intensity",
  "Dynamic High Intensity",
  "Running Medium Intensity",
  "Running High Intensity"
))

FMPThursday <- data.table::fread("Data/RawData/FMP_Thursday.csv") %>%
  janitor::clean_names() %>%
  mutate(
    athlete = sub(".*? ", "", athlete),
    athlete = as.factor(athlete)
  )

FMPThursday$movement_type <- factor(FMPThursday$movement_type, levels = c(
  "Very Low Intensity",
  "Low Intensity",
  "Dynamic Medium Intensity",
  "Dynamic High Intensity",
  "Running Medium Intensity",
  "Running High Intensity"
))


## -- Clean FMP data -- ##
# The following snippet will pad with "very low intensity" when there is a time gab between rows.
# Furthermore it will expand / lengthen the data frame to 1 Hz data set,
# this is done in the 'cleanFMPdata' function.


# The FMP dataset, contains data for all sensors, even though not all was in use.
# This is cleaned in the raw GPS data, wherefore, we extract the athlete name (e.g., Orange-2)
# based on the GPS file name, e.g., '1 Export for NIU2 Orange-2 69210.csv'.
# This is used to sort the FMP data for active and non active sensors.
TuesdayNIU2_names <- extractAthleteName(list.files(path = "Data/RawData/10hz_NIU2_Tuesday"))
TuesdayNIU3_names <- extractAthleteName(list.files(path = "Data/RawData/10hz_NIU3_Tuesday"))
WednesdayNIU1_names <- extractAthleteName(list.files(path = "Data/RawData/10hz_NIU1_Wednesday"))
ThursdayNIU2_names <- extractAthleteName(list.files(path = "Data/RawData/10hz_NIU2_Thursday"))
ThursdayNIU3_names <- extractAthleteName(list.files(path = "Data/RawData/10hz_NIU3_Thursday"))

FMPTuesdayNIU <- tibble()
for (i in TuesdayNIU2_names[TuesdayNIU2_names %in% unique(FMPTuesday$athlete)]) {
  FMPfilt <- cleanFMPdata(FMPTuesday %>% filter(athlete == i)) %>%
    as_tibble()
  FMPTuesdayNIU <- rbind(FMPTuesdayNIU, FMPfilt)
}

FMPTuesdayNIU2 <- FMPTuesdayNIU %>%
  arrange(athlete, unixTime) %>%
  filter(
    unixTime >= as.numeric(lubridate::as_datetime("2022-02-15 08:30:00 UTC")) - 1,
    unixTime <= as.numeric(lubridate::as_datetime("2022-02-15 09:16:01 UTC"))
  )

FMPTuesdayNIU3 <- FMPTuesdayNIU %>%
  arrange(athlete, unixTime) %>%
  filter(
    unixTime >= as.numeric(lubridate::as_datetime("2022-02-15 10:19:00 UTC")) - 1,
    unixTime <= as.numeric(lubridate::as_datetime("2022-02-15 11:15:01 UTC")),
    # filtering out Orange-3 as she left the training early
    athlete != "Orange-3"
  )

FMPWednesdayNIU <- tibble()
for (i in WednesdayNIU1_names[WednesdayNIU1_names %in% unique(FMPWednesday$athlete)]) {
  FMPfilt <- cleanFMPdata(FMPWednesday %>% filter(athlete == i)) %>%
    as_tibble()
  FMPWednesdayNIU <- rbind(FMPWednesdayNIU, FMPfilt)
}

FMPWednesdayNIU1 <- FMPWednesdayNIU %>%
  arrange(athlete, unixTime) %>%
  filter(
    unixTime >= as.numeric(lubridate::as_datetime("2022-02-16 08:30:00 UTC")),
    unixTime <= as.numeric(lubridate::as_datetime("2022-02-16 09:23:01 UTC"))
  )

FMPThursdayNIU <- tibble()
for (i in ThursdayNIU2_names[ThursdayNIU2_names %in% unique(FMPThursday$athlete)]) {
  FMPfilt <- cleanFMPdata(FMPThursday %>% filter(athlete == i)) %>%
    as_tibble()
  FMPThursdayNIU <- rbind(FMPThursdayNIU, FMPfilt)
}

FMPThursdayNIU2 <- FMPThursdayNIU %>%
  arrange(athlete, unixTime) %>%
  filter(
    unixTime >= as.numeric(lubridate::as_datetime("2022-02-17 08:26:00 UTC")),
    unixTime <= as.numeric(lubridate::as_datetime("2022-02-17 09:26:01 UTC")),
    # filtering out orange-6 and orange-10, inactivity.
    athlete %!in% c("Orange-6", "Orange-10")
  )

FMPThursdayNIU3 <- FMPThursdayNIU %>%
  arrange(athlete, unixTime) %>%
  filter(
    unixTime >= as.numeric(lubridate::as_datetime("2022-02-17 10:20:00 UTC")),
    unixTime <= as.numeric(lubridate::as_datetime("2022-02-17 11:20:01 UTC"))
  )


data.table::fwrite(FMPTuesdayNIU2, "Data/ProcessedData/FMPTuesdayNIU2.csv")
data.table::fwrite(FMPTuesdayNIU3, "Data/ProcessedData/FMPTuesdayNIU3.csv")
data.table::fwrite(FMPWednesdayNIU1, "Data/ProcessedData/FMPWednesdayNIU1.csv")
data.table::fwrite(FMPThursdayNIU2, "Data/ProcessedData/FMPThursdayNIU2.csv")
data.table::fwrite(FMPThursdayNIU3, "Data/ProcessedData/FMPThursdayNIU3.csv")
