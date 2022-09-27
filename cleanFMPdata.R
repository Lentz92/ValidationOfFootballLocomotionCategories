#This functions will pad with "very low intensity" when there is a time gab between rows.
#Furthermore it will expand / lengthen the data frame to 1 Hz data set.

cleanFMPdata <- function(data){
  
  
  #Create a index for for easy sorting purposes later on. 
  #Calculate time difference between end_time on row [i] and start_time on row [i+1] in unix time
  tableData <- data %>%  
    select(athlete, start_time, end_time, movement_type, duration) %>% 
    #Adding one hour to get the unix time to the correct time zone
    mutate(start_time = start_time + 3600,
           end_time = end_time + 3600,
           ind = seq_len(nrow(.)),
           timeDifference = floor(lead(start_time, 1) - end_time),
           #Last row won't be able to find a difference, therefore replace NA with 0.
           timeDifference = replace_na(timeDifference, 0)) %>% 
    arrange(start_time)
  
  #Insert Very low Intensity in the gabs where a gab of 1 s or more is found between rows.
  for (i in 1:nrow(tableData)){
    if (tableData[i]$timeDifference >= 1){
      tableData <- rbind(tableData, data.frame(
        athlete = tableData$athlete[i],
        start_time = tableData$start_time[i],
        end_time = tableData$end_time[i],
        movement_type = "Very Low Intensity",
        duration = tableData[i]$timeDifference,
        ind = i+0.1, 
        timeDifference = tableData[i]$timeDifference
      ))
    }
  }
  
  #Create a nested vector with movement type times duration, then unnest the vector
  #to lengthen the dataframe to a 1hz dataset.
  tableData <- tableData %>% 
    arrange(ind) %>% 
    mutate(movement_type = map2(movement_type, duration, ~rep(.x, .y))) %>% 
    unnest_longer(movement_type)
  
  finishedTableData <- tableData %>% 
    mutate(unixTime = seq(
      from = .$start_time[[1]],
      to = .$start_time[[1]] + nrow(tableData)-1,
      by = 1)) %>% 
    select(-c(duration, ind, timeDifference, start_time, end_time))
  
  return(finishedTableData) 
}
