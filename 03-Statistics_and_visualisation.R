##  ---------------------------
##  03-Statistics and visualisation
##
##  Following script will use the cleaned and time mapped data from 02-MappingDrills.R
##  and calculate the percent agreement while also creating visualisations.
##  
##  Author: "Nicki lentz"
##  Date: "15/3/2022"
##
##  Email: nickilentz@hotmail.com
## ---------------------------

library(tidyverse)

df <- readRDS("Data/ProcessedData/mappedData.Rds")

## -- PREPARING DATA -- ## 


controlledDrills <- reduce(df, rbind) %>% 
  #Fixing the naming and order of the factors to make it prettier for the confusion matrices.
  mutate(truth = fct_recode(truth, "Linear running" = "Running", "Non-linear running" = "Dynamic"),
         truth = fct_relevel(truth, c("Non-linear running", "Linear running", "Walking", "No locomotion")),
         locomotion = fct_recode(locomotion, "Linear running" = "Running", "Non-linear running" = "Dynamic"),
         locomotion = fct_relevel(locomotion, c("Non-linear running", "Linear running", "Walking", "No locomotion")))

#Calculating the relative distribution of prediction for each drill for each athlete.
#Additionally creating an x variable of random numbers between two values, to create some jitter
#to seperate each pieplot on the x-axis. 
plotData <- controlledDrills %>%
  group_by(athlete, drill, iteration) %>% 
  count(locomotion) %>% 
  mutate(perc = (n / sum(n)*100)) %>% 
  ungroup() %>% 
  pivot_wider(id_cols = c(athlete, drill, iteration), names_from = locomotion, values_from = perc, values_fill = 0L) %>%
  mutate(
    #Creating random yitter for the x axis relative to the drill of interest so it doesn't overlap 
    #in the scatterpie plot
    x = case_when(
      drill == "Walking" ~ runif(nrow(.), 0, 1),
      drill == "LinearRun10" ~ runif(nrow(.), 1.1, 2),
      drill == "LinearRun15" ~ runif(nrow(.), 2.1, 3),
      drill == "LinearRun16" ~ runif(nrow(.), 3.1, 4),
      drill == "LinearRun18" ~ runif(nrow(.), 4.1, 5),
      drill == "DynamicMedium" ~ runif(nrow(.), 5.1, 6),
      drill == "DynamicHigh" ~ runif(nrow(.), 6.1, 7),
      drill == "DynamicHighHalf" ~ runif(nrow(.), 7.1, 8)
    )) %>% 
  group_by(athlete) %>% 
  mutate(y = case_when(
    drill == "Walking" ~ Walking/100,
    drill == "LinearRun10" ~ `Linear running`/100,
    drill == "LinearRun15" ~ `Linear running`/100,
    drill == "LinearRun16" ~ `Linear running`/100,
    drill == "LinearRun18" ~ `Linear running`/100,
    drill == "DynamicMedium" ~ `Non-linear running`/100,
    drill == "DynamicHigh" ~ `Non-linear running`/100,
    drill == "DynamicHighHalf" ~ `Non-linear running`/100)) %>% 
  ungroup() %>% 
  rename('No Locomotion' = 'No locomotion',
         'Walking' = 'Walking',
         'Linear Running' = `Linear running`,
         'Non-linear Running' = `Non-linear running`)


## -- AGREEMENT -- ## 

#pivot the dataframe to longer format for easier computation
agreement <- plotData %>% 
  pivot_longer(cols = c("Walking","Non-linear Running","Linear Running","No Locomotion")) %>% 
  group_by(drill, name, iteration) %>% 
  summarise(m = mean(value),
            s = sd(value),
            mi = min(value),
            ma = max(value), .groups = "drop_last")

data.table::fwrite(agreement, "Data/ProcessedData/agreement.csv")

agreement %>% 
  filter(drill == "LinearRun18") %>% 
  view()

#Agreement for all iterations



#Finding who has high and low agreement
drills = c("Walking", "LinearRun10", "LinearRun15", "LinearRun16",
           "LinearRun18", "DynamicMedium", "DynamicHigh", "DynamicHighHalf")

high_low_agreement <- list()
i = 1
for (names in drills){
   
  temp <- plotData %>%
    filter(drill == names,
           `Linear Running` >= 95 | `Linear Running` <= 50)
  
  high_low_agreement[[i]] <- temp
  i = i + 1
}


## -- CONFUSION MATRIX -- ## 
source("Scripts/PrettyConfusionMatrix.R")

drill_specific_cm <- function(data, drillType, removeLabels = FALSE){
  tempDrill <- data %>% 
    filter(drill == drillType,
           iteration == 1)
  
  p <- PrettyConfusionMatrix(tempDrill$truth, tempDrill$locomotion)
  if (removeLabels == TRUE){
    p + theme(axis.title=element_blank(),
              axis.text=element_blank(),
              axis.ticks=element_blank(),
              legend.position = "none")
  }
  
  saveName <- paste(drillType, 'confusionMatrix.png', sep='')
  ggsave(saveName, width = 25, height = 15, dpi = 500, bg = 'white')
}

drill_specific_cm(data = controlledDrills, drillType = 'Walking', removeLabels = TRUE)
drill_specific_cm(data = controlledDrills, drillType = 'LinearRun10', removeLabels = TRUE)
drill_specific_cm(data = controlledDrills, drillType = 'LinearRun15', removeLabels = TRUE)
drill_specific_cm(data = controlledDrills, drillType = 'LinearRun16', removeLabels = TRUE)
drill_specific_cm(data = controlledDrills, drillType = 'LinearRun18', removeLabels = TRUE)
drill_specific_cm(data = controlledDrills, drillType = 'DynamicMedium', removeLabels = TRUE)
drill_specific_cm(data = controlledDrills, drillType = 'DynamicHigh', removeLabels = TRUE)
drill_specific_cm(data = controlledDrills, drillType = 'DynamicHighHalf', removeLabels = TRUE)

## -- SCATTER PIEPLOT -- ## 

ggthemr::ggthemr("greyscale")
textSize = 8
ggplot() +
  scatterpie::geom_scatterpie(aes(x = x, y = y, group = athlete), data = plotData, alpha = 0.9,
                              cols = c("Non-linear Running","Linear Running","Walking","No Locomotion"), 
                              pie_scale = 0.5) +
  geom_vline(xintercept = c(0,1,2,3,4,5,6,7,8), linetype = "dashed", color = "#838383", linewidth = 2, alpha = 0.75) + 
  annotate("text", x = 0.5, y = -0.05, label = "5.4 km/h", size = textSize) +
  annotate("text", x = 1.5, y = -0.05, label = "12 km/h", size = textSize) +
  annotate("text", x = 2.5, y = -0.05, label = "18 km/h", size = textSize) +
  annotate("text", x = 3.5, y = -0.05, label = "19.8 km/h", size = textSize) +
  annotate("text", x = 4.5, y = -0.05, label = "21.6 km/h", size = textSize) +
  annotate("text", x = 5.5, y = -0.05, label = "3 sec per cone", size = textSize) +
  annotate("text", x = 6.5, y = -0.05, label = "All out", size = textSize) +
  annotate("text", x = 7.5, y = -0.05, label = "All out - Half", size = textSize) +
  coord_equal(ratio = 1.75) + 
  scale_fill_manual(name = "Locomotion type",
                    values=c("No Locomotion"="#b2df8a", "Walking"="#a6cee3", 
                             "Non-linear Running"="#1f78b4", "Linear Running"="#33a02c"),
                    breaks = c("No Locomotion", "Walking", "Linear Running","Non-linear Running")) + 
  scale_y_continuous(breaks = seq(0,100,20) / 100,
                     labels = c("0","20","40","60","80","100")) +
  theme(legend.position = "top",
        text = element_text(size = 33),
        axis.text.x = element_blank(),
        axis.ticks.x = element_blank()) + 
  xlab("Controlled Drill") + 
  ylab("Correct prediction [%]")

ggsave("scatterPiePlot.png", width = 25, height = 15, dpi = 500)






  
