#...............................................................................
# 1. - Initialize Session 
#...............................................................................

cat("\014")
rm(list=ls()) 

#...............................................................................
# 2. - Installs libraries 
#...............................................................................

library(tidyverse)

#...............................................................................
# 3, - Restore the DB of the calculation 
#...............................................................................

network_results <- readRDS("./data/network_results.rds")

#...............................................................................
# 4. Create the Fault Signature Matrix/Vector (FSM)
#...............................................................................

sensor_pressure <- network_results$sensor_pressure[[2]]
sensor_flow     <- network_results$sensor_flow[[2]]

for(i in 3:length(network_results$id_result)) {
  
  sensor_pressure <- rbind( sensor_pressure,
                            network_results$sensor_pressure[[i]])
  
  sensor_flow     <- rbind( sensor_flow,
                            network_results$sensor_flow[[i]])
  
}

sensor_pressure$sensor_type <- "Pressure"
sensor_flow$sensor_type     <- "flow"

sensors_PF <- rbind(sensor_pressure, sensor_flow)

#...............................................................................
remove(i, sensor_pressure, sensor_flow, network_results)
#...............................................................................

sensor_residual     <- sensors_PF %>%
                       select(timeInSeconds, sensor_type, ID, ID_leak, Residual) %>%
                       group_by(timeInSeconds, sensor_type) %>%
                       nest() %>%
                       rename(Residual = data)

sensor_Sensitivity  <- sensors_PF %>%
                       select(timeInSeconds, sensor_type, ID, ID_leak, Sensitivity) %>%
                       group_by(timeInSeconds, sensor_type) %>%
                       nest() %>%
                       rename(Sensitivity = data)

sensors_PF <- full_join(sensor_residual, sensor_Sensitivity, 
                        by = c("timeInSeconds","sensor_type"))


remove(sensor_residual, sensor_Sensitivity)
#...............................................................................

# glimpse(sensors_PF)

# test_01 <- spread( sensors_PF$Sensitivity[[1]], ID_leak, Sensitivity)

