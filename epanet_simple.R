#...............................................................................
# Initialize Session
#...............................................................................

cat("\014")
rm(list=ls()) 

#...............................................................................
# Installs libraries 
#...............................................................................
library(tidyverse)
library(lubridate)
library(epanetReader)
library(epanet2toolkit)

#...............................................................................
# Initialize params
## coefficient ~= y = 0.1436*(l/s) - 0.0026 
##   Leack(l/s) = 2.0	then coef =  0.285
#...............................................................................

model_files <- list( network      = "./data/base_dma_02.inp",
                     temp_network = "./data/temp_network.inp",
                     report       = "./reports/rep_dma_02.rpt",
                     temp_report  = "./reports/temp_report.rpt",
                     rds          = "./data/network_results.rds")

params <- list( id_model  = "M00000",
                id_result = "R00000",
                nodes_to_analyze  = "^JT_0[A-K]", # RegExp
                coefficient = 0.2846) 

#...............................................................................
# Sources
#...............................................................................
source("./func/simple_functions.R")


#...............................................................................
# Read Data Network Base
#...............................................................................
networks <- read.inp(model_files$network)

#...............................................................................
# Calculation and generating Report Base
#...............................................................................
ENepanet(model_files$network, model_files$report)

#...............................................................................
#Read Report
#...............................................................................
report  <- read.rpt(model_files$report)

#...............................................................................
## Generate Network Components and Results of the Network BASE
#...............................................................................

network_components <- tibble( id_model = params$id_model,
                              junctions = list(as_tibble(networks$Junctions)),
                              reservoirs = list(as_tibble(networks$Reservoirs)),
                              tanks = list(as_tibble(networks$Tanks)),
                              pipes = list(as_tibble(networks$Pipes)),
                              pumps = list(as_tibble(networks$Pumps)),
                              valves = list(as_tibble(networks$Valves)),
                              coordinates = list(as_tibble(networks$Coordinates)))

network_results    <- tibble( id_model  = params$id_model,
                              id_result = params$id_result,
                              emitters = list(as_tibble(networks$Emitters)),
                              node_results = list(as_tibble(report$nodeResults)),
                              link_results = list(as_tibble(report$linkResults)),
                              residual_pressure = NA,
                              residual_flow = NA)

remove(report)

#...............................................................................
# Calculation of the pipe length associated each the nodes.
#...............................................................................

node1 <- network_components$pipes[[1]] %>% 
         select(Node1, Node2, Length) %>% 
         group_by(Node1) %>% 
         summarize(sum(Length)) %>% 
         rename(ID = Node1, sum1 = `sum(Length)`)

node2 <- network_components$pipes[[1]] %>% 
         select(Node1, Node2, Length) %>% 
         group_by(Node2) %>% 
         summarize(sum(Length)) %>% 
         rename(ID = Node2, sum2 = `sum(Length)`)

node  <- full_join(node1, node2, by = "ID") %>% 
         replace_na(list(sum1 = 0, sum2 = 0)) %>% 
         mutate(Length = (sum1+sum2)/2) %>%
         select(ID, Length) 

network_components$node_pipe_length <- list(node)

remove(node1, node2, node)

#...............................................................................
# Generate Models with Single-Leacks Scenario assumption !!
#...............................................................................
      

nodes <- network_components$junctions[[network_components$id_model == params$id_model]]
nodes <- nodes %>% subset(grepl(params$nodes_to_analyze,ID)) %>% select(ID)
nodes <- nodes$ID

# Loop
i <- 0

for (node in nodes) {

  i <- i + 1
  
  networks$Emitters <- data.frame(ID = node, FlowCoef = 0.2846)

  write.inp(networks, model_files$temp_network)

  ENepanet(model_files$temp_network, model_files$temp_report)

  #Read Report

  report  <- read.rpt(model_files$temp_report)

  network_results <- network_results %>%
                     add_row( id_model  = params$id_model,
                              id_result = result_id(i),
                              emitters = list(as_tibble(networks$Emitters)),
                              node_results = list(as_tibble(report$nodeResults)),
                              link_results = list(as_tibble(report$linkResults)))
}

remove(i,node, networks, report) # !! REMOVE

#-------------------------------------------------------------------------------
# Save the DB of the calculation
#-------------------------------------------------------------------------------

saveRDS(network_results, model_files$rds)


#-------------------------------------------------------------------------------
# Restore the DB of the calculation
#-------------------------------------------------------------------------------

# network_results <- readRDS(rds_file)

#-------------------------------------------------------------------------------
#  calculation residuals of pressure and flow
#-------------------------------------------------------------------------------

for(i in c(1:length(network_results$id_result))) {
  
  d_pressure <- residual_vector( network_results$node_results[[1]],
                                 network_results$node_results[[i]],
                                 reading = "Pressure")
  
  d_flow    <- residual_vector( network_results$link_results[[1]],
                                network_results$link_results[[i]],
                                reading = "Flow")
  
  network_results$residual_pressure[i] <- list(d_pressure)
  network_results$residual_flow[i]     <- list(d_flow)
  
}

remove(i,d_pressure,d_flow)  # !! REMOVE

#-------------------------------------------------------------------------------
# Save the DB of the calculation
#-------------------------------------------------------------------------------

saveRDS(network_results,model_files$rds)


