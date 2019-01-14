# Initialize Session
cat("\014")
rm(list=ls()) 

# Installs libraries 
library(tidyverse)
library(lubridate)
library(epanetReader)
library(epanet2toolkit)

# Read Data Network Base
networks <- read.inp("./data/base_dma_02.inp")

# Calculation and generating Report Base

ENepanet("./data/base_dma_02.inp", "./reports/rep_dma_02.rpt")

#Read Report
report  <- read.rpt("./reports/rep_dma_02.rpt")

## Network Components and Results of the Network BASE

network_components <- tibble( id_model = "M00000",
                              junctions = list(as_tibble(networks$Junctions)),
                              reservoirs = list(as_tibble(networks$Reservoirs)),
                              tanks = list(as_tibble(networks$Tanks)),
                              pipes = list(as_tibble(networks$Pipes)),
                              pumps = list(as_tibble(networks$Pumps)),
                              valves = list(as_tibble(networks$Valves)),
                              coordinates = list(as_tibble(networks$Coordinates)))

network_results    <- tibble( id_model  = "M00000",
                              id_result = "R00000",
                              emitters = list(as_tibble(networks$Emitters)),
                              node_results = list(as_tibble(report$nodeResults)),
                              link_results = list(as_tibble(report$linkResults)))

# Calculation of the pipe length associated each the nodes.

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

# Generate Models with Leacks

nodes_to_analyze <- "^JT_0[A-K]" # RegExp

nodes <- network_components$junctions[[network_components$id_model == "M00000"]]

nodes <- nodes %>% 
         subset(grepl(nodes_to_analyze,ID)) %>%
         select(ID)

nodes <- nodes$ID

# coef ~= y = 0.1436*(l/s) - 0.0026
#   Leack(l/s) = 2.0	then coef =  0.2846

# coefficient <- c(1.0, 2.0, 3.0 )
# coefficient <- coefficient * 0.1436 - 0.0026

coefficient <- 0.2846

# Loop
i <- 0

for (node in nodes) {

  i <- i + 1
  
  id_result <- ifelse ( i < 10,   "R0000", 
               ifelse ( i < 100,  "R000", 
               ifelse ( i < 1000, "R00", 
               ifelse ( i < 10000,"R0", "R"))))
  
  id_result <-  paste0(id_result, i)
     
  networks$Emitters <- data.frame(ID = node, FlowCoef = 0.2846)

  write.inp(networks, "./data/temp_network.inp")

  ENepanet("./data/temp_network.inp", "./reports/temp_report.rpt")

  #Read Report

  report  <- read.rpt("./reports/temp_report.rpt")

  network_results <- network_results %>%
                     add_row( id_model  = "M00000",
                              id_result = id_result,
                              emitters = list(as_tibble(networks$Emitters)),
                              node_results = list(as_tibble(report$nodeResults)),
                              link_results = list(as_tibble(report$linkResults)))
}

# Save the DB of the calculation
saveRDS(network_results, "./data/network_results.rds")

# Restore the DB of the calculation
# network_results <- readRDS("./data/network_results.rds")
