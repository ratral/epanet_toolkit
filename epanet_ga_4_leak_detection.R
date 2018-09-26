#...............................................................................
# 1. - Initialize Session                                                  #### 
#...............................................................................

cat("\014")
rm(list=ls()) 

# Installs libraries 
library(tidyverse)
library(dplyr)
library(zoo)
library(lubridate)
library(epanetReader)
library(epanet2toolkit)
library(ggfortify)
library(ggthemes)
library(scales)
library(purrr)
library(visNetwork)

# Initialize params
params <- list(base_network    = "base_dma_01", 
               new_network     = "base_dma_w_leaks",
               functs_name     = "epanet_api_functions",
               inlet_valves    = "PRV_",
               jt_to_analyze   = "^JT_0[A-K]", # RegExp
               pipe_to_analyze = "PS_",        # RegExp
               emitter_coeff   = 10000,
               leak_rate       = 0.01, # Percentage of the network with leaks
               demad_factor    = list( names =c( "wd_spring_summer",
                                                 "hw_spring_summer",
                                                 "wd_summer_break",
                                                 "hw_summer_break",
                                                 "wd_fall_winter",
                                                 "hw_fall_winter"),
                                       factors = c( 0.92, 1.00, 1.09, 
                                                    0.81, 0.66, 0.95)),
               work_folders   = list( dir_work   = getwd(),
                                      dir_report = file.path(getwd(),"reports"),
                                      dir_data   = file.path(getwd(),"data"),  
                                      dir_bin    = file.path(getwd(),"reports"),
                                      dir_func   = file.path(getwd(),"func")))

f_names  <- list( base_file_inp    = file.path(params$work_folders$dir_data, 
                                               paste0(params$base_network,".inp")),
                  base_file_report = file.path(params$work_folders$dir_report, 
                                               paste0(params$base_network,".rpt")),
                  new_file_inp     = file.path(params$work_folders$dir_data, 
                                               paste0(params$new_network,".inp")),
                  new_file_report  = file.path(params$work_folders$dir_report,
                                               paste0(params$new_network,".rpt")),
                  file_func        = file.path(params$work_folders$dir_func, 
                                               paste0(params$functs_name,".R")))

# Load Functions Standard
source(f_names$file_func)


#...............................................................................
# 2. GENERATE THE LEAKAGE IN A NEW NETWORK                                  ####
#...............................................................................

# Read network information from an *.inp

net_input_01  <- read.inp(f_names$base_file_inp)

#if(!file.exists(f_names$new_file_inp)){

   net_input_01 <- gen_network_w_leaks(net_input_01, 
                                       params$leak_rate, 
                                       params$jt_to_analyze )
   
   write.inp(net_input_01, f_names$new_file_inp)

#  }

rm(net_input_01)
#...............................................................................
# 3. Running a Full Simulation                                             ####
#    The function ENepanet() runs a full simulation and 
#    writes the results to a file. 
#...............................................................................

ENepanet(f_names$base_file_inp, f_names$base_file_report)
ENepanet(f_names$new_file_inp,  f_names$new_file_report)

net_input_01  <- read.inp(f_names$new_file_inp)
report_base   <- read.rpt(f_names$base_file_report)
report_leack  <- read.rpt(f_names$new_file_report)

# New Leaks (EMITTERS)

emitters <- as.tibble(net_input_01$Emitters)

# Select nodes

nodes <- eval_nodes (report_base,
                     node_type = "",
                     id_nodes  = "", 
                     group = TRUE, standardize = TRUE) %>%
                     select(ID,p_median) %>%
                     left_join(emitters, by = "ID")

# PIPES AND FLOW

pipes   <- strc_pipes(net_input_01, "", TRUE)

pipes_f_base   <- eval_pipes ( report_base,
                               link_type ="",
                               id_pipes = "", 
                               inlet_links ="",
                               value = "Flow", 
                               group = TRUE, 
                               standardize = FALSE)

pipes_f_leack  <- eval_pipes ( report_leack, 
                               link_type ="",
                               id_pipes = "", 
                               inlet_links ="",
                               value = "Flow", 
                               group = TRUE, 
                               standardize = FALSE)

pipes    <- full_join( pipes, pipes_f_base,   by = "ID")
pipes    <- full_join( pipes, pipes_f_leack,  by = "ID")

rm(pipes_f_base, pipes_f_leack)
rm(report_base, report_leack, net_input_01)


# change from_node and to_node in function of the Flow direction 

#.... New function ??
# ->
df <- pipes

df$from_node[pipes$f_median.y < 0 ]  <-  pipes$to_node  [pipes$f_median.y < 0] 
df$to_node  [pipes$f_median.y < 0 ]  <-  pipes$from_node[pipes$f_median.y < 0] 

df <- df %>% mutate(d_flow = abs(f_median.y - f_median.x)) 

maxs    <- apply(df[4:6], 2, max) 
mins    <- apply(df[4:6], 2, min)

scaled    <- as.data.frame(scale(df[4:6], center = mins, scale = maxs - mins)*100)

names(scaled) <- c("scaled_flow.x","scaled_flow.y", "scaled_d_flow")


df <- as.tibble(cbind(df[,1:5],scaled)) %>%
      mutate(relative_flow_change = abs(scaled_flow.y-scaled_flow.x))

df <- left_join(df, emitters, by = c("from_node" = "ID"))
df <- left_join(df, emitters, by = c("to_node"   = "ID"))

df <- as.tibble(df) %>% arrange(desc(relative_flow_change))

pipes <- as.tibble(df)

rm(scaled,df)

# suspicious nodes

l <- round(length(pipes$ID)*params$leak_rate,0)

susp_nodes <- as.tibble(c( pipes$from_node[1:l], pipes$to_node[1:l])) %>% 
              count(value, sort = TRUE )

susp_pipes <- pipes[1:l,] %>% select(ID, from_node, to_node)
#-------------------------------------------------------------------------------
# NETWORK VIZUALIZATION
#-------------------------------------------------------------------------------

nodes <- nodes %>%
         mutate(color = ifelse(is.na(FlowCoef),"grey","red"))

# generate data for the visNetwork

nodes <- data.frame( id     = nodes$ID, 
                     value  = nodes$p_median, 
                     color  = nodes$color,
                     shadow = TRUE)

edges <- data.frame( from   = pipes$from_node, 
                     to     = pipes$to_node,
                     value  = pipes$relative_flow_change,
                     arrows = "to",
                     shadow = TRUE)
# visNetwork
visNetwork(nodes, edges ,
           main = "A really simple example", 
           width = "100%") %>%
  visLayout(randomSeed = 12) %>%
  visIgraphLayout()

#...............................................................................
# glimpse(net_report_01)
# git push origin master
#...............................................................................
