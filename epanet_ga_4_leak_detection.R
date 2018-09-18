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

# Initialize params
params <- list(base_network    = "base_dma_02", 
               new_network     = "base_dma_w_leaks",
               functs_name     = "epanet_api_functions",
               inlet_valves    = "PRV_",
               time_step       = "hour",
               pattern_start   = "2020-1-1 00:30",
               pattern_end     = "2020-1-1 23:30",
               jt_to_analyze   = "^JT_0[A-K]", # RegExp
               pipe_to_analyze = "PS_", # RegExp
               main_nodes      = c("JT_0A_001", "JT_0K_011"),
               emitter_coeff   = 10000,
               leak_rate       = 0.01, # Percentage of the network with leaks
               demad_factor    = list( names =c( "wd_spring_summer",
                                                 "hw_spring_summer",
                                                 "wd_summer_break",
                                                 "hw_summer_break",
                                                 "wd_fall_winter",
                                                 "hw_fall_winter"),
                                       factors = c( 0.92, 1.00, 1.09, 
                                                    0.81, 0.66, 0.95)))

# initialize files paths and files
work_folders <- list( dir_work   = getwd(),
                      dir_report = file.path(getwd(),"reports"), 
                      dir_data   = file.path(getwd(),"data"),  
                      dir_bin    = file.path(getwd(),"reports"),
                      dir_func   = file.path(getwd(),"func"))

f_names  <- list( base_file_inp    = file.path(work_folders$dir_data, 
                                               paste0(params$base_network,".inp")),
                  base_file_report = file.path(work_folders$dir_report, 
                                               paste0(params$base_network,".rpt")),
                  new_file_inp     = file.path(work_folders$dir_data, 
                                               paste0(params$new_network,".inp")),
                  new_file_report  = file.path(work_folders$dir_report,
                                               paste0(params$new_network,".rpt")),
                  file_func        = file.path(work_folders$dir_func, 
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

#...............................................................................
# 3. Running a Full Simulation                                             ####
#    The function ENepanet() runs a full simulation and 
#    writes the results to a file. 
#...............................................................................

ENepanet(f_names$base_file_inp, f_names$base_file_report)
ENepanet(f_names$new_file_inp,  f_names$new_file_report)

net_input_01  <- read.inp(f_names$new_file_inp)

base_report   <- read.rpt(f_names$base_file_report)
leack_report  <- read.rpt(f_names$new_file_report)

# New Leaks (EMITTERS)

emitters <- net_input_01$Emitters

# PIPES AND FLOW

pipes   <- strc_pipes(net_input_01, "", TRUE)

pipes_f_base   <- eval_pipes ( base_report,
                               link_type ="",
                               id_pipes = "", 
                               inlet_links ="",
                               value = "Flow", 
                               group = TRUE, 
                               standardize = FALSE)

pipes_f_leack  <- eval_pipes ( leack_report, 
                               link_type ="",
                               id_pipes = "", 
                               inlet_links ="",
                               value = "Flow", 
                               group = TRUE, 
                               standardize = FALSE)

pipes    <- full_join( pipes, pipes_f_base,   by = "ID")
pipes    <- full_join( pipes, pipes_f_leack,  by = "ID")

rm(pipes_f_base, pipes_f_leack)

# calculate input flow
input_flow <- pipes %>%
              filter(grepl(params$inlet_valves, ID)) %>%
              summarise( total.x = sum(f_median.x), total.y = sum(f_median.y))

# select only the net pipes 
pipes    <- pipes %>% 
            filter(grepl(params$pipe_to_analyze, ID))


# change from_node and to_node in function of the Flow direction 
df <- pipes

df$from_node[pipes$f_median.y < 0 ]  <-  pipes$to_node  [pipes$f_median.y < 0] 
df$to_node  [pipes$f_median.y < 0 ]  <-  pipes$from_node[pipes$f_median.y < 0] 

df <- df %>% mutate(d_flow = abs(f_median.y - f_median.x)) 

maxs    <- apply(df[4:6], 2, max) 
mins    <- apply(df[4:6], 2, min)

# scaled  <- (scale(df[4:5], center = mins, scale = maxs - mins)*100)

scaled    <- as.tibble(scale(df[4:6], center = mins, scale = maxs - mins)*100)

names(scaled) <- c("scaled_flow.x","scaled_flow.y", "scaled_d_flow")

df <- as.tibble(cbind(df[,1:5],scaled)) %>%
      mutate(relative_flow_change = abs(scaled_flow.y-scaled_flow.x))

# maxs <- max(df$scaled_d_flow)
# mins <- min(df$scaled_d_flow)

# df <- df %>% 
#       mutate(scaled_d_flow = scale(f_median.y, 
#                                    center = mins[2], 
#                                   scale = maxs[2] - mins[2])*100)

df <- left_join(df, emitters, by = c("from_node" = "ID"))
df <- left_join(df, emitters, by = c("to_node"   = "ID"))

df <- as.tibble(df) %>% arrange(desc(relative_flow_change))

emitters

#...............................................................................
# glimpse(net_report_01)
# git push origin master
#...............................................................................

