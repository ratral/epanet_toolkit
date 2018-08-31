#...............................................................................
# 1. - Initialize Session                                                  #### 
#...............................................................................

cat("\014")
rm(list=ls()) 

# Installs libraries 
library(tidyverse)
library(zoo)
library(lubridate)
library(epanetReader)
library(epanet2toolkit)
library(ggfortify)
library(ggthemes)
library(scales)
library(stringr)
library(purrr)

# Initialize params
params <- list(base_network  = "base_dma", 
               new_network   = "base_dma_w_leaks",
               functs_name   = "epanet_api_functions",
               inlet_valves  = c("PRV_001"),
               time_step     = "hour",
               pattern_start = "2020-1-1 00:30",
               pattern_end   = "2020-1-1 23:30",
               jt_to_analyze = "^JT_0[A-K]", # RegExp
               main_nodes    = c("JT_0A_001", "JT_0K_011"),
               emitter_coeff = 10000,
               leak_rate     = 0.05, # Percentage of the network with leaks
               demad_factor  = list( names =c( "wd_spring_summer",
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

base_report   <- read.rpt(f_names$base_file_report)
leack_report  <- read.rpt(f_names$new_file_report)

# tab_reports(report,results, type, id, value, summary = FALSE)


pipes <- as.tibble(net_input_01$Pipes) %>%
         select(ID, from_node = Node1, to_node = Node2)


pres_base       <- tab_reports( report  = base_report,
                                results = "nodes", 
                                type    = "Junction", 
                                id      = params$jt_to_analyze,
                                value   = "Pressure", 
                                summary = FALSE)

headloss_base   <- tab_reports( report  = base_report, 
                                results = "links",
                                type    = "Pipe",
                                id      = "PS_",
                                value   = "Headloss",
                                summary = FALSE)

flow_base      <- tab_reports( report  = base_report, 
                               results = "links",
                               type    = "Pipe",
                               id      = "PS_",
                               value   = "Flow",
                               summary = FALSE)

pres_leack     <- tab_reports( report  = leack_report,
                               results = "nodes", 
                               type    = "Junction", 
                               id      = params$jt_to_analyze,
                               value   = "Pressure", 
                               summary = FALSE)

headloss_leack <- tab_reports( report  = leack_report, 
                               results = "links",
                               type    = "Pipe",
                               id      = "PS_",
                               value   = "Headloss",
                               summary = FALSE)

flow_leack     <- tab_reports( report  = leack_report, 
                               results = "links",
                               type    = "Pipe",
                               id      = "PS_",
                               value   = "Flow",
                               summary = FALSE)


delta_pressure <- (pres_base - pres_leack)
delta_headloss <- (headloss_base-headloss_leack)
delta_flow     <- (flow_base-flow_leack)

# glimpse(net_report_01)

