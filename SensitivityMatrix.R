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
params <- list(base_network    = "base_dma_02", 
               new_network     = "base_dma_w_leaks",
               functs_name     = "epanet_api_functions",
               inlet_valves    = "PRV_",
               jt_to_analyze   = "^JT_0[A-K]", # RegExp
               pipe_to_analyze = "PS_",        # RegExp
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

params$f_names <- list( base_file_inp    = file.path(params$work_folders$dir_data, 
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
source(params$f_names$file_func)

#...............................................................................
# 2. GENERATE THE LEAKAGE IN A NEW NETWORK                                  ####
#...............................................................................

# Read network information from an *.inp

net_input_01  <- read.inp(params$f_names$base_file_inp)

net_input_01 <- gen_network_w_leaks(net_input_01, 
                                    params$leak_rate, 
                                    params$jt_to_analyze )


write.inp(net_input_01, params$f_names$new_file_inp)

rm(net_input_01)

#...............................................................................
# 3. Running a Full Simulation                                             ####
#    The function ENepanet() runs a full simulation and 
#    writes the results to a file. 
#...............................................................................

ENepanet(params$f_names$base_file_inp, 
         params$f_names$base_file_report)

ENepanet(params$f_names$new_file_inp,
         params$f_names$new_file_report)

net_input_01  <- read.inp(params$f_names$new_file_inp)
report_base   <- read.rpt(params$f_names$base_file_report)
report_leack  <- read.rpt(params$f_names$new_file_report)

#...............................................................................
# 4. pattern to estimate the evolution of the consumption                   ####
#...............................................................................

# New Leaks (EMITTERS)

emitters <- as.tibble(net_input_01$Emitters)

# 4.1.- the average inflow f(t) was calculated at each hour t

base_inletflow <- inlet_flows(report_base,  params$inlet_valves, group = FALSE) 
leak_inletflow <- inlet_flows(report_leack, params$inlet_valves, group = FALSE)

inletflow      <- full_join(base_inletflow, leak_inletflow, 
                            by = "timeInSeconds") %>%
                  select(timeInSeconds, inflow.x, inflow.y)  %>%
                  mutate(leakflow = inflow.y - inflow.x)


rm(base_inletflow,leak_inletflow)

# 4.2.- the average network pressure p(t) was calculated at each hour t

rep01 <- eval_nodes (report_base, node_type = "",
                     id_nodes  = params$jt_to_analyze, 
                     group = FALSE) %>%
                     group_by(timeInSeconds) %>%
                     summarise(p_median = median(Pressure))
                
rep02 <- eval_nodes (report_leack, node_type = "",
                     id_nodes  = params$jt_to_analyze, 
                     group = FALSE) %>%
                     group_by(timeInSeconds) %>%
                     summarise(p_median = median(Pressure))

rep_p_time <-  full_join(rep01, rep02, by = "timeInSeconds" )

global_emitter <-  full_join(rep_p_time,inletflow, by = "timeInSeconds" )

rm(rep01,rep02, rep_p_time)

# calculate a global emitter

global_emitter <- global_emitter %>%
                  mutate(DMA_FlowCoef = leakflow/sqrt(p_median.y))

# residual vector

rep01 <- eval_nodes (report_base, node_type = "",
                     id_nodes  = params$jt_to_analyze, 
                     group = FALSE)

rep02 <- eval_nodes (report_leack, node_type = "",
                     id_nodes  = params$jt_to_analyze, 
                     group = FALSE)

residual <- full_join(rep01, rep02, by = c("timeInSeconds","ID")) %>%
            mutate(D_Pressure = Pressure.y - Pressure.x)

# sensitivity vector
emitters
summary(inletflow)

# detection capability matrix Mdc Where : 
# - the rows represent the pontential sensor locations and 
# - the columns represent the potential leak location




