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
               leak_rate     = 0.05, 
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

# Read network information from an *.inp
net_input_01  <- read.inp(f_names$base_file_inp)

#...............................................................................
# 2. GENERATE THE LEAKAGE IN A NEW NETWORK                                  ####
#...............................................................................

if(!file.exists(f_names$new_file_inp)){
  

junctions_base <- gen_emitter (inp_file = net_input_01, emitter_base = 0, 
                               id_junctions = params$jt_to_analyze) %>%
                  select(ID, Emitter_C, Length, FlowCoef)

# We proceed to generate the leaks by randomly into the network

index <- sample(1:nrow(junctions_base), 
                round(params$leak_rate*nrow(junctions_base)))

junctions_base[index,]$Emitter_C <-  1/(random_value(n = length(junctions_base[index,]$Emitter_C),
                                                     xmean = params$emitter_coeff, 
                                                     xsd   = params$emitter_coeff/3, 
                                                     lwr   = params$emitter_coeff/2, 
                                                     upr   = params$emitter_coeff*1,5))

junctions_base <- junctions_base %>%
                  mutate( FlowCoef = round(Emitter_C*Length,4))

net_input_01$Title <- paste0("BASIC DMA MODEL v00.03 WITH LEAKS IN NODES:\n",
                             str_c(junctions_base[index,]$ID, collapse = ", "), 
                             "\n", "Damages in approximately ",
                             sum(junctions_base[index,]$Length),
                             " meters of the network")


net_input_01$Emitters <- data.frame(ID       = junctions_base$ID,
                                    FlowCoef = junctions_base$FlowCoef)

# We generate the network with the randomized leaks.
write.inp(net_input_01, f_names$new_file_inp)

}

#if(!file.exists(f_names$new_file_inp)){file.copy(base_file_inp,ga_file_inp )}

#...............................................................................
# 3. Running a Full Simulation                                             ####
#    The function ENepanet() runs a full simulation and 
#    writes the results to a file. 
#...............................................................................



