#...............................................................................
# 1. - Initialize Session                                                  #### 
#...............................................................................

cat("\014")
rm(list=ls()) 

# Installs libraries 
library(tidyverse)
library(epanetReader)
library(epanet2toolkit)

library(visNetwork)
library(igraph)

# Initialize params
params <- list(base_network    = "base_dma_01", 
               new_network     = "base_dma_w_leaks",
               functs_name     = "epanet_api_functions",
               inlet_valves    = "^PRV_",
               jt_to_analyze   = "^JT_0[A-K]", # RegExp
               pipe_to_analyze = "PS_"         # RegExp
               ) 

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
# 2. Read network information from an *.inp
#...............................................................................

base_input <- read.inp(f_names$base_file_inp)

#...............................................................................
# 3. Running a Full Simulation                                             ####
#...............................................................................

ENepanet(f_names$base_file_inp, f_names$base_file_report)

base_report   <- read.rpt(f_names$base_file_report)

pipes         <- strc_pipes(base_input, "")

pipes_f_base  <- eval_pipes ( base_report,
                              value = "Flow", 
                              group = TRUE, 
                              standardize = FALSE)

pipes <- full_join( pipes, pipes_f_base, by = "ID")

df <- pipes

df$from_node[pipes$f_median < 0 ] <-  pipes$to_node[pipes$f_median < 0 ] 
df$to_node[pipes$f_median < 0 ]   <-  pipes$from_node[pipes$f_median < 0 ] 
df$f_median[pipes$f_median < 0 ]  <-  abs(pipes$f_median[pipes$f_median < 0 ])




