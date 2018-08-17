#...............................................................................
# 1. - Initialize Session #### 
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

# Initialize params

params <- list(net_works     = "prv_01", 
               inlet_valves  = c("PRV_001"),
               time_step     = "hour",
               pattern_start = "2020-1-1 00:30",
               pattern_end   = "2020-1-1 23:30",
               jt_to_analyze = "^JT_0[A-K]", # RegExp
               main_nodes    = c("JT_0A_001", "JT_0K_011"),
               emitter_coeff = 1/10000)


demad_factor <- list( c( "wd_spring_summer","hw_spring_summer",
                         "wd_summer_break","hw_summer_break",
                         "wd_fall_winter","hw_fall_winter"),
                      c( 0.92, 1.00, 1.09, 0.81, 0.66, 0.95))

# initialize files paths and files

dir_work   <-  getwd()
dir_report <-  file.path(dir_work,"reports") 
dir_data   <-  file.path(dir_work,"data")  
dir_bin    <-  file.path(dir_work,"reports")
dir_func   <-  file.path(dir_work,"func")

file_inp     <- file.path(dir_data,   paste0(params$net_work,".inp"))
file_report  <- file.path(dir_report, paste0(params$net_work,".rpt"))
file_bin     <- file.path(dir_report, paste0(params$net_work,".bin"))
file_func    <- file.path(dir_func,   "epanet_api_functions.R")

# Load Functions Standard

source(file_func)

# Read network information from an *.inp

net_input_01  <- read.inp(file_inp)
