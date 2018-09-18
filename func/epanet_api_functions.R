
#...............................................................................
# Plot Time Series
#...............................................................................

Stats_calc <- function(x){
  min    <- min(x, na.rm=TRUE)
  max    <- max(x, na.rm=TRUE)
  #  q25    <- quantile(x, probs=0.25, na.rm=TRUE )
  q50    <- quantile(x, probs=0.50, na.rm=TRUE )
  #  mean   <- round(mean(x, na.rm=TRUE),2)
  #  q75    <- quantile(x, probs=0.75, na.rm=TRUE )
  #  sd     <- round(sd(x, na.rm=TRUE),2)
  #  dvalue <- max-min
  return(c(Min=min, Qu=q50, Max=max))
}

#...............................................................................
# Plot Time Series
#...............................................................................

plot_ts_curves <- function (ts_curve,
                            m_title     = "",
                            y_limits    = c(0,7),
                            x_lab       = "Time (Hours)",
                            y_lab       = "Flow Factor", 
                            time_breaks = "2 hour",
                            time_minor  = "30 min",
                            x_labtls    = "%H:%M") {
  
  autoplot.zoo(ts_curve, facets = NULL) +
  ggtitle(m_title)+ xlab(x_lab) + ylab(y_lab) +
  scale_x_datetime(breaks       = date_breaks(time_breaks), 
                   minor_breaks = date_breaks(time_minor), 
                   labels       = date_format(x_labtls)) +
  ylim(y_limits)+
  theme_hc() + scale_colour_hc()
}

#...............................................................................
# select report of the netdata
# report: report name
# result: nodes or links
# Type: 
#    For Links: Pipe, Pump, PRV,  PSV,PBV, FCV
#    For Nodes: Junction, Tank, Reservoir
# value:
#    For Links: Flow Velocity Headloss
#    For Nodes: Elevetion, Demand, Head, Pressure  
# id: RegExp of the nodes or links 
# 
# serie_index
#...............................................................................

tab_reports <- function(report,results, type, id, value, summary = FALSE){
  
  if(results == "nodes"){
    tab_results <- as.tibble(report$nodeResults) %>%
                   subset(nodeType == type & grepl(id,ID )) 
  }
  
  if (results == "links"){
    tab_results <- as.tibble(report$linkResults) %>%
                   subset(linkType == type & grepl(id,ID )) 
  }
  
  tab_results <-tab_results %>%
                select(timeInSeconds, ID, value) %>%
                spread(ID, value) 
  
  if (summary == TRUE){
    tab_results <- cbind(tab_results, 
                         t(apply(tab_results[,2:dim(tab_results)[2]],1, Stats_calc)))
  }
  
  idx <- today() + seconds(tab_results$timeInSeconds)
  
  tab_results <- tab_results %>%
                 select(-timeInSeconds) %>%
                 as.zoo(idx)
}

#...............................................................................

# tab_reports <- function(report,results, type, id, value, summary = FALSE){





#...............................................................................
# EMITTERS
#
# Generation of the Emitters coefficients
# Generates a new network from a base network with randomly generated Leaks                             
#
# The pressure-leakage relationship for a pipe k can be stated as follows:
# q = (Betha * Length) * (Pressure)^ (Alpha) 
# 
# FlowCoef = (Betha * Length)
#
# q = FlowCoef * (Pressure)^ (Alpha) 
# 
# The Alpha parameter can take values between 0.5 and 2.5
# The Betha parameter is related to the pipe material material deterioration
#           and its value but must be set by calibration
#           (initial values can be set around 10e-7)
#...............................................................................

gen_network_w_leaks <- function(inp_file, leak_rate, id_junctions) {
  
  # All nodes are selected and the values of the existing emitters are added.
  
  if(is.null(inp_file$Emitters)){
    emitters  <- as.tibble(inp_file$Junctions) %>%
                 mutate(FlowCoef = 0) %>%
                 select(ID,Demand,FlowCoef)
  } else {
    emitters  <- as.tibble(inp_file$Junctions) %>%
                 left_join(as.tibble(inp_file$Emitters), by= "ID") %>%
                 select(ID,Demand,FlowCoef) %>%
                 replace_na(list(FlowCoef = 0))
  }
  
  
  # For each pipe, the total leakage is assigned to its end nodes,
  # half to each node.
  
  pipes <- as.tibble(inp_file$Pipes) %>% select(Node1, Node2, Length)
  
  node1 <- pipes %>% group_by(Node1) %>% summarize(sum(Length)) %>% 
           rename(ID = Node1, sum1 = `sum(Length)`)
           
  node2 <- pipes %>% group_by(Node2) %>% summarize(sum(Length)) %>% 
           rename(ID = Node2, sum2 = `sum(Length)`)
  
  node  <- full_join(node1, node2, by = "ID") %>% 
           replace_na(list(sum1 = 0, sum2 = 0)) %>% 
           mutate(Length = (sum1+sum2)/2) %>%
           select(ID, Length) 

  emitters <- emitters %>% 
              left_join(node, by= "ID" ) %>%
              mutate(Betha = FlowCoef/Length) %>%
              select(ID, Betha, Length, FlowCoef) %>% 
              subset(grepl(id_junctions,ID ))
  
  index   <- sample(1:nrow(emitters),round(params$leak_rate*nrow(emitters)))
  
  emitters[index,]$Betha <- emitters[index,]$Betha + 1/rweibull(1,5,10000)
  
  emitters <- emitters %>% 
              mutate( FlowCoef = round(Betha*Length,6)) %>%
              filter(FlowCoef > 0)
  
  net_input_01$Title <- "BASIC DMA MODEL v00.03 WITH LEAKS"
  
  net_input_01$Emitters <- data.frame(ID       = emitters$ID,
                                      FlowCoef = emitters$FlowCoef)

  net_input_01
}
  

#...............................................................................
# EVALUATION TABLE
# Evaluate, compare, benchmark Networks 
#...............................................................................

eval_nodes <- function(report,
                       node_type = "",
                       id_nodes  = "", 
                       group = FALSE, standardize = FALSE){

  nodes_tab <- as.tibble(report$nodeResults)  %>%
               filter(grepl(node_type,nodeType) & grepl(id_nodes,ID)) 
  
  if(!group){
    nodes_tab <- nodes_tab  %>%
                 select(timeInSeconds, ID, Pressure) %>%
                 spread(ID, Pressure) 
  }
  
  if (group) {
    nodes_tab <- nodes_tab %>%
                 select(ID, Pressure) %>% 
                 group_by(ID) %>%
                 summarise(p_median = median(Pressure))
 }
  
  if (standardize) {
 
    l <- length(nodes_tab)
    
    maxs    <- apply(nodes_tab[2:l], 2, max) 
    mins    <- apply(nodes_tab[2:l], 2, min)
    
    scaled  <- (scale(nodes_tab[2:l], 
                      center = mins, 
                      scale = maxs - mins)*100)
    
    nodes_tab <- as.tibble(cbind(nodes_tab[1],scaled))
 
}
  nodes_tab
}
#...............................................................................

eval_pipes <- function(report, 
                       link_type ="",
                       id_pipes = "", 
                       inlet_links ="",
                       value = "Flow", 
                       group = FALSE, standardize = FALSE){
    
    pipes_tab <- as.tibble(report$linkResults) %>%
                 filter(grepl(link_type, linkType) & grepl(id_pipes, ID)) 
    
    if(value == "Flow" & !group){
      pipes_tab <- pipes_tab  %>%
                   select(timeInSeconds, ID, Flow ) %>%
                   spread(ID, Flow) 
    }   
   
    if(value == "Flow" & group) {
      pipes_tab <- pipes_tab %>%
                   select(ID, Flow) %>%
                   group_by(ID) %>%
                   summarise(f_median  = median(Flow))
    }
 
    if (standardize) {

      maxs    <- pipes_tab %>% 
                 filter(grepl(inlet_links, ID)) %>%
                 select(f_median)
      
      mins    <- apply(pipes_tab[2:l], 2, min)
      
      scaled  <- (scale(pipes_tab[2:l], 
                        center = mins, 
                        scale = maxs - mins)*100)
      
      pipes_tab <- as.tibble(cbind(pipes_tab[1],scaled))
      
    }
    
    pipes_tab
}

#...............................................................................

strc_pipes <- function(inp_file, id_pipes = "", other_elements = FALSE ) {
  
  pipes <- as.tibble(inp_file$Pipes) %>%
           filter(grepl(id_pipes, ID)) %>%
           select(ID, from_node = Node1, to_node = Node2)

  if(other_elements){
    
    if(!is.null(inp_file$Valves)){
      valves <- as.tibble(inp_file$Valves) %>% 
                select(ID, from_node = Node1, to_node = Node2)
      pipes  <-  rbind.data.frame(pipes,valves)
    }
    
    if(!is.null(inp_file$Pumps)){
      pumps  <- as.tibble(inp_file$Pumps) %>% 
                select(ID, from_node = Node1, to_node = Node2)
      pipes  <-  rbind.data.frame(pipes,pumps)
    }
  }
  
  pipes
}

#...............................................................................
# EVALUATION INLET FLOWS
#...............................................................................
inlet_flows <- function(report,id_pipes, group = FALSE){
  
  pipes_tab <- as.tibble(report$linkResults) %>%
               filter( grepl(id_pipes, ID )) 
  
  if(!group){
    pipes_tab <- pipes_tab  %>%
                 select(timeInSeconds, ID, Flow ) %>%
                 spread(ID, Flow) 
    
    pipes_tab <- cbind(pipes_tab, 
                       t(apply(pipes_tab[,2:dim(pipes_tab)[2]],1, Stats_calc)))
  }   
  
  if(group) {
    pipes_tab <- pipes_tab %>%
                 select(ID, Flow) %>%
                 group_by(ID) %>%
                 summarise( f_min  = min(Flow),
                            f_q25     = quantile(Flow, 0.25),
                            f_median  = median(Flow),
                            f_mean    = mean(Flow),
                            f_q75     = quantile(Flow, 0.75),
                            f_max     = max(Flow))
    
  }
  pipes_tab
}