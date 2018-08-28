
#...............................................................................
# Plot Time Series
#...............................................................................

Stats_calc <- function(x){
  min    <- min(x, na.rm=TRUE)
  q25    <- quantile(x, probs=0.25, na.rm=TRUE )
  q50    <- quantile(x, probs=0.50, na.rm=TRUE )
  mean   <- round(mean(x, na.rm=TRUE),2)
  q75    <- quantile(x, probs=0.75, na.rm=TRUE )
  max    <- max(x, na.rm=TRUE)
  sd     <- round(sd(x, na.rm=TRUE),2)
  dvalue <- max-min
  return(c(Min=min, Qu=q25, Qu=q50, Mean=mean, Qu=q75, Max=max, SD=sd, D.Value=dvalue))
}

#...............................................................................
# Plot Time Series
#...............................................................................

plot_ts_curves <- function (ts_curve,
                            m_title = "",
                            y_limits = c(0,7),
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
# EMITTERS
# Generation of the Emitters coefficients
#...............................................................................

gen_emitter <- function(inp_file, emitter_base, id_junctions){

  junctions <- as.tibble(inp_file$Junctions) %>%
               subset(grepl(id_junctions,ID ))   
  
  pipes     <- as.tibble(inp_file$Pipes) %>% 
               subset(grepl(id_junctions,Node1)|grepl(id_junctions,Node2)) %>%
               select(Node1, Node2,Length)
  
  Node1     <- pipes %>% group_by(Node1) %>% summarize(sum(Length))
  Node2     <- pipes %>% group_by(Node2) %>% summarize(sum(Length))
  
  names(Node1) <- c("ID", "sum1")
  names(Node2) <- c("ID", "sum2")
  
  Node  <- full_join(Node1, Node2, by = "ID")
  
  Node$sum1[is.na(Node$sum1)] <- 0
  Node$sum2[is.na(Node$sum2)] <- 0
  
  Node  <- Node %>% mutate(Length = (sum1+sum2)/2)
  
  junctions <- junctions %>%
               full_join(Node, by = "ID") %>%
               mutate(Emitter_C  = emitter_base, 
                      Length     = (sum1+sum2)/2,
                      FlowCoef   = emitter_base * (sum1+sum2)/2) %>%
               select(ID, Demand, Pattern, Emitter_C, Length, FlowCoef) 
}

#...............................................................................
# EMITTERS
# Generate random numbers following a distribution within an interval
#...............................................................................

random_value <- function( n = 1, xmean, xsd, lwr, upr, rounding =3 ) {
  samp <- round(rnorm(n, xmean, xsd), rounding)
  samp[samp < lwr] <- lwr
  samp[samp > upr] <- upr
  samp
}

#...............................................................................
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
  
  emitters  <- as.tibble(inp_file$Junctions) %>%
               full_join(as.tibble(inp_file$Emitters), by= "ID") %>%
               select(ID,Demand,FlowCoef) %>%
               replace_na(list(FlowCoef = 0))
  
  # For each pipe, the total leakage is assigned to its end nodes,
  # half to each node.
  
  pipes <- as.tibble(inp_file$Pipes) %>% select(Node1, Node2,Length)
  
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
  
  emitters[index,]$Betha <- emitters[index,]$Betha + 1/rweibull(1,6,10000)
  
  emitters <- emitters %>% mutate( FlowCoef = round(Betha*Length,4))
  
  net_input_01$Title <- paste0("BASIC DMA MODEL v00.03 WITH LEAKS IN NODES:\n",
                               str_c(emitters[index,]$ID, collapse = ", "), 
                               "\n", "Damages in approximately ",
                               sum(emitters[index,]$Length),
                               " meters of the network")
  
  net_input_01$Emitters <- data.frame(ID       = emitters$ID,
                                      FlowCoef = emitters$FlowCoef)

  net_input_01
}
  
# test <- gen_network_w_leaks(net_input_01, 0.05, "^JT_0[A-K]" )
# test$Betha <- test$Betha %>% map_dbl(rnorm, n=1)
# test



#...............................................................................
# Generates a new network from a base network with randomly generated Leaks                             
#...............................................................................
