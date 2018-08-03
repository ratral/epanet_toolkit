
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

