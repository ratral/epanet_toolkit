
#...............................................................................
# Plot Time Series
#...............................................................................

plot_ts_curves <- function (ts_curve,
                            x_lab       = "Time (Hours)",
                            y_lab       = "Flow Factor", 
                            time_breaks = "2 hour",
                            time_minor  = "1 hour",
                            x_labtls    = "%H:%M") {
  
  autoplot.zoo(ts_curve, facets = NULL) +
  xlab(x_lab) + 
  ylab(y_lab) +
  scale_x_datetime(breaks       = date_breaks(time_breaks), 
                  minor_breaks = date_breaks(time_minor), 
                  labels       = date_format(x_labtls))+
  theme_wsj() + scale_colour_wsj("colors6", "")
}
#...............................................................................

