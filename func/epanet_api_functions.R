
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
  
  x_limits <- c(ymd_hm("2020-1-1 0:00"), ymd_hm("2020-1-1 24:00"))
  
  autoplot.zoo(ts_curve, facets = NULL) +
  ggtitle(m_title)+
  xlab(x_lab) + 
  ylab(y_lab) +
  scale_x_datetime(breaks       = date_breaks(time_breaks), 
                   minor_breaks = date_breaks(time_minor), 
                   labels       = date_format(x_labtls),
                   limits       = x_limits) +
  ylim(y_limits)+
  theme_hc() + scale_colour_hc()
}
#...............................................................................

