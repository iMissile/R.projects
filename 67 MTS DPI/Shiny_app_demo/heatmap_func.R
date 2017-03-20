# ================== решение из статьи, переписанной мной на nest\map идеологию
parseDFDayParts <- function(df, tz) {
  real_times <- ymd_hms(df$timestamp, tz=tz, quiet=TRUE)
  tibble(wkday=weekdays(as.Date(real_times, tz=tz)),
         hour=as.numeric(format(real_times, "%H", tz=tz)))
}

createEventPlot <- function(attacks, palette, fontsize) {

  wkdays <- count(attacks, wkday, hour)
  
  gg = ggplot(wkdays, aes(x=hour, y=wkday, fill=n))
  gg = gg + geom_tile(color="white", size=0.1)
  if(palette=="viridis"){
    gg = gg + scale_fill_viridis(option="B", name="KPI", label=comma) # https://cran.r-project.org/web/packages/viridis/vignettes/intro-to-viridis.html
  }else{
    gg = gg + scale_fill_distiller(palette="RdYlGn", name="KPI", label=comma) # http://docs.ggplot2.org/current/scale_brewer.html    
  }
  #gg = gg + coord_equal()
  gg = gg + coord_fixed(ratio = 1)
  gg = gg + labs(x=NULL, y=NULL, title="KPI по часам и дням недели")
  gg = gg + theme_ipsum_rc(base_size=fontsize)
  # gg = gg + theme_ipsum_rc(base_family="robotoC", base_size = 11.5)
  # gg = gg + theme_tufte(base_family="Verdana")
  gg = gg + theme(plot.title=element_text(hjust=0))
  gg = gg + theme(plot.background=element_rect(fill = "lightblue", 
                                               colour = "black", size = 2,
                                               linetype = "longdash"))
  
  gg
}

