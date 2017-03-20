# ================== решение из статьи, переписанной мной на nest\map идеологию
parseDFDayParts <- function(df, tz) {
  real_times <- ymd_hms(df$timestamp, tz=tz, quiet=TRUE)
  tibble(wkday=weekdays(as.Date(real_times, tz=tz)),
         hour=as.numeric(format(real_times, "%H", tz=tz)))
}

createEventPlot <- function(attacks_raw) {
  attacks <- attacks_raw %>%
    group_by(tz) %>%
    nest()
  
  attacks <- attacks %>%
    mutate(res=map2(data, tz, parseDFDayParts)) %>%
    unnest() %>%
    # превратим wkday в фактор принудительно с понедельника
    mutate(wkday=factor(wkday, levels=weekdays(dmy("13.02.2017")+0:6)))
  
  
  wkdays <- count(attacks, wkday, hour)
  
  gg = ggplot(wkdays, aes(x=hour, y=wkday, fill=n))
  gg = gg + geom_tile(color="white", size=0.1)
  gg = gg + scale_fill_viridis(option="B", name="KPI", label=comma) # https://cran.r-project.org/web/packages/viridis/vignettes/intro-to-viridis.html
  #gg = gg + scale_fill_distiller(palette="RdYlGn", name="# Events", label=comma) # http://docs.ggplot2.org/current/scale_brewer.html
  #gg = gg + coord_equal()
  gg = gg + coord_fixed(ratio = 1)
  gg = gg + labs(x=NULL, y=NULL, title="KPI по часам и дням недели")
  gg = gg + theme_ipsum_rc(base_size = 11.5) +
  # gg = gg + theme_ipsum_rc(base_family="robotoC", base_size = 11.5)
  # gg = gg + theme_tufte(base_family="Verdana")
  gg = gg + theme(plot.title=element_text(hjust=0))
  
  gg
}

