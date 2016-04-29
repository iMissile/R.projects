# генерируем тестовые файлы для отладки интерфейса
#library(tidyr)
library(ggplot2) #load first! (Wickham)
library(lubridate) #load second!
library(dplyr)
library(readr)
library(ggthemes)
library(ggmap)
library(RColorBrewer)
library(gtable)
library(grid) # для grid.newpage()
library(gridExtra) # для grid.arrange()

# library(ggthemr) # устарело :(
# library(wesanderson)

ggplot_dual_axis <- function(plot1, plot2, which.axis = "x") {
  # исходник взят отсюда: https://gist.github.com/jslefche/e4c0e9f57f0af49fca87
  
  grid.newpage()
  
  # Increase right margin if which.axis == "y"
  if (which.axis == "y")
    plot1 <- plot1 + theme(plot.margin = unit(c(0.7, 1.5, 0.4, 0.4), "cm"))
  
  # Extract gtable
  g1 <- ggplot_gtable(ggplot_build(plot1))
  g2 <- ggplot_gtable(ggplot_build(plot2))
  
  # Overlap the panel of the second plot on that of the first
  pp <- c(subset(g1$layout, name == "panel", se = t:r))
  
  g <- gtable_add_grob(g1, g2$grobs[[which(g2$layout$name == "panel")]], pp$t, pp$l, pp$b, pp$l)
  
  # Steal axis from second plot and modify
  axis.lab <- ifelse(which.axis == "x", "axis-b", "axis-l")
  ia <- which(g2$layout$name == axis.lab)
  ga <- g2$grobs[[ia]]
  ax <- ga$children[[2]]
  
  # Switch position of ticks and labels
  if (which.axis == "x"){
    ax$heights = rev(ax$heights)
  } else {
    ax$widths = rev(ax$widths)
  }
  
  ax$grobs <- rev(ax$grobs)
  
  if (which.axis == "x") {
    ax$grobs[[2]]$y <- ax$grobs[[2]]$y - unit(1, "npc") + unit(0.15, "cm")
  } else {
    ax$grobs[[1]]$x <- ax$grobs[[1]]$x - unit(1, "npc") + unit(0.15, "cm")
  }
  
  # Modify existing row to be tall enough for axis
  if (which.axis == "x"){
    g$heights[[2]] = g$heights[g2$layout[ia, ]$t]
  }
  
  # Add new row or column for axis label
  if (which.axis == "x") {
    g <- gtable_add_grob(g, ax, 2, 4, 2, 4)
    g <- gtable_add_rows(g, g2$heights[1], 1)
    g <- gtable_add_grob(g, g2$grob[[6]], 2, 4, 2, 4)
  } else {
    g <- gtable_add_cols(g, g2$widths[g2$layout[ia,]$l], length(g$widths) - 1)
    g <- gtable_add_grob(g, ax, pp$t, length(g$widths) - 1, pp$b)
    g <- gtable_add_grob(g, g2$grob[[7]], pp$t, length(g$widths), pp$b - 1)
  }
    
  # Draw it
  grid.draw(g)
}

dual_axis_demo <- function() {
  # взято отсюда: https://gist.github.com/jslefche/e4c0e9f57f0af49fca87
  # требуется
  # library(gtable)
  # library(grid) # для grid.newpage()
  
  
  # Example for x-axis
  # Create fake data.frame
  data.add.x = data.frame(
    y1 = runif(100, 0, 100),
    x1 = runif(100, 0, 100)
  )
  
  # Add second x-axis that scales with first
  data.add.x$x2 = (data.add.x$x1 + 50)^0.75
  
  # Create plots
  plot1.x = qplot(y = y1, x = x1, data = data.add.x)
  plot2.x = qplot(y = y1, x = x2, data = data.add.x)
  
  # Run function
  ggplot_dual_axis(plot1.x, plot2.x, "x")
  
  # Example for y-axis
  
  # Add second y-axis that scales with first
  data.add.x$y2 = (data.add.x$y^0.5) / 500
  
  # Create plots
  plot1.y = qplot(y = y1, x = x1, data = data.add.x)
  plot2.y = qplot(y = y2, x = x1, data = data.add.x)
  
  # Run function
  ggplot_dual_axis(plot1.y, plot2.y, "y")
}

generate_field_data <- function(ofile = "tsensors.csv", back_days = 7, forward_days = 7) {
  # данные по расположению сенсоров
  sensor <- data.frame(
    c(1, 37.578691607470724, 55.766160765720493),
    c(2, 37.57990362015564, 55.766504551149772),
    c(3, 37.580025839922193, 55.765874275547077),
    c(4, 37.577632369493983, 55.764819973580607),
    c(5, 37.581431367237478, 55.764384492709446),
    c(6, 37.579109191673091, 55.765158040903287),
    c(7, 37.58182858147876, 55.766160765720493),
    c(8, 37.580586013852198, 55.767008764295831),
    c(9, 37.579862880233463, 55.764109449653162),
    c(10, 37.577856439065989, 55.766567578149633),
    c(11, 37.576776831128157, 55.76609773806296)
  )
  # sensor <- t(sensor) #транспонируем
  # sensor <- tFrame(sensor) #транспонируем
  sensor <- as.data.frame(t(sensor))
  rownames(sensor) <- NULL
  colnames(sensor) <- c("name", "lon", "lat") # http://stackoverflow.com/questions/6081439/changing-column-names-of-a-data-frame-in-r
  
  #sensor <- as.data.frame(t(sensor)) #транспонируем
  # dimnames(sensor) <- NULL
  
  # генерируем данные по показаниям сенсоров (почасовая раскладка)
  # смотрим за последнюю неделю
  # tick.seq <- seq(as.POSIXct("2016-03-01 23:00:00"), as.POSIXct("2016-03-10 08:32:00"), by = "4 hours") # http://stackoverflow.com/questions/10887923/hourly-date-sequence-in-r
  tick.seq <- seq(now() - days(back_days), now() + days(forward_days), by = "4 hours") # http://stackoverflow.com/questions/10887923/hourly-date-sequence-in-r
  # собираем сразу data.frame: время, #сенсора, показание
  mydata <- data.frame(name = rep(sensor$name, each = length(tick.seq)),
                       type = "Temp",
                       location = "Капуста 1",
                       #location = "Картофель 1",
                       timestamp = tick.seq, 
                       value = rnorm(nrow(sensor)*length(tick.seq), 15, 1)) # используем методику дополнения
  # rnorm(1000, 3, .25) # Generates 1000 numbers from a normal with mean 3 and sd=.25
  
  # для соотв реалиям сделаем разброс во времени измерения
  mydata$timestamp <- mydata$timestamp + runif(nrow(mydata), min = -5*60, max = 5*60)
  mydata$value <- round(mydata$value + 0.2*sin(as.numeric(mydata$timestamp)/86400*(0.1*pi)), 1)

  qplot(timestamp, value, data = mydata)
  
  write.table(
    mydata,
    file = ofile,
    sep = ",",
    row.names = FALSE,
    qmethod = "double"
  )
}

generate_weather_data <- function(ofile = "tweather.csv", back_days = 7, forward_days = 7) {
  # генерируем погодные даты
  
  tick.seq <- seq(now() - days(back_days), now() + days(forward_days), by = "4 hours") # http://stackoverflow.com/questions/10887923/hourly-date-sequence-in-r

  # для подготовки данных 
  # собираем сразу data.frame: время, #сенсора, показание
  n <- length(tick.seq)
  mydata <- data.frame(timestamp = tick.seq,
                       temp.min = rnorm(n, 14, 3), # используем методику дополнения
                       pressure = rnorm(n, 750, 30),
                       humidity = runif(n, 10, 100),
                       precipitation = runif(n, 0, 20))

  # максимальная температура должна быть выше минимальной
  # средняя должна быть между максимальной и минимальной
  mydata %<>% mutate(temp.max = temp.min + runif(n, 5, 15)) %>%
    mutate(temp = temp.min + 0.7 * runif(n, 0, temp.max - temp.min))
    
    

  write.table(
    mydata,
    file = ofile,
    sep = ",",
    row.names = FALSE,
    qmethod = "double"
  )
}

plot_field_data <- function(ifile = ".\\data\\test_data.csv") {
  # отобразим за указанный диапазон график: среднее, стандартное отклонение в виде серой области, выбросы

  # подгружаем данные по сенсорам
  raw.df <- read_delim(ifile, delim = ",", quote = "\"",
                       col_names = TRUE,
                       locale = locale("ru", encoding = "windows-1251", tz = "Europe/Moscow"), # таймзону, в принципе, можно установить здесь
                       # col_types = list(date = col_datetime(format = "%d.%m.%Y %H:%M")), 
                       progress = interactive()
  ) # http://barryrowlingson.github.io/hadleyverse/#5
  
  # сформируем часовые группы и посчитаем среднее по ансамблю сенсоров за каждую часовую группу
  # raw.df["time.group"] <- lapply(raw.df$timestamp, function(x){round(x, units="hours")})
  # t <- raw.df$timestamp
  # object.size(t)
  
  # m <- lapply(raw.df$timestamp, function(x){round(x, units="hours")}) # так из 130 кб получаем 35 Мб, надо использовать round_date {lubridate}
  # m <- lapply(t, function(x){round_date(x, unit = "hour")}) # тут получаем 8Мб !!!
  # m <- round_date(t, unit = "hour") # самый быстрый и компактный вариант
  # object.size(m)
  # object.size(m[[2]])
  raw.df["timegroup"] <- round_date(raw.df$timestamp, unit = "hour")
  
  # t.df <- raw.df %>%
  #   filter(timegroup < lubridate::now()) %>%
  #   filter(timegroup > lubridate::now() - days(3))
  
  avg.df <- raw.df %>%
    group_by(location, timegroup) %>%
    summarise(value.mean = mean(value), value.sd = sd(value))
  
  object.size(raw.df)
  object.size(avg.df)  
  
  
  # ggplot(avg.df, aes(timegroup, value.mean)) +
  #   geom_point() +
  #   geom_smooth(method="loess", level = 0.99999)
  
  ggplot(avg.df, aes(timegroup, value.mean, colour = factor(location))) +
    # ggtitle("График температуры") +
    geom_point() +
    geom_line() +
    ylim(0, NA) +
    theme_solarized() +
    scale_colour_solarized("blue") +
    theme(panel.border = element_rect(colour = "black", fill=NA, size = 2))
  # theme_solarized(light = TRUE) +
  # scale_colour_solarized("blue")
  # theme_hc() +
  # scale_colour_hc()
  
  
  # http://docs.ggplot2.org/current/geom_boxplot.html
  # You can also use boxplots with continuous x, as long as you supply a grouping variable.
  # http://stackoverflow.com/questions/23433776/change-thickness-of-the-whole-line-geom-boxplot
  ggplot(raw.df, aes(timestamp, value)) +
    # geom_point(shape = 1) +
    # geom_line() +
    geom_jitter(width = 0.2) +
    ylim(0, NA) +
    geom_boxplot(aes(group = cut_width(timestamp, 86400/5))) + 
    geom_smooth(method="loess", level = 0.99999)
}

plot_weather_data <- function(ifile = ".\\data\\tweather.csv") {
  
  raw.df <- read_delim(
    ifile,
    delim = ",",
    quote = "\"",
    col_names = TRUE,
    locale = locale("ru", encoding = "windows-1251", tz = "Europe/Moscow"),
    # таймзону, в принципе, можно установить здесь
    # col_types = list(date = col_datetime(format = "%d.%m.%Y %H:%M")),
    progress = interactive()
  ) # http://barryrowlingson.github.io/hadleyverse/#5
  
  # сформируем часовые группы и посчитаем среднее по ансамблю сенсоров за каждую часовую группу
  raw.df['timegroup'] <- round_date(raw.df$timestamp, unit = "hour")
  

  # разметим данные на прошлое и будущее. будем использовать для цветовой группировки
  raw.df['time.pos'] <- ifelse(raw.df$timestamp < now(), "PAST", "FUTURE")
  raw.df$temp[raw.df$time.pos == "FUTURE"] <- NA
  
  # http://www.cookbook-r.com/Graphs/Colors_(ggplot2)/
  jBrewColors <- brewer.pal(n = 8, name = "Dark2") 
  
  # https://www.datacamp.com/community/tutorials/make-histogram-ggplot2
  ggplot(raw.df, aes(timestamp, temp)) +
    # ggtitle("График температуры") +
    # scale_fill_brewer(palette="Set1") +
    scale_fill_brewer(palette="Paired") +
    geom_ribbon(aes(ymin = temp.min, ymax = temp.max, fill = time.pos), alpha = 0.5) +
    geom_point(shape = 1, size = 3) +
    geom_line(lwd = 1, linetype = 'dashed', color = "red") +
    
    # а теперь добавим данные по осадкам
    theme_igray() 
  
    # scale_colour_tableau() +
    # theme_solarized(light = FALSE) +
    # scale_colour_solarized("red")
  
  
    
  
  
    # geom_area(fill="violet", alpha=.4) +
    # geom_step() +
    # ylim(0, NA)
    # наложим гистограмму с влажностью
    # geom_histogram(breaks = seq(20, 50, by = 2),
    #                col="red",
    #                fill="green",
    #                alpha = .2)
    # theme_solarized() +
    # scale_colour_solarized("blue") +
    # theme(panel.border = element_rect(colour = "black", fill=NA, size = 2))
  # theme_solarized(light = TRUE) +
  # scale_colour_solarized("blue")
  # theme_hc() +
  # scale_colour_hc()
}


# =================== main ==================

# ofile <- ".\\data\\tsensors.csv"
# generate_field_data(ofile, back_days = 7, forward_days = 0)
# plot_field_data(".\\data\\test_data.csv")
# 
#generate_weather_data(".\\data\\tweather.csv", back_days = 7, forward_days = 3)
p1 <- plot_weather_data(".\\data\\test_weather.csv")

# dual_axis_demo()

# Add boxplots to a scatterplot
par(fig = c(0,0.8,0,0.8), new = FALSE) 
p1

grid.arrange(p1, p1, ncol = 1)

stop()

# Add boxplots to a scatterplot
par(fig=c(0,0.8,0,0.8), new=FALSE)
plot(mtcars$wt, mtcars$mpg, xlab="Car Weight",
     ylab="Miles Per Gallon")
par(fig=c(0,0.8,0.55,1), new=TRUE)
boxplot(mtcars$wt, horizontal=TRUE, axes=FALSE)
#par(fig=c(0.65,1,0,0.8),new=TRUE)
#boxplot(mtcars$mpg, axes=FALSE)
#mtext("Enhanced Scatterplot", side=3, outer=TRUE, line=-3) 

stop()

# ======================
getMap <- get_map(
  enc2utf8("Москва, Зоологическая 2"),
  language = "ru-RU",
  # source = "stamen",
  # maptype = "watercolor", 
  maptype = "terrain",
  zoom = 16
)

ggmap(getMap, extent="panel")

