# rm(list=ls()) # очистим все переменные

library(dplyr)
library(magrittr)
library(ggplot2) #load first! (Wickham)
library(ggmap)
library(lubridate) #load second!
library(scales)
library(readr) #Hadley Wickham, http://blog.rstudio.org/2015/04/09/readr-0-1-0/
library(sp)       # spatial operations library
library(leaflet)
library(KernSmooth)
library(akima)
library(rgl)
library(RColorBrewer)

options(warn=1) # http://stackoverflow.com/questions/11239428/how-to-change-warn-setting-in-r
# options(viewer = NULL) # send output directly to browser

load_field_data <- function() {
  ifile <- "./data/appdata_field.csv"
  # подгружаем данные по сенсорам
  raw.df <- read_delim(ifile, delim = ",", quote = "\"",
                       col_names = TRUE,
                       locale = locale("ru", encoding = "windows-1251", tz = "Europe/Moscow"), # таймзону, в принципе, можно установить здесь
                       # col_types = list(date = col_datetime(format = "%d.%m.%Y %H:%M")), 
                       progress = interactive()
  ) # http://barryrowlingson.github.io/hadleyverse/#5
  
  raw.df["timegroup"] <- round_date(raw.df$timestamp, unit = "hour")
  raw.df$value <- round(raw.df$value, 1)
  
  raw.df # возвращаем загруженные данные
}

draw_field_ggmap <- function(sensors.df, heatmap = TRUE) {

  fmap <-
    get_map(
      # enc2utf8("Москва, Зоологическая 2"),
      # "Москва, Зоологическая 2", # надо понимать из какой кодировки грузим
      
      c(median(sensors.df$lon), median(sensors.df$lat)), # будем запрашивать по координатам c(lon, lat)
      language = "ru-RU",
      # source = "stamen", maptype = "watercolor", 
      # source = "stamen", maptype = "toner-hybrid",
      # source = "stamen", maptype = "toner-lite",
      source = "google", maptype = "terrain",
      # source = "osm", maptype = "terrain-background",
      # source = "google", maptype = "hybrid",
      # source = "stamen", maptype = "toner-2011",
      zoom = 16
    )

  # определяем, рисовать ли тепловую карту
  if (heatmap) {
    # ================================ вычисляем тепловую карту
    
    # структура sensors должна быть предельно простая: lon, lat, val. Алгоритм расчитан именно на это
    smap.df <- sensors.df %>%
      select(lon, lat, value) %>%
      rename(val = value)
    
    # print(smap.df)
    
    # данных крайне мало, чтобы не было сильных перепадов принудительно раскидаем
    # по периметру прямоугольника сенсоры с минимальным значением влажности. (мы там не поливаем)
    # сделаем периметр по размеру прямоугольника отображения карты
    hdata <- data.frame(expand.grid(
      lon = seq(attr(fmap, "bb")$ll.lon, attr(fmap, "bb")$ur.lon, length = 10),
      lat = c(attr(fmap, "bb")$ll.lat, attr(fmap, "bb")$ur.lat),
      val = min(smap.df$val)
    ))
    
    vdata <- data.frame(expand.grid(
      lon = c(attr(fmap, "bb")$ll.lon, attr(fmap, "bb")$ur.lon),
      lat = seq(attr(fmap, "bb")$ll.lat, attr(fmap, "bb")$ur.lat, length = 10),
      val = min(smap.df$val)
    ))
    
    
    tdata <- rbind(smap.df, hdata, vdata)
    # print(tdata)
    
    # smap.df <- tdata
    # теперь готовим матрицу для градиентной заливки
    # берем идеи отсюда: http://stackoverflow.com/questions/24410292/how-to-improve-interp-with-akima
    # и отсюда: http://www.kevjohnson.org/making-maps-in-r-part-2/
    fld <- interp(
      tdata$lon,
      tdata$lat,
      tdata$val,
      xo = seq(min(tdata$lon), max(tdata$lon), length = 100),
      yo = seq(min(tdata$lat), max(tdata$lat), length = 100),
      duplicate = "mean",
      # дубликаты возникают по углам искуственного прямоугольника
      #linear = TRUE, #FALSE (после того, как добавили внешний прямоугольник, можно)
      linear = FALSE,
      extrap = TRUE
    )
    
    # превращаем в таблицу значений для комбинаций (x, y)
    # хранение колоночного типа, адресация (x, y)
    # поэтому для делается хитрая развертка -- бегущий x раскладывается по фиксированным y, как оно хранится
    dInterp <-
      data.frame(expand.grid(x = fld$x, y = fld$y), z = c(fld$z))
    # при моделировании сплайнами,
    # в случае крайне разреженных данных могут быть косяки со слишком кривыми аппроксимациями
    # dInterp$z[dInterp$z < min(smap.df$val)] <- min(smap.df$val)
    # dInterp$z[dInterp$z > max(smap.df$val)] <- max(smap.df$val)
    dInterp$z[is.nan(dInterp$z)] <- min(smap.df$val)
  }
  
  # ======================================== генерируем карту
  # http://www.cookbook-r.com/Graphs/Colors_(ggplot2)/
  plot_palette <- brewer.pal(n = 8, name = "Dark2")
  cfpalette <- colorRampPalette(c("white", "blue"))

  # а теперь попробуем отобразить растром, понимая все потенциальные проблемы
  # проблемы хорошо описаны здесь: https://groups.google.com/forum/embed/#!topic/ggplot2/nqzBX22MeAQ
  gm <- ggmap(fmap, extent = "normal", legend = "topleft", darken = c(.5, "white")) # осветлили карту
  # legend = device
  if (heatmap){
    gm <- gm +
      # geom_tile(data = dInterp, aes(x, y, fill = z), alpha = 0.5, colour = NA) +
      geom_raster(data = dInterp, aes(x, y, fill = z), alpha = 0.5) +
      coord_cartesian() +
      # scale_fill_distiller(palette = "Spectral") + # http://docs.ggplot2.org/current/scale_brewer.html
      # scale_fill_distiller(palette = "YlOrRd", breaks = pretty_breaks(n = 10))+ #, labels = percent) +
      # scale_fill_gradientn(colours = brewer.pal(9,"YlOrRd"), guide="colorbar") +
      scale_fill_gradientn(colours = c("#FFFFFF", "#FFFFFF", "#FFFFFF", "#0571B0", "#1A9641", "#D7191C"), 
                           limits = c(0, 100), breaks = c(25, 40, 55, 70, 85), guide="colorbar") +
      # scale_fill_manual(values=c("#CC6666", "#9999CC", "#66CC99")) + # минимум -- белый    stat_contour(data = dInterp, aes(x, y, z = z), bins = 4, color="white", size=0.5) +
      # To use for line and point colors, add
      stat_contour(data = dInterp, aes(x, y, z = z), bins = 4, lwd = 1.1, color="black")
  }
  
  work.df <- sensors.df %>% filter(work.status)
  broken.df <- sensors.df %>% filter(!work.status)
  
  # рисуем показания по рабочим сенсорам
  gm <- gm +
    # scale_colour_manual(values = plot_palette) +
    scale_color_manual(values=c("royalblue", "palegreen3", "sienna1"), name = "Влажность\nпочвы") +
    geom_point(data = work.df, size = 4, alpha = 0.8, aes(x = lon, y = lat, colour = level)) +
    geom_text(data = work.df, aes(lon, lat, label = round(value, digits = 1)), hjust = 0.5, vjust = -1)
  
  # отдельно отрисовываем нерабочие сенсоры
  gm <- gm +
    geom_point(data = broken.df, size = 4, shape = 21, stroke = 1, colour = 'black', fill = 'gold') +
    geom_point(data = broken.df, size = 4, shape = 13, stroke = 1, colour = 'black') +
    geom_text(data = broken.df, aes(lon, lat, label = paste0(delta, " мин"), hjust = 0.5, vjust = 1.8), size = rel(3))
  
  # тематическое оформление
  gm <- gm +
    theme_bw() +
    # убираем все отметки
    theme(axis.line=element_blank(),
          axis.text.x=element_blank(),
          axis.text.y=element_blank(),
          axis.ticks=element_blank(),
          axis.title.x=element_blank(),
          axis.title.y=element_blank(),
          # legend.position="none",
          panel.background=element_blank(),
          panel.border=element_blank(),
          panel.grid.major=element_blank(),
          panel.grid.minor=element_blank(),
          plot.background=element_blank())
  
  gm # возвращаем ggpmap!
}


raw_field.df <- load_field_data()

slicetime <- now()
slicetime <- dmy_hm("29.04.2016 5:00", tz = "Europe/Moscow")
df <- raw_field.df %>%
  filter(timestamp <= slicetime) %>%
  group_by(name) %>%
  filter(timestamp == max(timestamp)) %>%
  mutate(delta = round(as.numeric(difftime(slicetime, timestamp, unit = "min")), 0)) %>%
  arrange(name) %>%
  ungroup() %>%
  # рабочий статус также определяется тем, насколько давно мы видели показания от конкретного сенсора
  mutate(work.status = (delta < 60))


# откатегоризируем
df <- within(df, {
  level <- NA
  level[value >= 0 & value <= 33] <- "Low"
  level[value > 33  & value <= 66] <- "Normal"
  level[value > 66  & value <= 100] <- "High"
})

# при группировке по Lev по умолчанию, порядок следования строк осуществляется по алфавиту
# ggplot(sensors.df, aes(x = lat, y = lon, colour = level)) + 
#  geom_point(size = 4)

# пытаемся изменить группировку
# http://docs.ggplot2.org/current/aes_group_order.html
# сделаем из текстовых строк factor и их принудительно отсортируем
# http://www.ats.ucla.edu/stat/r/modules/factor_variables.htm

# у нас два критерия разделения -- диапазон значений и работоспособность.
# диапазон измерений -- цвет, работоспособность -- форма

sensors.df <- df %>%
  rename(level.unordered = level) %>%
  # mutate(lev.of = ordered(lev, levels = c('Low', 'Normal', 'High'))) %>%
  mutate(level = factor(level.unordered, levels = c('High', 'Normal', 'Low'))) %>%
  mutate(work.status = work.status & !is.na(level)) # что не попало в категорию также считается нерабочим


gm <- draw_field_ggmap(sensors.df, heatmap = FALSE)
# benchplot(gm)
gm