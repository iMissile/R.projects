# нумерация дней недели начинается с воскресенья (1), сделаем список с названиями дней для частного отображения
wdnames <-
  c(
    "Воскресенье",
    "Понедельник",
    "Вторник",
    "Среда",
    "Четверг",
    "Пятница",
    "Суббота"
  )



# =================================================
date_format_tz <- function(format = "%Y-%m-%d", tz = "UTC") {
  function(x) format(x, format, tz=tz)
}

# =================================================
day_raw_plot <- function (date, data = subdata) {
  # превращаем в long-формат, чтобы выдать как группу на гистограмме
  # http://seananderson.ca/2013/10/19/reshape.html
  # fix.date -- дата в формате POSIXct на которую необходимо построить график
  
  # browser()
  # поставить ветку else
  if (!is.POSIXct(date)) {
    date <- dmy_hms(date, truncated = 3, tz = "Europe/Moscow")
  }
  
  fix.date <- floor_date(date, "day")
  print(fix.date)
  
  df_open <<-
    filter(data, date == fix.date) %>%
    select(timestamp, value, baseline) %>%
    melt(id.vars = c("timestamp"))
    # filter(!is.na(value)) # для корректного отображения данных опустим все, что NA
  
  gp <- ggplot(data = df_open, aes(x=timestamp, y=value)) +
    theme_bw() +
    geom_point(size = 4, fill = "white", shape = 21, na.rm = TRUE) +    # White fill
    geom_line(size = 1, aes(color = variable), na.rm = TRUE) +
    # scale_color_manual(values = wes_palette("Moonrise2")) +
    # дельта в 3 часа при выводе на суточном графике
    # http://stackoverflow.com/questions/10339618/what-is-the-appropriate-timezone-argument-syntax-for-scale-datetime-in-ggplot
#    scale_x_datetime(labels = date_format_tz("%H:%M", tz="Europe/Moscow"), breaks = date_breaks("1 hours"), minor_breaks = date_breaks("30 mins")) +
    scale_x_datetime(labels = date_format_tz("%H", tz="Europe/Moscow"), breaks = date_breaks("1 hours"), minor_breaks = date_breaks("30 mins")) +
    labs(x="Дата", y="Interface load, %")
  # %d.%m (%a)  
  gp
}


# =================================================
plot_mean_integr <- function() {
  gp71 <- ggplot(df7, aes(x = date, y = mean)) +
    theme_bw() +
    geom_line(size = 1, color = RColorBrewer::brewer.pal(3, "Set2")[1]) +
    geom_line(aes(y = integr),
              size = 1,
              color = RColorBrewer::brewer.pal(3, "Set2")[2]) +
    geom_point(aes(size = std.dev), fill = "white", shape = 21) +    # White fill
    geom_point(aes(y = integr), fill = "white", shape = 21) +    # White fill
    geom_smooth(method = 'lm', formula = y ~ x) +
    geom_smooth(aes(y = integr), method = 'lm', formula = y ~ x) +
    scale_x_datetime(labels = date_format("%d.%m (%a)"),
                     minor_breaks = date_breaks("1 days")) +
    coord_trans(y = "log10") + # http://www.cookbook-r.com/Graphs/Axes_(ggplot2)/#axis-transformations-log-sqrt-etc
    labs(x = "Дата", y = "Среднее за сутки значение загрузки")
  
  # Open a new png device to print the figure out to (or use tiff, pdf, etc).
  png(
    filename = "03. days_ditributon.png",
    width = 800,
    height = 600,
    units = 'px'
  )
  print(gp71) #end of print statement
  dev.off() #close the png device to save the figure.
}


# =================================================
plot_regr_hdata <- function(){
  # https://stackoverflow.com/questions/7549694/ggplot2-adding-regression-line-equation-and-r2-on-graph
  # http://stackoverflow.com/questions/15867263/ggplot2-geom-text-with-facet-grid
  # Basically, you create a data.frame with the text which contains a column with the text, and a column with the variables you use for facet_grid.
  
  # https://trinkerrstuff.wordpress.com/2012/09/01/add-text-annotations-to-ggplot2-faceted-plot/
  # http://www.cookbook-r.com/Graphs/Facets_(ggplot2)/
  # This is done by giving a formula to facet_grid(), of the form vertical ~ horizontal.
  gp8 <- ggplot(data = df8, aes(x = date, y = ratio)) +
    theme_bw() +
    geom_point(size = 3,
               fill = "white",
               shape = 21) +    # White fill
    geom_line(size = 1, color = RColorBrewer::brewer.pal(3, "Set2")[1]) +
    geom_smooth(method = 'lm', formula = y ~ x) +
    facet_wrap( ~ hgroup, ncol = 6, scale = "free_y") +
    #facet_wrap(~ hgroup, ncol = 6) +
    #geom_text(data = regr_hdata, aes(x, y = max(df8$ratio), label = lm_label, hjust = 0, vjust = 1.1)) +
    geom_text(data = regr_hdata,
              aes(
                x,
                y,
                label = lm_label,
                hjust = 0,
                vjust = 1.1
              ),
              size = 4) +
    # facet_grid(hgroup ~ ., scale = "free_y") +
    scale_x_datetime(labels = date_format("%d.%m (%a)"),
                     minor_breaks = date_breaks("1 days")) +
    labs(x = "Дата", y = "Ratio, %")
  
  gp8
  
  # Open a new png device to print the figure out to (or use tiff, pdf, etc).
  png(
    filename = "04. hours_ditributon.png",
    width = 1485,
    height = 1050,
    units = 'px'
  )
  print(gp8) #end of print statement
  dev.off() #close the png device to save the figure.
}


# =================================================
baseline_raw_plot <- function (df, visual.scale = 0.9) {
  df_open <- df %>%
    select(timestamp, value) %>%
    melt(id.vars = c("timestamp"))
    # filter(!is.na(value)) # для корректного отображения данных опустим все, что NA
  
  # считаем верхний и нижний уровень допустимых значений
  dev <- .15 # 15%
  #mindev <- max(df$baseline, na.rm = TRUE) * .05
  #print(mindev)
  mindev <- 3 # считаем в абсолютных значениях. Максимальная загрузка у нас 100%
  
  df_baseline <<- df %>%
    select(timestamp, baseline, value)
  
  df_baseline$low <<- unlist(lapply(df$baseline, function(x) max(0, (x - max(x * dev, mindev)) )))
  df_baseline$up <<- unlist(lapply(df$baseline, function(x) (x + max(x * dev, mindev)) ))
  
  # DEBUG
  df_baseline$dev_delta <<- unlist(lapply(df$baseline, function(x) max(x * dev, mindev) ))

# df_baseline <<- df %>%
#     mutate(low = baseline - max(baseline * dev, mindev),
#            up = baseline + max(baseline * dev, mindev))
    
  cpal <- brewer.pal(7, "Blues")
  cpal <- brewer.pal(7, "Oranges")
  cpal <- brewer.pal(7, "Greens")
  cpal <- brewer.pal(7, "RdPu")
  cpal <- brewer.pal(7, "YlOrBr")
  
  # browser()
  
  gp <<- ggplot(data = df_open, aes(x = timestamp, y = value)) +
    theme_bw() +
    theme(legend.position="none") +
    scale_colour_brewer(palette="Dark2") +
    theme(axis.text.x = element_text(size=rel(visual.scale)), 
          axis.text.y = element_text(size=rel(visual.scale)),
          axis.title.x = element_text(size=rel(visual.scale)),
          axis.title.y = element_text(size=rel(visual.scale))) +

    # добавляем отдельно baseline +-10%
    # в случае, если все значения baseline = NA, то в geom_ribbon появляется ошибка "'x' and 'units' must have length > 0"
    geom_ribbon(data = df_baseline, aes(x = timestamp, ymin = low, ymax = up), fill=cpal[2], na.rm = TRUE) +
    geom_point(size = 4, fill = "white", shape = 21, na.rm = TRUE) +    # White fill
    geom_line(size = 1, aes(color = variable), na.rm = TRUE) +
    # добавляем поверх baseline
    geom_line(data = df_baseline, aes(x = timestamp, y = baseline), size = 1, color = "red", linetype="dashed", na.rm = TRUE) +
    
    # scale_color_manual(values = wes_palette("Moonrise2")) +
    # дельта в 3 часа при выводе на суточном графике
    # http://stackoverflow.com/questions/10339618/what-is-the-appropriate-timezone-argument-syntax-for-scale-datetime-in-ggplot
    scale_x_datetime(labels = date_format_tz("%d-%m-%Y %H:%M", tz="Europe/Moscow"), minor_breaks = date_breaks("1 days")) +
    labs(x="Дата", y="Interface load, %")
  
  # %d.%m (%a)  
  
  gp
}

hgroup.enum <- function(date){
  hour(date) * 100 + floor(minute(date) / 15)
}

precalc_df <- function () {
  # вычисляем суточный интеграл для каждой записи, чтобы потом иметь возможность исключения
  # даже по фрагментарным данным дней с выбросами
  # интегрирование методом трапеций
  # считаем, что измерений достаточно, чтобы посчитать интеграл y(x) методом трапеций
  # https://chemicalstatistician.wordpress.com/2013/12/14/conceptual-foundations-and-illustrative-examples-of-trapezoidal-integration-in-r/
  # http://stackoverflow.com/questions/24813599/finding-area-under-the-curve-auc-in-r-by-trapezoidal-rule
  # http://svitsrv25.epfl.ch/R-doc/library/caTools/html/trapz.html
  df0 <- subdata %>%
    group_by(date) %>%
    # интеграл методом трапеций по суткам, trapz из пакета caTools
    # также считаем среднюю загрузку по дню для последующего расчета отношения
    summarise(
      integr = trapz(timestamp, value),
      # вообще-то, так считать среднее неверно. мы же не уверены, что весь поток данных получен. Правильно интеграл на время делить
      # mean = mean(value),
      mean_value = integr/(24*60) #!!!!!
    )
  subdata <<- dplyr::left_join(subdata, df0, by = "date")
  #browser()
  df0
}

# функция для отображения 
# http://stackoverflow.com/questions/28162486/display-regression-slopes-for-multiple-subsets-in-ggplot2-facet-grid
# http://stackoverflow.com/questions/19699858/ggplot-adding-regression-line-equation-and-r2-with-facet
lm_eqn = function(df){
  # 1. [ggplot2 Quick Reference: geom_abline](http://sape.inf.usi.ch/quick-reference/ggplot2/geom_abline), примеры есть [здесь](http://docs.ggplot2.org/current/geom_abline.html)
  # - slope - (required) slope of the line (the "a" in "y=ax+b")
  # - intercept - (required) intercept with the y axis of the line (the "b" in "y=ax+b").
  
  # print(df)
  # browser()

  # здесь нам надо тоже убедиться, что регрессию можно построить
  m = lm(formula = ratio ~ date, data = df);
  # путем экспериментов убедился, что при наличии только одной точки Slope получает значение NA
  
  #print(m)
  #dm <<- m
  #dd <<- coefficients(m)
  intercept <- as.numeric(coef(m)[1])
  slope <- as.numeric(coef(m)[2])
  r2 <- as.numeric(summary(m)$r.squared)
#   intercept <- signif(coef(m)[1], digits = 2)
#   slope <- signif(coef(m)[2], digits = 2)
#   r2 <- signif(summary(m)$r.squared, 3)
  #lm_label <- as.character(paste("y=", slope, "*x", intercept, ", r2=", r2, sep = ''))
  lm_label <- sprintf("y=%.2e*x%+.2f, r2=%.2f", signif(slope, 2), signif(intercept, 2), signif(r2,3))
  # теперь посчитаем координаты на отображение
  x <- min(df$date)
  y <- max(df$ratio)
  # https://cran.r-project.org/web/packages/dplyr/vignettes/data_frames.html
  dplyr::data_frame(x, y, lm_label, slope, intercept, r2)
}

tfun = function(df){
  print(df)
}

generate.discrete <- function(df) {
  # на вход получаем df с колонками
  # timestamp = POSIXct
  # value
  
  # по хорошему, тут надо использовать фукнциональный подход..., но это тестовая функция
  # а теперь попробуем сделать интерполяцию по 15-ти минутным промежуткам
  # http://stackoverflow.com/questions/16011790/add-missing-xts-zoo-data-with-linear-interpolation-in-r
  # http://www.noamross.net/blog/2014/2/10/using-times-and-dates-in-r---presentation-code.html
  # seq(dmy("01-01-2015"), dmy("02-01-2015"), "15 mins")
  # seq(round_date(min(df$timestamp), "hour"), max(df$timestamp), "15 mins")

  # ZOO: Z's Ordered Observations
  # создали последовательность временных интервалов по которым делаем интерполяцию
  # создали временной ряд по данным
  tseq <- seq(round_date(min(df$timestamp), "hour"), max(df$timestamp), "15 mins")
  # не совсем понятно, почему не получается поставить > 15 мин, не генерится базовая линия :(
  # IN
  tt <- zoo(df$value, order.by = df$timestamp)
  # na.approx\na.spline
  v_seq <- na.approx(object = tt, xout = tseq)
  
  # превращаем zoo обратно в data.frame
  # http://stackoverflow.com/questions/14064097/r-convert-between-zoo-object-and-data-frame-results-inconsistent-for-different
  # df2b <- data.frame(timestamp=time(t2), t2, check.names=FALSE, row.names=NULL)
  
  # поскольку все данные новые, фактически надо пересобирать data.frame
  # Use Function time() to return all dates corresponding to a series index(z) or equivalently
  # Use Function coredata() to return all values corresponding to a series index(z) or equivalently
  r.dev <- runif(length(v_seq), 0.99, 1.01) # добавляем небольшой случайный разброс
  testdata <-
    dplyr::data_frame(
      timestamp = tseq,
      value = coredata(v_seq) * r.dev 
    )
  
  testdata
}
