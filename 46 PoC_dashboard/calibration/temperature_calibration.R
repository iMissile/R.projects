# калибровка данных аналоговых температурных датчиков, интегрированных с датчиками влажности почвы
#library(tidyr)
library(ggplot2) #load first! (Wickham)
library(ggdendro) # для пустой темы
library(lubridate) #load second!
library(dplyr)
library(readr)
library(tidyr)
library(broom)
#library(jsonlite)
library(magrittr)
library(httr)
library(ggthemes)
#library(ggmap)
library(RColorBrewer) # http://www.cookbook-r.com/Graphs/Colors_(ggplot2)/
library(scales)
library(gtable)
library(grid) # для grid.newpage()
library(gridExtra) # для grid.arrange()
library(curl)

# library(KernSmooth)
#library(akima)
#library(rdrop2)
# library(rgl)

getwd()

df <- read_delim(
  "./calibration/measurement_temp_1.csv",
  delim = ";",
  quote = "\"",
  locale = locale("ru", encoding = "windows-1251", tz = "Europe/Moscow"),
  # таймзону, в принципе, можно установить здесь
  progress = interactive()
) # http://barryrowlingson.github.io/hadleyverse/#5

problems(df)

# https://rpubs.com/bradleyboehmke/data_wrangling
df1 <- df %>% gather(sensor, value, c(s1, s2, s3))


p1 <- ggplot(df1, aes(value, temperature, color = sensor)) +
  # geom_line() +
  geom_point(shape = 2) +
  facet_grid(sensor ~ .) +
  geom_smooth(method=lm,   # Add linear regression lines
              # se=FALSE,    # Don't add shaded confidence region
              fullrange=TRUE) # Extend regression lines

p1

# http://blog.yhat.com/posts/r-lm-summary.html
# 
df1 %>% group_by(sensor) %>% do(tidy(lm(temperature ~ value, .)))


# а теперь нарисуем график до даты выхода на ноль
# я хочу нарисовать прямую по которой была проведена регрессия, а не сплайны.
# 1. [ggplot2 Quick Reference: geom_abline](http://sape.inf.usi.ch/quick-reference/ggplot2/geom_abline), примеры есть [здесь](http://docs.ggplot2.org/current/geom_abline.html)
# - slope - (required) slope of the line (the "a" in "y=ax+b")
# - intercept - (required) intercept with the y axis of the line (the "b" in "y=ax+b").
# 2. Использовать функцию [stat_function()](https://kohske.wordpress.com/2010/12/25/draw-function-without-data-in-ggplot2/)

# http://www.r-tutor.com/elementary-statistics/simple-linear-regression/estimated-simple-regression-equation
fit <- lm(formula = s3 ~ temperature, data = df) 
coeffs <- coefficients(fit); coeffs 

newdata <- data.frame(temperature = c(0, 50))
t <- predict(fit, newdata)
t
# tidy(fit)


# посмотрим занятые объемы памяти
# http://isomorphism.es/post/92559575719/size-of-each-object-in-rs-workspace
# for (obj in ls()) { message(obj); print(object.size(get(obj)), units='auto') }

mem.df <- data.frame(obj = ls(), stringsAsFactors = FALSE) %>% 
  mutate(size = unlist(lapply(obj, function(x) {object.size(get(x))}))) %>% 
  arrange(desc(size))

