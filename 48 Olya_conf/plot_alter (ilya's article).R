# генерируем тестовые файлы для отладки интерфейса
#library(tidyr)
library(ggplot2) #load first! (Wickham)
library(ggdendro) # для пустой темы
library(lubridate) #load second!
library(dplyr)
library(readr)
library(scales)
library(RColorBrewer)
library(arules)
library(iterators)
library(foreach)
library(ggthemes)


# импорт данных
getwd()
angles <- seq(from = 0.01, to = pi/2, by = 0.02)

read_data <- function(n){
  read_delim(paste0("./alter/finellalter(angle=", n, ").csv"), delim = ";", quote = "\"",
             # гармоника; угол; значение
             col_names = c(
               "harm",
               "angle",
               "value"
             ),
             col_types = "ddd",
             locale = locale("ru", encoding = "windows-1251", tz = "Europe/Moscow"), # таймзону, в принципе, можно установить здесь
             progress = interactive()
  ) # http://barryrowlingson.github.io/hadleyverse/#5
}

# http://brianmannmath.github.io/blog/2014/01/20/using-lapply-to-import-files-to-r/
# df <- lapply(angles, read_data)
# пробегаемся по строчкам data.frame
# http://stackoverflow.com/questions/1699046/for-each-row-in-an-r-dataframe
df <- foreach(it = iter(angles), .combine = rbind) %dopar% {
  # cat("----\n"); str(it);
  temp.df <- read_data(it)
  problems(temp.df)  

  temp.df
}

df <- df %>% filter(complete.cases(.)) %>% # выкинем NA данные
  mutate(harm = harm / (2 * pi * 10))

summary(df)

levs <- list(categories = seq(from = 0, to = max(df$harm), length.out = 11))
levs$labels <- paste0(head(ceiling(levs$categories), -1), ' - ', tail(floor(levs$categories), -1))

# немного причешем
df1 <- df %>%
  # внесем случайные изменения в картинку
  mutate(da = rnorm(angle, 0.2, 0.2)) %>%
  mutate(angle = angle + da) %>%
  mutate(s = value) %>%
  mutate(marker = discretize(harm, method = "fixed", 
                             categories = levs$categories, 
                             labels = levs$labels)) %>%
  filter(harm < 10000) %>%
  filter(harm > 1) %>%
  arrange(value)

# общий график
pp.base <- ggplot(df1, aes(x = harm, y = angle, colour = value, fill = value, size = value)) +
  # geom_point(size = 4, shape = 21, alpha = 0.6) +
  guides(size = FALSE) + # загасим легенду
  geom_point(shape = 21, alpha = 0.6) +
  scale_colour_gradientn(colours = rev(brewer.pal(9,"RdYlBu")), guide = FALSE) +
  scale_fill_gradientn(colours = rev(brewer.pal(9,"RdYlBu")), 
                       guide="colorbar", name = "") +
  scale_y_continuous(breaks = c(0, pi/4, pi/2), 
                     labels = c(expression(paste(-pi, "/4")), expression(paste(0)), expression(paste(pi, "/4")))) +
  geom_hline(yintercept = pi/4, lwd = 0.8, color = "Black", linetype = 'dashed') +
  xlab("Гармоника") + 
  ylab(expression(paste(alpha, ", рад "))) +
  # theme_bw()
  theme_solarized()
  

pp0 <- pp.base + 
  scale_size_continuous(range = c(1.5, 4.5)) + # управляем диапазоном размера точек
  scale_x_continuous(breaks = pretty_breaks(n = 20))
  
pp <- pp.base +
  scale_size_continuous(range = c(0.5, 3)) + # управляем диапазоном размера точек
  # facet_wrap( ~ marker, scales="free", ncol = 5)
  facet_wrap( ~ marker, scales="free_x", ncol = 5)
  # facet_wrap( ~ marker, scales="fixed", ncol = 5)
  # scale_colour_solarized("blue")

pp

# сохраним
ggsave("_ilya_common.jpg", plot = pp0, width = 30, height = 15, units = 'cm', dpi = 600)
ggsave("_ilya_facet.jpg", plot = pp, width = 30, height = 15, units = 'cm', dpi = 600)

# dev.off()