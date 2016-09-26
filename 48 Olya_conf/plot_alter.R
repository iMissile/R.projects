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
library(doParallel) # http://blog.aicry.com/r-parallel-computing-in-5-minutes/
library(tibble)


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

# упоминание 2014 года: http://www.vesnam.com/Rblog/existing-code-parallelization-yes-or-no/
registerDoParallel(cores = (detectCores() - 1)) # http://blog.aicry.com/r-parallel-computing-in-5-minutes/
# registerDoParallel(cores = 1)
getDoParWorkers()

# http://brianmannmath.github.io/blog/2014/01/20/using-lapply-to-import-files-to-r/
# df <- lapply(angles, read_data)
# пробегаемся по строчкам data.frame
# http://stackoverflow.com/questions/1699046/for-each-row-in-an-r-dataframe
# df <- foreach(it = iter(angles), .combine = rbind) %do% { # %dopar% { # %dopar% -- запуск параллельного процессинга
df <- foreach(it = iter(angles), .combine = rbind, .packages='readr') %dopar% { # %dopar% -- запуск параллельного процессинга
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
  mutate(s = value) %>%
  mutate(marker = discretize(harm, method = "fixed", 
                             categories = levs$categories, 
                             labels = levs$labels)) %>%
  filter(harm < 10000) %>%
  arrange(value)

# общий график
pp.base <- ggplot(df1, aes(x = harm, y = angle, colour = value, fill = value, size = value)) +
  # geom_point(size = 4, shape = 21, alpha = 0.6) +
  guides(size = FALSE) + # загасим легенду
  geom_point(shape = 21, alpha = 0.6) +
  # === так включаем цветную палитру
  # scale_colour_gradientn(colours = rev(brewer.pal(9, "Spectral")), guide = FALSE) +
  # scale_fill_gradientn(colours = rev(brewer.pal(9, "Spectral")),
  #                      guide="colorbar", name = "") +
  # === так включаем серую палитру
  scale_colour_gradientn(colours = brewer.pal(9, "Greys"), guide = FALSE) +
  scale_fill_gradientn(colours = brewer.pal(9, "Greys"), # "Spectral" 
                      guide="colorbar", name = "") +
  # =====
  # scale_y_continuous(breaks = c(0, pi/4, pi/2), 
  #                    labels = c("0", expression(paste(pi, "/4")), expression(paste(pi, "/2")))) +
  geom_hline(yintercept = pi/4, lwd = 0.8, color = "Black", linetype = 'dashed') +
  xlab("Гармоника") + 
  xlab("Harmonic") +
  ylab(expression(paste(alpha, ", rad"))) +
  theme_bw()

pp0 <- pp.base + 
  # scale_size_continuous(range = c(1.5, 4.5)) + # управляем диапазоном размера точек
  scale_size_continuous(range = c(0.2, 5)) + # управляем диапазоном размера точек
  scale_x_continuous(breaks = pretty_breaks(n = 20))
  
pp <- pp.base +
  scale_size_continuous(range = c(0.5, 3)) + # управляем диапазоном размера точек
  facet_wrap( ~ marker, scales="free", ncol = 5)

pp

# сохраним
ggsave("_ilya_common.png", plot = pp0, width = 30, height = 15, units = 'cm', dpi = 200)
ggsave("_ilya_facet.png", plot = pp, width = 30, height = 15, units = 'cm', dpi = 200)

# dev.off()