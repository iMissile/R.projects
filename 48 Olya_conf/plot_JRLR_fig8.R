library(ggplot2)
library(ggdendro)
library(lubridate)
library(dplyr)
library(readr)
library(purrr)
library(scales)
library(RColorBrewer)
library(arules)
library(iterators)
library(foreach)
library(doParallel)
library(tibble)

setTimeLimit(cpu = Inf, elapsed = Inf, transient = FALSE)

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
             locale = locale("ru", encoding = "windows-1251", tz = "Europe/Moscow"),
             progress = interactive()
  )
}

registerDoParallel(cores = (detectCores() - 1))
getDoParWorkers()

df <- foreach(it = iter(angles), .combine = rbind, .packages='readr') %dopar% {
  temp.df <- read_data(it)
  problems(temp.df)  

  temp.df
}

df <- df %>% filter(complete.cases(.)) %>%
  mutate(harm = harm / (2 * pi * 10))

levs <- list(categories = seq(from = 0, to = max(df$harm), length.out = 11))
levs$labels <- paste0(head(ceiling(levs$categories), -1), ' - ', tail(floor(levs$categories), -1))

df1 <- df %>%
  mutate(s = value) %>%
  mutate(marker = discretize(harm, method = "fixed", 
                             categories = levs$categories, 
                             labels = levs$labels)) %>%
  filter(harm < 10000) %>%
  arrange(value)

palette <- brewer.pal(9, "Greys") # "Spectral"
cpalette <- brewer.pal(11, "Spectral")[c(10, 2)]
# cfpalette <- colorRampPalette(c("red", "blue"))

pp.base <- ggplot(df1, aes(x = harm, y = angle, colour = value, fill = value, size = value)) +
  guides(size = FALSE) + # загасим легенду
  geom_point(shape = 21, alpha = 0.4) + # 0.6 для цвета
   scale_colour_gradientn(colours = cpalette, guide = FALSE) +
   scale_fill_gradientn(colours = cpalette,  
                       guide="colorbar", name = "") +
  geom_hline(yintercept = pi/4, lwd = 0.8, color = "Black", linetype = 'dashed') +
  xlab("Harmonic") +
  ylab(expression(paste(alpha, ", rad"))) +
  theme_bw()

pp0 <- pp.base + 
  scale_size_continuous(range = c(1.5, 4.5)) + # управляем диапазоном размера точек
  scale_x_continuous(breaks = pretty_breaks(n = 20))
  
pp <- pp.base +
  scale_size_continuous(range = c(0.2, 4.5)) + # управляем диапазоном размера точек
  theme(text = element_text(size = rel(5))) +
  theme(legend.text = element_text(size = rel(5)), legend.key.size = unit(2, "cm")) +
  facet_wrap( ~ marker, scales="free", ncol = 5)

pp

ggsave("fig8.png", plot = pp, width = 30, height = 15, units = 'cm', dpi = 200)

# dev.off()