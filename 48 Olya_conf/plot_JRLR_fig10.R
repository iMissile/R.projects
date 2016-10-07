library(ggplot2)
library(ggdendro)
library(lubridate)
library(ggrepel)
library(dplyr)
library(readr)
library(purrr)
library(scales)
library(colorRamps)
library(RColorBrewer)
library(arules)
library(iterators)
library(foreach)
library(doParallel)
library(tibble)

setTimeLimit(cpu = Inf, elapsed = Inf, transient = FALSE)

getwd()
angles <- seq(from = 0.01, to = pi/2, by = 0.02)

read_data <- function(angle){
  #cat(n_intens)
  read_delim(paste0("./intens/int(angle=", angle, ").csv"), delim = ";", quote = "\"",
             # гармоника; угол; значение
             col_names = c(
               "harm",
               "intens"
             ),
             col_types = "dd",
             locale = locale("ru", encoding = "windows-1251", tz = "Europe/Moscow"),
             progress = interactive()
  )
}

registerDoParallel(cores = (detectCores() - 1))
getDoParWorkers()

df <- foreach(it = iter(angles), .combine = rbind, .packages='readr') %dopar% {
    temp.df <- read_data(it)
    problems(temp.df)
    temp.df$angle = it
    
    temp.df
  }

df <- df %>% filter(complete.cases(.)) %>%
  mutate(submarker = as.factor(harm %% 4)) %>% 
  mutate(source_harm = harm) %>%
  mutate(harm = harm / (2 * pi * 10)) %>%
  mutate(round_harm = round(harm, 0))

levs <- list(categories = seq(from = 0, to = max(df$harm), length.out = 11))
levs$labels <- paste0(head(ceiling(levs$categories), -1), ' - ', tail(floor(levs$categories), -1))

df1 <- df %>%
  mutate(marker = discretize(harm, method = "fixed",
                             categories = levs$categories,
                             labels = levs$labels)) %>%
  filter(harm >= 4 & harm <= 19) %>%
  mutate(intens = intens/max(intens)) %>%
  filter(intens > 0.07) %>%
  arrange(marker, angle)

plot_palette <- brewer.pal(n = 11, name = "Set3")
plot_palette <- colorRampPalette(brewer.pal(9, "Set1"))

# общий график
pp.base <- ggplot(df1, aes(x = angle, y = intens, shape = as.factor(round_harm))) +
  geom_point(size = 3, alpha = 1) +
  scale_colour_manual(values = colorRampPalette(brewer.pal(9, "Set1"))(16), name = "# гармоники") +
  scale_shape_manual(values = rep(c(1, 19, 6, 4), 4), name = "Harmonic #") +
  scale_x_continuous(breaks = c(0, pi/4, pi/2), 
                     labels = c("0", expression(paste(pi, "/4")), expression(paste(pi, "/2")))) +  
  xlab(expression(paste(alpha))) +
  ylab("Интенсивность") + 
  ylab("Intensity") +
  theme_bw() +
  theme(text = element_text(size = rel(5.5))) +
  theme(legend.text = element_text(size = rel(5)), legend.key.size = unit(0.7, "cm"))

pp <- pp.base +
  facet_wrap( ~ marker, scales="free", ncol = 5)

pp

ggsave("fig10.png", plot = pp, width = 30, height = 15, units = 'cm', dpi = 200)