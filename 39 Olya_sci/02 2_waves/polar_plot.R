library(tidyverse)
library(magrittr)
library(ggplot2)
library(fs)
library(data.table)
library(akima)
library(hrbrthemes)
library(extrafont)

# hrbrthemes::import_roboto_condensed()

processFile <- function(fname){

    tidyr::expand_grid(rho = seq(0, 15, by = 0.5), phi = seq(0, pi, by = pi/20)) %>%
    arrange(rho, phi) %>%
    bind_cols(readr::read_csv(fname, col_names = "value"))
}

raw_df <- fs::dir_ls(here::here("data"), glob = "*.csv") %>%
  enframe(name = NULL, value = "fname") %>%
  mutate(area = path_ext_remove(path_file(fname))) %>%
  mutate(data = map(fname, processFile)) %>%
  select(-fname) %>%
  unnest(cols = data) %>%
  mutate(x = rho * cos(phi), y = rho * sin(phi))

clean_df <- raw_df %>%
  # после такого преобразования из полярных в декартовы координаты
  # уберем дубликаты в точке (0; 0)
  as.data.table() %>%
  .[, head(.SD, 1), by = .(area, x, y)] %>%
  as_tibble() %>%
  # исключаем пока непонятные выбросы
  filter(value < 1e5)

# нарисуем график в осях [rho, phi] на декартовой сетке
ggplot(clean_df, aes(rho, phi, z = value)) +
  geom_contour_filled(bins = 10) +
  geom_contour(bins = 10) +
  scale_fill_viridis_d(option = "plasma") + 
  facet_wrap(vars(area))

# рисуем в полярных координатах на декартовой сетке
# делаем интерполяцию на регулярную сетку
ff2 <- function(df){
  df %$%
    akima::interp(x, y, value, duplicate = "mean", nx = 200, ny = 200) %>%
    akima::interp2xyz(data.frame = TRUE)
}
  
interp_df <- clean_df %>%
  nest(data = -area) %>%
  # mutate(across(data), map(ff2))
  mutate(data = map(data, ff2)) %>%
  unnest(data)

gp <- ggplot(interp_df, aes(x, y, z = z)) +
  # geom_density_2d_filled() + 
  # geom_contour_filled(bins = 10, colour = "white", size = 0.1) +
  geom_contour_filled(bins = 10) +
  geom_contour(bins = 10, colour = "white", size = 0.1) +
  coord_fixed(ratio = 1) +
  # scale_fill_viridis_d(option = "plasma") +
  # scale_fill_viridis_d(option = "magma") +
  scale_fill_viridis_d(option = "viridis") + 
  facet_wrap(vars(area)) +
  theme_ipsum_rc()
  

# # подвал --------
# # делаем прямоугольную сетку ---------
# # https://tidyr.tidyverse.org/reference/expand.html
# all_df <- df %>% tidyr::expand(area, x, y)
# df %<>% dplyr::right_join(all_df)
# 
# 
# 
# # посмотрим на возможные оставшиеся проблемные точки
# # count(df, area, x, y) %>%
# #   filter(n > 1)
# df %>%
#   mutate_all(as.character) %>%
#   skimr::skim()
# 
# df %<>% filter(area == "deprolow")
# 
# janitor::get_dupes(df, area, x, y)
# 
# gp <- ggplot(df, aes(x, y, z = value)) +
#   # geom_density_2d_filled() + 
#   geom_contour(bins = 40) # +
#   # geom_contour_filled()
#   # facet_wrap(vars(area))
# 
# 
#   # 
# 
# 
# # \[Rho]i = 0;
# # \[Rho]f = 15;
# # d\[Rho] = 0.5;
# # 
# # phii = 0;
# # phif = Pi;
# # dphi = Pi/20;
# 
# # ---------------
# # https://ggplot2.tidyverse.org/reference/geom_contour.html
# faithfuld %>%
#   # mutate_all(as.character) %>%
#   skimr::skim()
