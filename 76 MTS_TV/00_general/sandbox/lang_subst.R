library(tidyverse)

dict_df <- read_csv("rus, enu
                    Стол, Table
                    Шляпа, Hat", locale=locale("ru", encoding="windows-1251"), trim_ws=TRUE)

df <- read_csv("enu 
               Table 
               Wizdom", locale=locale("ru", encoding="windows-1251"), trim_ws=TRUE)

# для отсутствующих переводов получаем NA
df0 <- df %>%
  left_join(dict_df, by="enu") %>%
  select(rus, enu) # поменяли порядок


df1 <- df0 %>%
  # санация, для уже определенной колонки ее позиция остается неизменной
  mutate(rus=if_else(is.na(rus), enu, rus))

df2 <- df0 %>%
  # санация, для уже определенной колонки ее позиция остается неизменной
  select(enu, rus) %>%
  mutate(enu=if_else(is.na(rus), enu, rus))


stop()
  # санация
  mutate(name_rus={map2_chr(.$name_rus, .$name_enu, 
                            ~if_else(is.na(.x), .y, .x))})
