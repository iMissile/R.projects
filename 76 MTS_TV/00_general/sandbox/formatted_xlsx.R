library(tidyverse)
library(openxlsx)

df <- read_csv("rus, enu, new_ratio, val, ratio
                Стол, Table, 0.1, 0.1
                Шляпа, Hat, 0.9, 0.9", locale=locale("ru", encoding="windows-1251"), trim_ws=TRUE)

df0 <- df
# так работает
class(df0$val) <- "percentage"
write.xlsx(df0, file="test.xlsx", asTable = TRUE)

# делаем по списку
ratio_vars <- c("ratio")
df1 <- df %>%
  mutate_at(vars(one_of(ratio_vars)), `class<-`, "percentage")
dput(df1)

write.xlsx(df1, file="test1.xlsx", asTable = TRUE)
