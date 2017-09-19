library(tidyverse)
# очистим все warnings():
assign("last.warning", NULL, envir = baseenv())


# округл€ем колонки, которые €вл€ютс€ числовыми, но не вход€т в список
df <- tribble(
  ~text, ~val, ~dic_val, ~tdata,
  "—трока 1", 1.7, 1.5, "f",
  "—трока 2", 2.7, 2.5, "g"
  )

# провер€ем что передаетс€ в проверку
myFun <- function(x)
{
  dput(x)
  print(is.numeric(x))
  TRUE
}

df0 <- df %>%
  mutate_if(myFun, as.character)

# прогон€ем кастомную функцию-преобразование
vlist <- c("val", "tdata")

myFun2 <- function(x, val)
{
  # сюда загнали только колонки с именами, прошедшими обработку
  dput(x)
  print(is.numeric(x))
  if(is.numeric(x)) as.character(x) else "3"
}

df1 <- df %>%
  mutate_at(vars(vlist), myFun2, val="доп. параметр")

# теперь сокращенно
df2 <- df %>%
  mutate_at(vars(vlist), ~{if(is.numeric(.x)) as.character(.x) else "3"})

identical(df1, df2)




