library(tidyverse)
library(readxl)
library(lubridate)

df <- read_excel("potok_6250_05_07_2018.xlsx") %>%
  rename(invest_num="Номер счета инвестора", loan_num="Номер счета заемщика", date="Дата",
         amount="Сумма", loan_name="Наименование заемщика", agreement_num="Номер договора") %>%
  mutate(type=recode(.$'Тип операции', "выплата ОД"="payment", "инвестирование"="investment",
                     "пени"="penalties", "проценты"="interest"))

  
df1 <- df %>% 
  # сложим идентичные записи
  select(-"Тип операции", -loan_num, -agreement_num) %>%
  group_by_at(vars(-amount)) %>%
  summarise(amount=sum(amount)) %>%
  ungroup()


# собираем все платежи
df2 <- df1 %>% 
  tidyr::spread(type, amount, fill=0) %>%
  group_by(loan_name) %>%
  summarise(start=min(date), last_p = max(date), investment=sum(investment), payment=sum(payment),
            interest=sum(interest), penalties=sum(penalties)) %>%
  ungroup() %>%
  mutate(dt=as.numeric(today()-as.Date(start))) %>%
  mutate(calc_percent=dt/183, 
         real_percent=payment/investment,
         risk=if_else(real_percent<calc_percent, 1, 0)) %>%
  # фильтруем выплаченные
  filter(real_percent<1 & real_percent>0) %>%
  filter(calc_percent>0.15) %>%
  mutate(scale=real_percent/calc_percent) %>%
  arrange(desc(risk), scale) %>%
  select(-interest, -penalties, -dt)


  
  



Hmisc::describe(df)
