library(tidyverse)
library(forcats)
library(magrittr)
library(stringi)
library(ggthemes)
library(RColorBrewer)

# Reading and combining many tidy data files in R, Jun 13, 2016 in science
# http://serialmentor.com/blog/2016/6/13/reading-and-combining-many-tidy-data-files-in-R

flow_list <- dir(path="./data/", pattern="edr_BASE-edr_flow_format.*", full.names=TRUE)
http_list <- dir(path="./data/", pattern="edr_BASE-edr_http_format.*", full.names=TRUE)


# долгий импорт -----------------------------------------
# flow_df <- flow_list %>%
#   purrr::map(read_delim, delim=',')


http_df <- http_list %>%
  purrr::map(read_delim, delim=',')

df <- reduce(http_df, rbind) %>%
  repair_names()

# преобразования -----------------------------------------
# очистим имена колонок от кривых символов
fix_names <- names(df) %>%
  stri_replace_all_fixed(pattern=c("#", "-", " "), replacement=c("", "_", "_"), vectorize_all=FALSE)

names(df) <- fix_names

df1 <- df %>%
  mutate(start_timestamp=as.POSIXct(sn_start_time, origin="1970-01-01", tz="Europe/Moscow")) %>%
  mutate(end_timestamp=as.POSIXct(sn_end_time, origin="1970-01-01", tz="Europe/Moscow")) %>%
  mutate(downlink_bytes=as.numeric(transaction_downlink_bytes)) %>%
  mutate(uplink_bytes=as.numeric(transaction_uplink_bytes)) %>%  
  select(start_timestamp, end_timestamp, everything())

# посмотрим параметры полученных xDR
cat(sprintf("Минимальное время закрытия:  %s\nМаксимальное время закрытия: %s\nUplink:   %s Мб\nDownlink: %s Гб",
            min(df1$end_timestamp), 
            max(df1$end_timestamp),
            round(sum(df1$uplink_bytes)/1024^2, 1),
            round(sum(df1$downlink_bytes)/1024^3, 1)
            )
)

group_df <- df1 %>%
  group_by(radius_user_name) %>%
  summarise(user_recs=n(), 
            uplink_Kb=round(sum(uplink_bytes)/1024, 1), 
            downlink_Kb=round(sum(downlink_bytes)/1024, 1)) %>%
  arrange(desc(user_recs))


# нарисуем TOP-10 пользователей -------------------------------------------------------

plot_df <- group_df %>%
  top_n(10, downlink_Kb) %>%
  mutate(downlink_Mb=downlink_Kb/1024) %>%
  arrange(desc(downlink_Kb))

gp <- ggplot(plot_df, aes(fct_reorder(radius_user_name, downlink_Mb), downlink_Mb)) + 
  geom_bar(fill=brewer.pal(n=9, name="Blues")[4], alpha=0.5, stat="identity") +
  theme_igray() +
  xlab("Raduis хэш") +
  ylab("Суммарный Downlink, Mb") +
  coord_flip()

gp
