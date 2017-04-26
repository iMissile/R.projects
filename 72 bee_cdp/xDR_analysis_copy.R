library(tidyverse)
library(forcats)
library(magrittr)
library(stringi)
library(ggthemes)
library(scales)
library(RColorBrewer)
library(hrbrthemes)
library(lubridate)
library(profvis)

# Reading and combining many tidy data files in R, Jun 13, 2016 in science
# http://serialmentor.com/blog/2016/6/13/reading-and-combining-many-tidy-data-files-in-R

flow_list <- dir(path="D:/iwork.GH/R.projects/67 MTS DPI/data/", pattern="edr_BASE-edr_flow_format_.*", full.names=TRUE)
http_list <- dir(path="D:/iwork.GH/R.projects/67 MTS DPI/data/", pattern="edr_BASE-edr_http_format_.*", full.names=TRUE)


process_xDR <- function(fname, ...){
  # выносим процесс загрузки в отдельный файл для того, чтобы иметь возможность делать потом прогресс бар и логирование
  cat(fname)
  df <- read_delim(fname, delim=',')
  problems(df)
  df
}

df <- http_list %>%
  head(2) %>%
  purrr::map_df(process_xDR, .id = NULL) %>%
  repair_names()
  


# преобразования -----------------------------------------
# очистим имена колонок от кривых символов
fix_names <- names(df) %>%
  stri_replace_all_fixed(pattern=c("#", "-", " "), replacement=c("", "_", "_"), vectorize_all=FALSE)
names(df) <- fix_names

# сэмулируем данные для анализа на основе сэмпла реальных xDR ------------------------------
# посчитаем число уникальных radius_name и замапируем на в два раза меньшее число MSISDN
radius_subst <- distinct(df, radius_user_name)
n_users <- round(nrow(radius_subst)/230)
msisdn <- sample(c(916, 925, 918), n_users, replace=TRUE)*10^7 + floor(runif(n_users, 10^6, 10^7-1))
# сэмулируем подстановку
radius_subst$msisdn <- sample(msisdn, nrow(radius_subst), replace=TRUE)
  
radius_subst %>%
  group_by(msisdn) %>% 
  count() %>%
  arrange(desc(n))

# добавим MSISDN в исходные данные
df1 <- left_join(df, radius_subst, by="radius_user_name")

# а теперь равномерно размажем записи по временному промежутку в сутки. 
# сделаем так, чтобы start_time и end_time отличались на [0-10] сек
time_sample <- function(N, st = "2012/01/01", et = lubridate::now()) {
  st <- as.POSIXct(as.Date(st))
  et <- as.POSIXct(as.Date(et))
  dt <- as.numeric(difftime(et, st, unit = "sec"))
  ev <- sort(runif(N, 0, dt))
  rt <- st + ev
}

df1 %<>% mutate(end_timestamp=time_sample(nrow(.), now()-days(1), now())) %>%
  mutate(start_timestamp=end_timestamp-seconds(runif(nrow(.), -10, 0))) %>% 
  #mutate(start_timestamp=as.POSIXct(sn_start_time, origin="1970-01-01", tz="Europe/Moscow")) %>%
  #mutate(end_timestamp=as.POSIXct(sn_end_time, origin="1970-01-01", tz="Europe/Moscow")) %>%
  mutate(downlink_bytes=as.numeric(transaction_downlink_bytes)) %>%
  mutate(uplink_bytes=as.numeric(transaction_uplink_bytes)) # %>%  
  # select(start_timestamp, end_timestamp, everything())

write_csv(df1, "./Shiny_DPI_reports/edr_http.csv")

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

# отобразим распределение трафика ------------------------------------------------------
df2 <- df1 %>% 
  select(timestamp=end_timestamp, downlink_bytes, uplink_bytes) %>%
  gather(downlink_bytes, uplink_bytes, key="direction", value="bytes") %>%
  sample_frac(0.2) %>%
  filter(bytes>0.1)

windowsFonts(robotoC="Roboto Condensed")

gp2 <- ggplot(df2, aes(timestamp, bytes)) + 
  geom_point(aes(colour=direction), alpha=0.4, shape=1, size=2) +
  theme_ipsum_rc(base_family="robotoC", base_size=14) +
  theme(legend.position="right") +
  scale_y_log10(breaks=trans_breaks("log10", function(x) 10^x),
                labels=trans_format("log10", math_format(10^.x)))

gp2
