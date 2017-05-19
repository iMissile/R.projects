library(tidyverse)
library(forcats)
library(magrittr)
library(stringi)
library(ggthemes)
library(scales)
library(RColorBrewer)
library(viridis)
library(hrbrthemes)
library(lubridate)
library(fasttime)
library(anytime)
library(profvis)
library(tictoc)
library(microbenchmark)

eval(parse("funcs.R", encoding="UTF-8"))

fields <-
  flatten_chr(
    stri_split_regex(
      "record_type,subscriber_no,channel_seizure_date_time,
      message_switch_id,us_seq_no:n,at_feature_code,call_action_code,
      feature_selection_dt,at_call_dur_sec:n,call_to_tn_sgsn,calling_no_ggsn,
      mps_file_number:n,message_type,duration,guide_by,outcollect_ind,
      catchup_ind,data_volume:n,original_amt:n,original_amt_gn:n,imsi,imei,event_type:n,
      cell_id,ac_amt:n,call_forward_ind,lac,provider_id,call_source,rm_tax_amt_air:n,
      waived_call_ind,basic_service_code,basic_service_type,dialed_digits,
      record_id,tax_id,calculate_uc_rate_ind,sdr_amount:n,uom,supplementry_srvc_code,
      home_ctn,technology,chanel_type,transparency_ind,ms_classmark,ss_param_ip_address,
      original_call_type,original_call_npi,original_call_number,sdr_exchange_rate:n,
      dual_service_type,dual_service_code,camel_served_address,camel_service_key,
      camel_msc_address,camel_ref_number,camel_dest,msc_chrg_type_ind,file_name,
      country_of_orig,rec_status,called_country_code,camel_charge:n,sdr_camel_charge:n,
      from_provider_id,call_destination,ss_action_code,new_balance:n,charging_id:n,tap_data",
      ",\\p{WHITE_SPACE}*"
    )
  )

cdr_spec <- tibble(fields) %>% 
  separate(fields, into=c("cname", "ctype"), sep=":") %>% 
  replace_na(list(ctype = "c"))

# ручками создаем строку парсинга
if (FALSE){
# t <- stri_dup("c", length(cnames))
# stri_sub(t, from=c(1, 3), length=1) <- "f"
t <- rep("c", length(cnames))
t[c(1,3,5)] <- "i"
stri_flatten(t)
}


# cdr_list <- dir(path="./bee_cdr/", pattern="cdr_example.*", full.names=TRUE)
cdr_list <- dir(path="./bee_cdr/data/", pattern="CDP_.*[.]log", full.names=TRUE)

process_xDR <- function(fname, col_names, col_types, ...){
  # выносим процесс загрузки в отдельный файл для того, чтобы иметь возможность делать потом прогресс бар и логирование
  cat(fname)
  # для упрощения задачи приходится ручками задать формат строки
  df <- read_delim(fname, col_names=col_names, delim='|', col_types=col_types)
  # problems(df)
  s <- spec(df)
  # print(s)
  df
}

tic("Parsing")
df0 <- cdr_list %>% 
  #head(2) %>%
  purrr::map_df(process_xDR, cdr_spec$cname, stri_flatten(cdr_spec$ctype), .id = NULL)
toc()

tic("Postprocessing")

df <- df0 %>%
  repair_names() %>%
  filter(record_type=="01") %>% # выкинули закрывающие строчки и потенциальный хлам
  # mutate(timestamp=readr::parse_datetime(channel_seizure_date_time, format="%Y%m%d%H%M%S")) %>%
  mutate(timestamp=lubridate::parse_date_time(channel_seizure_date_time, orders="%Y%m%d%H%M%S", tz="Europe/Moscow")) %>%
  # select(-channel_seizure_date_time, -record_id) %>%
  # очистили шлак и дубли после предварительного анализа данных
  select(-message_switch_id, -us_seq_no, -at_feature_code, -catchup_ind, -call_action_code, -call_destination) %>%
  select(-subscriber_no, -call_to_tn_sgsn) %>%
  #  --
  select_if(function(x) !all(is.na(x))) %>% # удаляем колонки только с NA значениями
  select_if(function(x) n_distinct(x)>1) %>% # удалим колонки у которых нет уникальных значений
  select(timestamp, everything()) 

toc()

tic()
# посчитаем количество уникальных значений в колонках
dist_cols <- map_df(df, n_distinct)
# посчитаем словарные уникальные значения
unq <- map(select(df, -timestamp, -calling_no_ggsn), unique)
toc()

# эксперименты с данными ---------
# посчитаем кол-во записей по каждому no_ggsn
a_df <- df %>%
  group_by(calling_no_ggsn) %>%
  summarise(n=n()) %>%
  arrange(desc(n))

tic()
write_csv(df, "CDP_result.log.gz")
toc()

# попробуем отобразить графики. предварительная подготовка ================
df %<>%
  mutate(timegroup=hgroup.enum(timestamp, min_bin=10)) %>%
  mutate(CP=calling_no_ggsn)

# top_n применяется для каждой группы, поэтому сначала  необходимо определить ТОП N по контент провайдерам.
# выберем наиболее активных контент-провайдеров
cp_df <- df %>%
  group_by(CP) %>%
  summarise(n=n()) %>%
  top_n(9, n) %>%
  arrange(desc(n))

# а теперь выберем из исходного материала только данные, касающиеся этих ТОП N
df2 <- df %>%
  semi_join(cp_df, by="CP") %>%
  group_by(timegroup, CP, message_type) %>%
  summarise(n=n()) %>%
  ungroup() %>%
  # сортировать контент-провайдеров надо по максимальному числу SMS (O/S/T)
  group_by(CP) %>%
  mutate(max_n=max(n)) %>%
  ungroup() %>%
  arrange(desc(max_n)) %>%
  # 3. Add order column of row numbers
  mutate(order=row_number())


windowsFonts(robotoC="Roboto Condensed")

# To change the order in which the panels appear, change the levels
# of the underlying factor.
# df2$CP_order <- reorder(df2$CP, df2$n)
# так тоже не работает, потому что внутри группировка идет по порядку O,S,T. А там максимумы другие.

# Time-Series представление данных ====================================
gp <- ggplot(df2, aes(timegroup, n, colour=message_type)) + 
  geom_point(alpha=0.85, shape=1, size=3) +
  geom_line(alpha=0.85, lwd=1) +
  # theme_ipsum_rc(base_family="robotoC", base_size=16, axis_title_size=14) +
  scale_x_datetime(breaks=date_breaks("2 hour")
                   #minor_breaks = date_breaks("6 hours"),
  ) +
  #facet_wrap(~fct_reorder(CP, n, .desc=TRUE), scales = "free_y") +
  #facet_wrap(~CP_order, scales = "free_y") +
  facet_wrap(~fct_reorder(CP, max_n, .desc = TRUE)) +
  # theme_ipsum_rc(base_family="robotoC", base_size=14, axis_title_size=12) +
  theme_ipsum_rc(base_size=14, axis_title_size=12) +
  theme(axis.text.x = element_text(angle=90)) +
  xlab("Дата, время") +
  ylab("Количество CDR за 10 мин") +
  ggtitle("Динамика обмена контентом")

gp

# очистим все warnings():
assign("last.warning", NULL, envir = baseenv())

# Heat-map представление данных ================================
df3 <- df2 %>%
  mutate(timegroup=hgroup.enum(timegroup, hour_bin=1)) %>% # тоже сделали часовую группировку
  group_by(timegroup, CP, message_type) %>%
  summarise(n=sum(n)) %>%
  mutate(hour=as.numeric(format(timegroup, "%H", tz="Europe/Moscow"))) %>%
  ungroup()
  


gg = ggplot(df3, aes(x=hour, y=fct_reorder(CP, n, .desc=FALSE), fill=n))
gg = gg + geom_tile(color="white", size=0.1)
gg = gg + scale_fill_viridis(option="B", name="CDR в час", label=comma) # https://cran.r-project.org/web/packages/viridis/vignettes/intro-to-viridis.html
#gg = gg + scale_fill_distiller(palette="RdYlGn", name="CDR в час", label=comma) # http://docs.ggplot2.org/current/scale_brewer.html
#gg = gg + coord_equal()
gg = gg + coord_fixed(ratio = 1)
gg = gg + labs(x=NULL, y=NULL, title="KPI по часам и дням недели")
gg = gg + theme_tufte(base_family="Verdana")
gg = gg + theme(plot.title=element_text(hjust=0))
# gg = gg + theme(plot.background=element_rect(fill = "transparent", #"lightblue", 
#                                              colour = "black", size = 1,
#                                              linetype="longdash"))
gg = gg + facet_wrap(~message_type, ncol=1)

gg

if (FALSE){
# проверим какой из вариантов по отбросу полностью NA колонок быстрее
f1 <- function(df) {
  Filter(function(x) !all(is.na(x)), df)
}

f2 <- function(df) {
  df %>% select_if(function(x) !all(is.na(x)))
}

microbenchmark(f1 = f1(df), f2 = f2(df))


df1 <- f1(df)
df2 <- f2(df)
identical(df1, df2) # TRUE!!!!

df1 %<>% mutate(timestamp=anytime(channel_seizure_date_time))

rm(df2)
}
# http://r4ds.had.co.nz/lists.html


stop()

df3 <- df2 %>%
  arrange(desc(n))
