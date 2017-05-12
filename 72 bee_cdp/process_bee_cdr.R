library(tidyverse)
library(forcats)
library(magrittr)
library(stringi)
library(ggthemes)
library(scales)
library(RColorBrewer)
library(hrbrthemes)
library(lubridate)
library(fasttime)
library(anytime)
library(profvis)
library(tictoc)
library(microbenchmark)

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
  mutate(timestamp=readr::parse_datetime(channel_seizure_date_time, format="%Y%m%d%H%M%S")) %>%
  select(-channel_seizure_date_time, -record_id) %>%
  select_if(function(x) !all(is.na(x))) %>% # удаляем колонки только с NA значениями
  select_if(function(x) n_distinct(x)>1) %>% # удалим колонки у которых нет уникальных значений
  select(timestamp, everything()) 

toc()

tic()
# посчитаем количество уникальных значений в колонках
dist_cols <- map_df(df, n_distinct)
toc()



# эксперименты с данными ---------
# посчитаем кол-во записей по каждому no_ggsn
a_df <- df %>%
  group_by(calling_no_ggsn) %>%
  summarise(n=n()) %>%
  arrange(desc(n))


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
