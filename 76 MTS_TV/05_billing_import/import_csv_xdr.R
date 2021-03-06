library(tidyverse)
library(lubridate)
library(readr)
library(stringi)
library(profvis)
library(anytime)
library(config)
library(tictoc)
# library(config)
packageVersion("dplyr")


xdr_file <- "./data/itg_62057_15.07.2017.csv.zip"
getwd()

# [1] "��� ��������",         "�/�"                               "�������� ����� ������������"      
# [4] "������������ ������"   "��� ������"                        "����������"                       
# [7] "��� ������"            "����� �������� � �������� �������" "������ ����������"

raw_df <- read_tsv(xdr_file, locale=locale("ru", encoding="windows-1251"), col_types="ccccccccc")
# ��������� ������ ��������������, �� ������������� ������� ��� � ������ ��� ����������� ������ ���������
print(problems(raw_df), n=Inf)

# ��������������
df0 <- raw_df %>% 
  # ����������� ������� �� ���������� ������������� �������
  setNames(c("billing_code", "account", "equipment_sn", "service_name", 
             "service_code", "charge", "service_type", "payment", "charge_period")) %>%
  # ���������� ������, � ������� ���� ������ ��������
  filter((!row_number() %in% problems(.)$row)) %>%
  # ����������� �������� ���� � �����
  mutate_at(vars(charge, payment), as.numeric) %>%
  mutate_at(vars(charge_period), myd, truncated=1) %>%
  mutate_at(vars(account, equipment_sn), function(x) stri_replace_all_regex(x, "^'", "")) %>%
  mutate(id=row_number())

# ���������� ����������
df1 <- df0 %>%
  separate_rows(equipment_sn) %>% 
  # ��������� ���������� ����������
  filter(!is.na(charge)) %>%
  # ����������� �� ����� SN
  filter(!is.na(equipment_sn)) %>%
  # filter(stri_length(equipment_sn)==11) %>%
  # ����� ������� ������ �� 11 �������� ������ � ���������� �� 001..009
  filter(!is.na(stri_match_first_regex(equipment_sn, "^00[1-9][:digit:]{8}"))) %>%
  # head(50) %>%
  # �������� �������
  group_by(id) %>%
  mutate(split_charge=charge/n(), split_payment=payment/n()) %>%
  ungroup() %>%
  select(equipment_sn, everything()) %>%
  arrange(equipment_sn)
  
    

# count_fields(xdr_file, tokenizer_tsv())

stop()

write_csv(df1, "clean_xdr.csv")

df <- df1
tic("Analysis")
# ��������� ���������� ���������� �������� � ��������
dist_cols <- map_df(select(df, everything()), n_distinct)
# ��������� � ����������� ��������� ���������� ��������
unq <- map(df, unique) %>% map(sort, decreasing=T)
toc()

dist_cols
unq
