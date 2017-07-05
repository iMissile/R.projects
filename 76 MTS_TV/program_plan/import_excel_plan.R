library(tidyverse)
library(readxl)
library(stringi)
library(profvis)
library(anytime)


pplan <- "./data/������_��_DVBC_2017-05-02_small.xlsx"
pplan <- "./data/������_��_DVBS_2017-04-03.xlsx"
pplan <- "./data/������_��_IPTV_2017-04-03.xlsx"

tmp <- excel_sheets(pplan)
sheets <- tmp[!stri_detect_fixed(tmp, c('��', 'AC'))]
# sheets <- tmp[!stri_detect_fixed(tmp, c('AC'))]

# ���������� ��� ����� --------------------
ptype <- case_when(
  "IPTV" %in% sheets  ~ "iptv",
  "DVB-S" %in% sheets ~ "dvbs",
  length(sheets) > 8 ~ "dvbc",
  TRUE                ~ "unknown"
)

ptype

# ������ ������� ���� ---------------------
pdate <- stri_extract_first_regex(pplan, pattern="\\d{4}-\\d{2}-\\d{2}")
pdate <- Sys.Date() # ����� ����� ��������� � ��������
# dput(anytime(date))

parseSheet <- function(sheet_name, fname){
  
  print(paste0(fname, " - ", sheet_name))
  
  df0 <- read_excel(pplan, sheet=sheet_name, skip=1) %>% # ���������� �����
    # select(title=`������������ ������`, epg_id=`EPG ID`, genre=`����`) %>%
    select(row_num=`#`, title=`������������ ������`, epg_id=`EPG ID`) %>%
    # filter(complete.cases(.)) %>%
    mutate(city=sheet_name)
    #mutate(row_num=row_number()+) %>% # +2 ������ ��� ������� ������ ������, � ������ ���� � ���������
  
  df0
} 

# ��������� ��� ��������� else, ��� ������ ���������
if(ptype == "dvbc") {
  # ������� 2, ���� ������: http://readxl.tidyverse.org/articles/articles/readxl-workflows.html
  # ��� ������� �� 20% � ������ � 3 ���� ������ ���������
  # �� ������� ������� �������� �������� ���������� � ����, ����� ������ �����, � 4-6 ��� ������.
  df0 <- sheets %>%
    purrr::map_df(parseSheet, fname = pplan) %>%
    mutate(timezone=0)
}
if(ptype == "dvbs") {
  df0 <- read_excel(pplan, sheet="DVB-S", skip=1) %>% # ���������� �����
    select(row_num=`#`, title=`������������ ������`, epg_id=`EPG ID`, timezone=`��� ����`) %>%
    mutate(city='') %>%
    mutate(timezone=as.numeric(stri_extract_first_regex(timezone, pattern="\\d+"))) %>%
    replace_na(list(timezone=0))
}
if(ptype == "iptv") {
  df0 <- read_excel(pplan, sheet="IPTV", skip=1) %>% # ���������� �����
    select(row_num=`#`, title=`�������� ������`, epg_id=`EPG ID`) %>%
    mutate(city='') %>%
    mutate(timezone=0)
}


# ����� �������������� --------------
df1 <- df0 %>%
  mutate(epg_id=stri_trim_left(epg_id, pattern="\\P{Wspace}")) %>% # ������ ���������� �������
  mutate(date=pdate) %>%
  mutate(type=ptype) %>%
  select(date, row_num, epg_id, title, city, everything())

# ���������� �������� ������������ ---------
# �������� ������ �� �����, � ����� ������� �����������
clean_df <- df1 %>%
  filter(!is.na(epg_id)) %>%
  filter(stri_startswith_fixed(epg_id, 'epg')) %>%
  distinct()

bad_df <- anti_join(df1, clean_df)  

stop()

write_excel_csv(df, "program_plan_fast.csv", na="NA", append=FALSE, col_names=TRUE)
# write_delim(df, "program_plan.csv", delim="\t", na="NA", append=FALSE, col_names=TRUE)
write.table(df, "program_plan_slow.csv", na="NA", append=FALSE, col.names=TRUE, row.names=FALSE, sep=";")


na_df <- df %>%
  filter(is.na(epg_id))

badepg_df <- df %>%
  # filter(!str_detect(epg_id, '\\d{3}-\\d{7}$')) %>%
  filter(!stri_startswith_fixed(new_epg_id, 'epg'))


df2 <- df %>% 
  filter(complete.cases(.)) %>%
  distinct()


# ����������� �� �������
# 1. ������ � NA ���� �� ������� ����������. ������ ���� ��� �����-�� �����, � ��� ���� ���-�� ���-�� ������ - ��� ����� ��������� ������� ��������
# 2. ��� ���������� ������, � ������� ������ epg_id ���� "-"

stop()
# ---------------
t <- read_excel(pplan, sheet=sheets[2], skip=1) # ���������� �����
df0 <- t %>%
  select(title=`������������ ������`, epg_id=`EPG ID`, genre=`����`) %>%
  filter(complete.cases(.))