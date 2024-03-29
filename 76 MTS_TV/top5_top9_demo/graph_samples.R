library(tidyverse)
library(lubridate)
library(tibble)
library(purrr)
library(magrittr)
library(forcats)
library(microbenchmark)
library(anytime)
library(Cairo)
library(tictoc)
library(RColorBrewer)
library(hrbrthemes)


eval(parse("funcs.R", encoding="UTF-8"))


system.time(rawdf <- readRDS("./data/tvstream3.rds"))

df <- rawdf %>%
  select(-data_date) %>% # data_date ������ ����� ����� NA
  filter(complete.cases(.)) %>% # �������� ������ � NA
  sample_frac(0.2) %>% # � ����� ��������� ������� ������ �����
  # mutate(t=as.POSIXct(date, origin='1970-01-01'))
  # mutate(t=anytime(date/1000, tz="Europe/Moscow"))
  # ������, ��� timestamp = date, ������ ������ ������. ������� �������, ����� ������ �� ��������
  mutate(timestamp=anytime(date/1000, tz="UTC")) %>%
  mutate(timegroup=hgroup.enum(timestamp, min_bin=60)) %>%
  select(-date)

object.size(df)/1024/1024

unq <- map(select(df, -timestamp), unique)

# ��������� ���-5 ������� ��� ���-9 �������� =============

# top_n ����������� ��� ������ ������, ������� �������  ���������� ���������� ��� N �� ��������
# ������� �������� �������� ������� c ������� �������� �������
reg_df <- df %>%
  group_by(region) %>%
  summarise(duration=sum(duration), n=n()) %>%
  top_n(9, duration) %>%
  arrange(desc(duration))

# � ������ ������� �� ��������� ��������� ������ ������, ���������� ���� ��� N ��������
df1 <- df %>%
  semi_join(reg_df, by="region") %>%
  group_by(region, channelId) %>%
  summarise(duration=sum(duration)) %>%
  ungroup() %>%
  # ����������� ������� ���� ����� �� ������������ ��������� ������������
  group_by(region) %>%
  mutate(total_duration=sum(duration)) %>%
  # top_n(5, duration) %>% # ��� ������ �� ��������
  ungroup() %>%
  arrange(desc(total_duration), desc(duration)) %>%
  # 3. Add order column of row numbers
  mutate(order=row_number())

windowsFonts(robotoC="Roboto Condensed")

# To change the order in which the panels appear, change the levels
# of the underlying factor.

# ������������ ����������� ��� ����������� ����������� ������ facet
# https://drsimonj.svbtle.com/ordering-categories-within-ggplot2-facets
df2 <- df1 %>% 
  group_by(region) %>% 
  top_n(5, duration) %>%
  ungroup() %>%
  arrange(desc(total_duration), desc(duration)) %>%
  # mutate_at(vars(channelId, region), as.factor) %>%
  mutate(order=row_number())
  
# ������� ��� warnings():
assign("last.warning", NULL, envir = baseenv())

# ����������� ���-5 �������� �� ��������� �������� ====================================
# 45 ����� � 34 ������� �� ���������!
# gp <- ggplot(df2, aes(fct_reorder(channelId, duration, .desc=TRUE), duration)) +
# gp <- ggplot(df2, aes(order, duration)) +
gp <- ggplot(df2, aes(fct_reorder(as.factor(order), order, .desc = TRUE), duration)) +
# gp <- ggplot(df2, aes(x=fct_reorder(channelId, order, .desc = TRUE), y=duration)) +  
  geom_bar(fill=brewer.pal(n=9, name="Blues")[4], alpha=0.5, stat="identity") +
  # geom_text(aes(label=order), vjust=-0.5, colour="red") + # ��� ������������
  geom_text(aes(label=order), hjust=+0.5, colour="red") + # ��� ������������
  scale_x_discrete("��������", breaks=df2$order, labels=df2$channelId) +
  #scale_x_discrete("��������", labels=df2$channelId)
  #scale_x_manual("��������", values=df2$channelId)
  # scale_x_discrete(labels=channelId)) +
  #facet_wrap(~fct_reorder(CP, n, .desc=TRUE), scales = "free_y") +
  #facet_wrap(~CP_order, scales = "free_y") +
  facet_wrap(~fct_reorder(region, total_duration, .desc = TRUE), scales = "free") +
  theme_ipsum_rc(base_size=14, axis_title_size=12) +  
  theme(axis.text.x = element_text(angle=90)) +
  ylab("��������� ���������� �����") +
  ggtitle("���������� �������������", subtitle="��� 5 ������� ��� ��� 9 ��������") +
  coord_flip()


gp


