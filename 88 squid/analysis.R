# library(RCurl)
library(tidyverse)
library(magrittr)
library(lubridate)
library(readxl)
library(httr)
#library(webreadr)
library(anytime)
library(stringi)
library(RColorBrewer)
library(extrafont)
library(hrbrthemes)
library(futile.logger)

eval(parse("funcs.R", encoding="UTF-8"))
windowsFonts(robotoC="Roboto Condensed")

# m <- RCurl::scp("10.0.0.246", path="/var/log/squid/access.log", binary=FALSE, password="_pass_", user="root")
m <- content(httr::GET("http://10.0.0.246/access.log"))
write(m, "./data/acc.log")

stop()
df <- loadSquidLog(m)
skimr::skim(df)
summarytools::descr(df) # только с числами
summarytools::dfSummary(df, plain.ascii=TRUE) # долго
# summarytools::dfSummary(df, plain.ascii=FALSE) # ну очень долго
tableone::CreateTableOne(data=df)


stop()

# m2 <- rawToChar(m, multiple=FALSE)
m <- RCurl::scp("10.0.0.246", path="/var/log/squid/access.log", binary=FALSE, key="id_rsa", user="root")
df0 <- loadSquidLog(m)

# unicode test: я
df0 <- loadSquidLog("./data/access.log")

stop()
# -------------- нарисуем трафик ----

df0 <- loadSquidLog("./data/acc.log")

df <- df0 %>%
  filter(timestamp>now()-minutes(30))
plotTopHostDownload(df0, subtitle="за последние 5 минут")

df <- df0 %>%
  filter(timestamp>now()-days(1)) %>%
  mutate(timegroup=hgroup.enum(timestamp, mins_bin=10)) %>%
  mutate(date=lubridate::date(timegroup)) %>%
  # filter(date==date(now()-days(1))) %>%
  select(timegroup, host, bytes, url) %>%
  group_by(timegroup, host) %>%
  summarise(volume=sum(bytes)/1024/1024*8/(10*60)) %>% # Перевели в Мбит/с
  top_n(10, volume)

gp <- ggplot(df, aes(timegroup, volume)) + 
  # geom_line(aes(color=host), alpha=0.8, lwd=1) +
  # geom_point(aes(color=host), alpha=0.8, shape=1, size=2) +
  geom_area(aes(fill=host), alpha=0.5, position="stack") +
  # geom_area(aes(fill=host), alpha=0.5, position="identity") +
  # scale_y_continuous(trans='log10') +
  scale_color_brewer(palette="Set1") +
  # scale_y_continuous(trans=log2_trans()) +
  # facet_wrap(~host, scales="free_y", ncol=4) +
  # facet_wrap(~host, ncol=4) +
  # guides(colour=g, fill=g, shape=g) +
  # scale_y_log10(breaks=trans_breaks("log10", function(x) 10^x),
  #              labels=trans_format("log10", math_format(10^.x))) +
  #annotation_logticks() +
  scale_x_datetime(labels=date_format_tz("%d.%m\n%H:%M", tz="Europe/Moscow"),
                   breaks=date_breaks("4 hours"), 
                   minor_breaks=date_breaks("1 hours")) +
  theme_ipsum_rc(base_size=16, axis_title_size=14) +
  # theme_ipsum_rc(base_family="robotoC", base_size=16, axis_title_size=14) +
  xlab("Дата, время") +
  ylab("Скорость, Mbit/s") +
  ggtitle("Динамика трафика")

gp

# пытаемся изменить группировку
# в ggplot для ручного управления порядком следования необходимо управлять параметром group
# http://docs.ggplot2.org/current/aes_group_order.html
# -------------- нарисуем Top10 по HOST за последние N минут ----
df <- df0 %>%
  # mutate(timegroup=hgroup.enum(timestamp, mins_bin=60)) %>%
  # mutate(date=lubridate::date(timegroup)) %>%
  # filter(date==date(now()-days(1))) %>%
  filter(timestamp > now()-days(2)) %>%
  # filter(between(timestamp, anytime("2017-09-01"), anytime("2017-10-01")))
  select(host, bytes, url) %>%
  group_by(host) %>%
  summarise(volume=round(sum(bytes)/1024/1024, 1)) %>% # Перевели в Мб
  top_n(10, volume) %>%
  # может возникнуть ситуация, когда все значения top_n одинаковы. тогда надо брать выборку
  filter(row_number()<=10) %>%
  filter(volume>1) %>%
  arrange(desc(volume)) %>%
  mutate(label=format(volume, big.mark=" "))


plot_df <- df %>%
  mutate(host=fct_reorder(host, volume)) %>%
  mutate(host=glue("{host}\n ФИО"))

gp <- ggplot(plot_df, aes(host, volume)) + 
  geom_bar(fill=brewer.pal(n=9, name="Blues")[4], 
           alpha=0.5, stat="identity") +
  #scale_x_discrete(labels=function(x) str_wrap(str_replace_all(x, "foo" , " "), width=40)) +
  geom_label(aes(label=label), fill="white", colour="black", fontface="bold", hjust=+1.1) +
  theme_ipsum_rc(base_size=16, axis_title_size=14) +
  xlab("HOST") +
  ylab("Суммарный Downlink, Мб") +
  ggtitle("ТОП 10 скачивающих", subtitle="По суммарному времени телесмотрения, мин") +
  coord_flip()

gp


# plotTop10Download(df)

stop()

txt <- c("http://yandex.ocsp-responder.com/", "www.yandex.ocsp-responder.com/sa", "www.ya.ru:44") 

txt %>%
  stri_replace_all_regex(pattern=c("^([a-z]*)://", "^www\\.", "([^/]+).+", ":\\d+"),
                         replacement=c("", "", "$1", ""),
                         vectorize_all=FALSE)


txt %>%
  stri_replace_all_regex("^([a-z]*)://", "") %>%
  stri_replace_all_regex("^www\\.", "") %>%
  stri_replace_all_regex("([^/]+).+", "$1")

# =====================
data <- read_log(fname)

data <- read_squid(fname)

data <- read_squid(system.file("extdata/log.squid", package="webreadr"))
