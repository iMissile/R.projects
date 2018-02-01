# library(RCurl)
library(tidyverse)
library(magrittr)
library(lubridate)
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
plotTopIpDownload(df0, subtitle="за последние 5 минут")

df <- df0 %>%
  mutate(timegroup=hgroup.enum(timestamp, mins_bin=5)) %>%
  mutate(date=lubridate::date(timegroup)) %>%
  filter(date==date(now()-days(1))) %>%
  select(timegroup, ip=client_address, bytes, url) %>%
  group_by(timegroup, ip) %>%
  summarise(volume=round(sum(bytes)/1024/1024, 1)) %>% # Перевели в Мб
  top_n(10, volume) %>%
  filter()


gp <- ggplot(df, aes(timegroup, volume)) + 
  geom_line(aes(color=ip), alpha=0.8, lwd=1) +
  geom_point(aes(color=ip), alpha=0.8, shape=1, size=2) +
  # scale_y_continuous(trans='log10') +
  scale_color_brewer(palette="Set1") +
  # facet_wrap(~ip, scales="free_y", ncol=4) +
  facet_wrap(~ip, ncol=4) +
  # guides(colour=g, fill=g, shape=g) +
  #scale_y_log10(breaks=trans_breaks("log10", function(x) 10^x),
  #              labels=trans_format("log10", math_format(10^.x))) +
  #annotation_logticks() +
  theme_ipsum_rc(base_size=16, axis_title_size=14) +
  # theme_ipsum_rc(base_family="robotoC", base_size=16, axis_title_size=14) +
  xlab("Дата, время") +
  ylab("Суммарный объем данных, Mb") +
  ggtitle("Динамика трафика")

gp

# пытаемся изменить группировку
# в ggplot для ручного управления порядком следования необходимо управлять параметром group
# http://docs.ggplot2.org/current/aes_group_order.html
# -------------- нарисуем Top10 по IP за последние N минут ----
df <- df0 %>%
  # mutate(timegroup=hgroup.enum(timestamp, mins_bin=60)) %>%
  # mutate(date=lubridate::date(timegroup)) %>%
  # filter(date==date(now()-days(1))) %>%
  filter(timestamp > now()-days(2)) %>%
  # filter(between(timestamp, anytime("2017-09-01"), anytime("2017-10-01")))
  select(ip=client_address, bytes, url) %>%
  group_by(ip) %>%
  summarise(volume=round(sum(bytes)/1024/1024, 1)) %>% # Перевели в Мб
  top_n(10, volume) %>%
  # может возникнуть ситуация, когда все значения top_n одинаковы. тогда надо брать выборку
  filter(row_number()<=10) %>%
  filter(volume>1) %>%
  arrange(desc(volume)) %>%
  mutate(label=format(volume, big.mark=" "))


plot_df <- df %>%
  mutate(ip=fct_reorder(ip, volume))

gp <- ggplot(plot_df, aes(ip, volume)) + 
  geom_bar(fill=brewer.pal(n=9, name="Blues")[4], 
           alpha=0.5, stat="identity") +
  geom_label(aes(label=label), fill="white", colour="black", fontface="bold", hjust=+1.1) +
  theme_ipsum_rc(base_size=16, axis_title_size=14) +
  xlab("IP") +
  ylab("Суммарный Downlink, Мб") +
  ggtitle("ТОП 10 скачивающих", subtitle="По суммарному времени телесмотрения, мин") +
  coord_flip()

gp


# plotTop10Download(df)

stop()

txt <- c("http://yandex.ocsp-responder.com/", "www.yandex.ocsp-responder.com/sa") 

txt %>%
  stri_replace_all_regex(pattern=c("^([a-z]*)://", "^www\\.", "([^/]+).+"),
                         replacement=c("", "", "$1"),
                         vectorize_all=FALSE)


txt %>%
  stri_replace_all_regex("^([a-z]*)://", "") %>%
  stri_replace_all_regex("^www\\.", "") %>%
  stri_replace_all_regex("([^/]+).+", "$1")

# =====================
data <- read_log(fname)

data <- read_squid(fname)

data <- read_squid(system.file("extdata/log.squid", package="webreadr"))
