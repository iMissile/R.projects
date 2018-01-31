library(RCurl)
library(tidyverse)
library(magrittr)
library(lubridate)
#library(webreadr)
library(anytime)
library(stringi)
library(RColorBrewer)
library(extrafont)
library(hrbrthemes)
library(futile.logger)

eval(parse("funcs.R", encoding="UTF-8"))
windowsFonts(robotoC="Roboto Condensed")

# m <- scp("10.0.0.246", path="/var/log/squid/access.log", keypasswd="zDhA8136!", user="root")

# unicode test: я
fname <- "./data/access.log"
raw_df <- read_table2(fname, 
                      col_names=c("timestamp", "duration", "client_address", "result_codes", 
                                  "bytes", "request_method", "url", "user", "hierarcy_code", "type"),
                      col_types=("nicciccccc")
                      )
df0 <- raw_df %>%
  mutate(t=anytime(timestamp, tz="Europe/Moscow")) %>%
  mutate(url=stri_replace_all_regex(url, 
                                    pattern=c("^([a-z]*)://", "^www\\.", "([^/]+).+"),
                                    replacement=c("", "", "$1"),
                                    vectorize_all=FALSE)) %>%
  mutate_at(vars(client_address), as.factor)

# -------------- нарисуем трафик ----

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
  filter(volume<1) %>%
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
  ggtitle("ТОП 10 скачивающих") +
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
