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


df <- readRDS("./Shiny_DPI_reports/edr_http.rds")

# facet графики для top 10 up/down ---------------------------------

df1 <- df %>%
  select(timestamp=end_timestamp, downlink_bytes, uplink_bytes, site, msisdn) %>%
  gather(downlink_bytes, uplink_bytes, key="direction", value="bytes") %>%
  sample_frac(0.2) %>%
  filter(bytes>0.1)


group_df <- df %>%
  mutate(msisdn=as.factor(msisdn)) %>%
  group_by(msisdn) %>%
  summarise(user_recs=n(), 
            uplink_Kb=round(sum(uplink_bytes)/1024, 1), 
            downlink_Kb=round(sum(downlink_bytes)/1024, 1)) %>%
  arrange(desc(user_recs))

plot_df <- group_df %>%
  gather(downlink_Kb, uplink_Kb, key="direction", value="volume") %>%
  group_by(direction) %>%
  top_n(10, volume) # объем считается в килобайтах
  #mutate(downlink_Mb=downlink_Kb/1024) %>%
  #arrange(desc(downlink_Kb))

gp <- ggplot(plot_df, aes(fct_reorder(msisdn, volume), volume)) + 
  geom_bar(fill=brewer.pal(n=9, name="Blues")[4], alpha=0.5, stat="identity") +
  facet_grid(.~direction, scales="free") +
  theme_ipsum_rc(base_size=16, axis_title_size=14) +
  xlab("'Как бы' MSISN") +
  ylab("Суммарный Downlink, Mb") +
  coord_flip()

gp

# выборка top 10 для площадок, up/down ---------------------------------

# regions <- c("Владивосток", "Новосибирск", "Екатеринбург", "Н.Новгород", "Краснодар", "Москва", "Санкт-Петербург")
regions <- c("Краснодар", "Москва")

df2 <- df %>%
  select(timestamp=end_timestamp, down=downlink_bytes, up=uplink_bytes, site, msisdn) %>%
  gather(up, down, key="direction", value="bytes") %>%
  group_by(site, direction, msisdn) %>%
  summarise(user_recs=n(), bytes=sum(bytes)) %>%
  top_n(10, bytes) %>%
  ungroup() # %>% spread(direction, bytes)

# spread дает на 4 меньше, поскольку в нескольких местах один и тот же msisdn присутствует и в up и в down
# df2 %>% filter(!is.na(downlink_bytes) & !is.na(uplink_bytes))