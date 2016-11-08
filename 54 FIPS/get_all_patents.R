library(ggplot2) #load first! (Wickham)
library(lubridate) #load second!
library(dplyr)
library(tidyr)
library(readr)
library(stringr)
library(jsonlite)
library(magrittr)
library(curl)
library(httr)
library(jsonlite)
library(xml2)
library(rvest)
library(iterators)
library(foreach)
library(doParallel)
library(future)
library(microbenchmark)
library(futile.logger)

flog.appender(appender.file("FIPS.log"))
flog.threshold(TRACE)
flog.info("============= Parsing started ===============")


# магический url для считывания части (50 шт) документов по МПК = G05B взята из дебага в браузере и выглядит следующим образом:
# http://www1.fips.ru/wps/portal/!ut/p/c5/jY7LDoIwFES_hS-4l2dhWYhpC4hgYhA2pCENYngYVBZ-vbByJTqznJyZgRIWD3JuG_lox0F2cIbSqYSgKY-Yjij2LlIvC0Pqu8h2-pIXThUwyi0SI7LkGKCwfMvgzDdQmP_Q-EUUf9D5-nZ7fc03-hM-9goKKMhn55AQD2lsRzpPXJN5NuSTuo_PqVaQ1bK-qFjNqktlo-DWn854JS9KNe0NbQg1dw!!/?beanMethod=doRestoreQuery&queryId=2737608&doSearch=true&pageNumber=2&selectedDBs=RUPATABRU%3BRUPATAP%3BRUPAT_NEW%3BRUPMAB%3BRUPM_NEW%3BIMPIN&fromUserId=514
# где pagenumber меняется от 0 до 74 (всего документов 3743)
# в ответ получаем json, у которого в поле result -> hitlist лежит html со списком блока документов
# http://www.jsoneditoronline.org/
# http://codebeautify.org/jsonviewer

req_str1 <- "http://www1.fips.ru/wps/portal/!ut/p/c5/jY7LDoIwFES_hS-4l2dhWYhpC4hgYhA2pCENYngYVBZ-vbByJTqznJyZgRIWD3JuG_lox0F2cIbSqYSgKY-Yjij2LlIvC0Pqu8h2-pIXThUwyi0SI7LkGKCwfMvgzDdQmP_Q-EUUf9D5-nZ7fc03-hM-9goKKMhn55AQD2lsRzpPXJN5NuSTuo_PqVaQ1bK-qFjNqktlo-DWn854JS9KNe0NbQg1dw!!/?beanMethod=doRestoreQuery&queryId=2737608&doSearch=true&pageNumber="
req_str2 <- "&selectedDBs=RUPATABRU%3BRUPATAP%3BRUPAT_NEW%3BRUPMAB%3BRUPM_NEW%3BIMPIN&fromUserId=514"

all_docs <-
  foreach(n = iter(0:0), .packages = 'futile.logger', .combine = "c") %do% {
            url <- str_c(req_str1, n, req_str2, collapse = "")
            resp <- try(curl_fetch_memory(url))

            # проверим только 1-ый элемент класса, поскльку при разных ответах получается разное кол-во элементов
            if (class(resp)[[1]] == "try-error" || resp$status_code != 200) {
              # http://stackoverflow.com/questions/15595478/how-to-get-the-name-of-the-calling-function-inside-the-called-routine
              flog.error(paste0("Error in ", calledFun, " called from ", callingFun, ". Class(resp) = ", class(resp)))
              flog.error(paste0("resp = ", resp))
              # сигнализируем о невозможности обновить данные
              elem <- NULL
            } else {
              # проводим обработку контента
              flog.info(paste0("Parsing page ", n))
              htext <- fromJSON(rawToChar(resp$content))
              
              elem <- htext$result$SearchResult$hitList
              m <- read_html(elem, options=c("RECOVER"))
              # теперь собираем все по частям в data.frame
              dvIndex <- html_nodes(m, xpath="//div[@class='dvIndex']") %>% 
                magrittr::extract(-1) %>% # удалили первую строку, там заголовок таблицы
                html_text()
              #  iconv(from = "UTF8", to = "windows-1251")
#              browser()
#              m %>% html_nodes('div')
            }
            
            elem
  }

write(all_docs, file="out.txt", append=FALSE)
flog.info("Output file generated")
