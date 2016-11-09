library(ggplot2) #load first! (Wickham)
library(lubridate) #load second!
library(dplyr)
library(tidyr)
library(tibble)
library(readr)
library(stringi)
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


# ==== functions definition
getAttr <- function(text, attrName) {
  res <- html_nodes(text, xpath=paste0("//div[@class='", attrName, "']")) %>% 
    extract(-1) %>% # magrittr::extract(-1), удалили первую строку, там заголовок таблицы
    # magrittr provides a series of aliases which can be more pleasant to use when composing chains using the %>% operator
    html_text() # взяли сам атрибут
  
  # j <- res[[1]]
  # stri_encode(j, from="UTF-8", to="windows-1251", to_raw = FALSE)
  # browser()
  # iconv(from = "UTF8", to = "windows-1251")
  # m %>% html_nodes('div')
  res
}

# ======
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
  foreach(n = iter(0:74), .packages = 'futile.logger', .combine = rbind) %do% {
    url <- str_c(req_str1, n, req_str2, collapse = "")
    resp <- try(curl_fetch_memory(url))
    # переходим на httr: https://cran.r-project.org/web/packages/httr/vignettes/quickstart.html
    # обработку exception пока не проводим
    resp <- GET(url)
    
    # проводим обработку контента
    flog.info(paste0("Parsing page ", n, " HTTP Status Code = ", resp$status_code))
    
    htext <- fromJSON(content(resp, "text"))
    # browser()
    
    ht <- htext$result$SearchResult$hitList
    # j3 <- stri_encode(ht, from = "UTF-8", to = "cp1251")
    # browser()
    m <- read_html(ht, encoding = "UTF-8")
    # m2 <- read_html(ht, encoding = "windows-1251")
    # guess_encoding(m)
    # browser()
    
    
    # теперь собираем все по частям в data.frame
    dvIndex <- getAttr(m, "dvIndex") %>% stri_replace_all_fixed(".", "") # убрали точку после номера
    dvNumDoc <- getAttr(m, "dvNumDoc")
    dvDatePubl <- getAttr(m, "dvDatePubl")
    dvTitle <- getAttr(m, "dvTitle")
    
    # browser()
    elem <- tibble(
      dvIndex = as.numeric(dvIndex),
      dvNumDoc = dvNumDoc,
      dvDatePubl = dvDatePubl,
      dvTitle = dvTitle
    )
    # Encoding(dvTitle)
    
    elem
  }


write_csv(all_docs, "out.csv", append=FALSE)
flog.info("Output file generated")

stop()

m <- iconv(dvTitle, from="UTF8", to="windows-1251")
j3 <- stri_encode(j, from="UTF-8", to="cp1251")
