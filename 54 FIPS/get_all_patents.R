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
library(RSelenium)
library(microbenchmark)
library(futile.logger)


common_log_name <- "FIPS.log"
output_fname <- "patents_list.csv"

# ==== functions definition
getAttr <- function(text, attrName) {
  res <- html_nodes(text, xpath=paste0("//div[@class='", attrName, "']")) %>% 
    magrittr::extract(-1) %>% # magrittr::extract(-1), удалили первую строку, там заголовок таблицы
    # magrittr provides a series of aliases which can be more pleasant to use when composing chains using the %>% operator
    html_text() # вз€ли сам атрибут
  # browser()
  # j <- res[[1]]
  # stri_encode(j, from="UTF-8", to="windows-1251", to_raw = FALSE)
  # browser()
  # iconv(from = "UTF8", to = "windows-1251")
  # m %>% html_nodes('div')
  res
}

# ======
flog.appender(appender.file(common_log_name))
flog.threshold(TRACE)
flog.info("============= Parsing started ===============")

# точка захода (от Ѕрыкова): http://www1.fips.ru/wps/portal/IPS_Ru#1478591840047

# магический url дл€ считывани€ части (50 шт) документов по ћѕ  = G05B вз€та из дебага в браузере и выгл€дит следующим образом:
# http://www1.fips.ru/wps/portal/!ut/p/c5/jY7LDoIwFES_hS-4l2dhWYhpC4hgYhA2pCENYngYVBZ-vbByJTqznJyZgRIWD3JuG_lox0F2cIbSqYSgKY-Yjij2LlIvC0Pqu8h2-pIXThUwyi0SI7LkGKCwfMvgzDdQmP_Q-EUUf9D5-nZ7fc03-hM-9goKKMhn55AQD2lsRzpPXJN5NuSTuo_PqVaQ1bK-qFjNqktlo-DWn854JS9KNe0NbQg1dw!!/?beanMethod=doRestoreQuery&queryId=2737608&doSearch=true&pageNumber=2&selectedDBs=RUPATABRU%3BRUPATAP%3BRUPAT_NEW%3BRUPMAB%3BRUPM_NEW%3BIMPIN&fromUserId=514
# где pagenumber мен€етс€ от 0 до 74 (всего документов 3743)
# в ответ получаем json, у которого в поле result -> hitlist лежит html со списком блока документов
# http://www.jsoneditoronline.org/
# http://codebeautify.org/jsonviewer

# категори€ ћѕ  = G05B (3738 документов на 11.11.2016)
req_str1 <- "http://www1.fips.ru/wps/portal/!ut/p/c5/jY7LDoIwFES_hS-4l2dhWYhpC4hgYhA2pCENYngYVBZ-vbByJTqznJyZgRIWD3JuG_lox0F2cIbSqYSgKY-Yjij2LlIvC0Pqu8h2-pIXThUwyi0SI7LkGKCwfMvgzDdQmP_Q-EUUf9D5-nZ7fc03-hM-9goKKMhn55AQD2lsRzpPXJN5NuSTuo_PqVaQ1bK-qFjNqktlo-DWn854JS9KNe0NbQg1dw!!/?beanMethod=doRestoreQuery&queryId=2737608&doSearch=true&pageNumber="
req_str2 <- "&selectedDBs=RUPATABRU%3BRUPATAP%3BRUPAT_NEW%3BRUPMAB%3BRUPM_NEW%3BIMPIN&fromUserId=514"

# категори€ ћѕ  = G06Q (2538 документов на 11.11.2016)
req_str1 <- "http://www1.fips.ru/wps/portal/!ut/p/c5/jY7LDoIwFES_hS-4l2dhWYhpC4hgYhA2pCENYngYVBZ-vbByJTqznJyZgRIWD3JuG_lox0F2cIbSqYSgKY-Yjij2LlIvC0Pqu8h2-pIXThUwyi0SI7LkGKCwfMvgzDdQmP_Q-EUUf9D5-nZ7fc03-hM-9goKKMhn55AQD2lsRzpPXJN5NuSTuo_PqVaQ1bK-qFjNqktlo-DWn854JS9KNe0NbQg1dw!!/?beanMethod=doRestoreQuery&queryId=2772556&doSearch=true&pageNumber="
req_str2 <- "&selectedDBs=RUPATABRU%3BRUPATAP%3BRUPAT_NEW%3BRUPMAB%3BRUPM_NEW%3BIMPIN&fromUserId=514"


# пробегаемс€ по страницам, начина€ с 0 и до n-1
all_patents <-
  foreach(n = iter(0:50), .packages = 'futile.logger', .combine = rbind) %do% {
    ur1 <- str_c(req_str1, n, req_str2, collapse = "")
    # browser()
    # resp <- try(curl_fetch_memory(url))
    # переходим на httr: https://cran.r-project.org/web/packages/httr/vignettes/quickstart.html
    # обработку exception пока не проводим
    resp <- GET(ur1)
    
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
    
    
    # теперь собираем все по част€м в data.frame
    dvIndex <- getAttr(m, "dvIndex") %>% stri_replace_all_fixed(".", "") # убрали точку после номера
    dvNumDoc <- getAttr(m, "dvNumDoc")
    dvDatePubl <- getAttr(m, "dvDatePubl")
    dvTitle <- getAttr(m, "dvTitle")
    
    # browser()
    # выцепл€ем идентификатор документа
    docID <- html_nodes(m, xpath="//a[@class='hitListRow']") %>% html_attr("id")
    
    
    # browser()
    elem <- tibble(
      dvIndex=as.numeric(dvIndex),
      dvNumDoc=dvNumDoc,
      dvDatePubl=dvDatePubl,
      docID=docID, 
      dvTitle=dvTitle
    )
    # Encoding(dvTitle)
    
    elem
  }


write_csv(all_patents, output_fname, append=FALSE)
flog.info("Output file generated")

stop()

# === пробуем вытащить документ вручную по его id
# пр€ма€ адресаци€ по ссылке: http://www1.fips.ru/wps/portal/!ut/p/c5/jY7LDoIwFES_hS-4l2dhWYhpC4hgYhA2pCENYngYVBZ-vbByJTqznJyZgRIWD3JuG_lox0F2cIbSqYSgKY-Yjij2LlIvC0Pqu8h2-pIXThUwyi0SI7LkGKCwfMvgzDdQmP_Q-EUUf9D5-nZ7fc03-hM-9goKKMhn55AQD2lsRzpPXJN5NuSTuo_PqVaQ1bK-qFjNqktlo-DWn854JS9KNe0NbQg1dw!!/?beanMethod=getDocument&queryId=2760601&documId=dae8d132a3b82a6abef5bab3de50c234&checkBoxes=&fromUserId=514


m <- iconv(dvTitle, from="UTF8", to="windows-1251")
j3 <- stri_encode(j, from="UTF-8", to="cp1251")

# достаем идентификаторы документов
id <- html_nodes(m, xpath="//a[@class='hitListRow']") %>% html_attr("id")
# сама за€вка доступна потом по такому url:
# http://www1.fips.ru/wps/portal/IPS_Ru#docNumber=13&docId=dae8d132a3b82a6abef5bab3de50c234
ur <- paste0("http://www1.fips.ru/wps/portal/IPS_Ru#docNumber=13&docId=", id[[1]])
resp <- GET(ur)
cc <- content(resp, "text")
str(content(resp, "text"), nchar.max=5000)
write(cc, "resp.txt", append=FALSE)

# лезем через Selenium
remDrv <- remoteDriver()
remDrv$open()
remDr <- remoteDriver(remoteServerAddr = "localhost", port = 5555, browserName = "internet explorer")
remDr <- remoteDriver(remoteServerAddr = "localhost", port = 9515, browserName = "chrome")
remDr <- remoteDriver(browserName = "internet explorer")
remDr <- remoteDriver(browserName = "chrome")
remDr$open()

# === разбираем документ через Selenium
# Selenium необходимо запускать ручками
# remDrv <- remoteDriver()
remDrv <- remoteDriver(browserName = "chrome")
# remDrv <- remoteDriver(browserName = "internet explorer")
remDrv$open()

ur2 <- "http://www1.fips.ru/wps/portal/IPS_Ru#docNumber=13&docId=dae8d132a3b82a6abef5bab3de50c234"

remDrv$navigate(ur1)
Sys.sleep(2)
remDrv$navigate(ur2)

