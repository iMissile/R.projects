rm(list=ls()) # очистим все переменные
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


# ==== functions definition
getAttr <- function(text, attrName) {
  res <- html_nodes(text, xpath=paste0("//div[@class='", attrName, "']")) %>% 
    magrittr::extract(-1) %>% # magrittr::extract(-1), удалили первую строку, там заголовок таблицы
    # magrittr provides a series of aliases which can be more pleasant to use when composing chains using the %>% operator
    html_text() # взяли сам атрибут
  # browser()
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
flog.info("============= Enreachment started ===============")


# загружаем ранее собранный список патентов
all_patents <- read_csv("patents_list_test.csv")

# магический url для считывания первой страницы документа из скачанного ранее списка с МПК = G05B взята из дебага в браузере и выглядит следующим образом:
# http://www1.fips.ru/wps/portal/!ut/p/c5/jY7LDoIwFES_hS-4l2dhWYhpC4hgYhA2pCENYngYVBZ-vbByJTqznJyZgRIWD3JuG_lox0F2cIbSqYSgKY-Yjij2LlIvC0Pqu8h2-pIXThUwyi0SI7LkGKCwfMvgzDdQmP_Q-EUUf9D5-nZ7fc03-hM-9goKKMhn55AQD2lsRzpPXJN5NuSTuo_PqVaQ1bK-qFjNqktlo-DWn854JS9KNe0NbQg1dw!!/?beanMethod=getDocument&queryId=2760601&documId=6b45832227654a2686e278babf036537&checkBoxes=&fromUserId=514
# где documId берем из загруженной выше таблицы (всего найдено документов 3743)
# в ответ получаем json, у которого в поле result -> hitlist лежит html со списком блока документов
# http://www.jsoneditoronline.org/
# http://codebeautify.org/jsonviewer

req_str1 <- "http://www1.fips.ru/wps/portal/!ut/p/c5/jY7LDoIwFES_hS-4l2dhWYhpC4hgYhA2pCENYngYVBZ-vbByJTqznJyZgRIWD3JuG_lox0F2cIbSqYSgKY-Yjij2LlIvC0Pqu8h2-pIXThUwyi0SI7LkGKCwfMvgzDdQmP_Q-EUUf9D5-nZ7fc03-hM-9goKKMhn55AQD2lsRzpPXJN5NuSTuo_PqVaQ1bK-qFjNqktlo-DWn854JS9KNe0NbQg1dw!!/?beanMethod=getDocument&queryId=2760601&documId="
req_str2 <- "&checkBoxes=&fromUserId=514"

all_docs <-
  foreach(n=iter(all_patents$docID), .packages='futile.logger', .combine=rbind) %do% {
    ur1 <- str_c(req_str1, n, req_str2, collapse = "")
    # browser()
    # resp <- try(curl_fetch_memory(url))
    # переходим на httr: https://cran.r-project.org/web/packages/httr/vignettes/quickstart.html
    # обработку exception пока не проводим
    resp <- GET(ur1)
    
    # проводим обработку контента
    flog.info(paste0("Parsing documentId = ", n, " HTTP Status Code = ", resp$status_code))
    
    htext <- fromJSON(content(resp, "text"))
    # browser()
    
    ht <- htext$result$html
    # j3 <- stri_encode(ht, from = "UTF-8", to = "cp1251")
    # browser()
    m <- read_html(ht, encoding = "UTF-8")
    # m2 <- read_html(ht, encoding = "windows-1251")
    # guess_encoding(m)
    # browser()
    
    
    # выцепляем Патентообладателя
    # IPR <- html_nodes(m, xpath="//*[@id='bibl']/p[2]/b") %>% html_text()
    tmp <- html_nodes(m, xpath="//*[@id='bibl']") %>% html_text()
    IPR <- stri_match_first_regex(tmp, "\\(73\\) Патентообладатель\\(и\\):(.+?)\r\n")[[2]]
    # browser()
    flog.info(paste0("Патентообладатель = ", IPR))
    # browser()
    
    # browser()
    elem <- tibble(
      IPR=IPR
    )
    # Encoding(dvTitle)
    
    elem
  }


write_csv(all_docs, "out_enreach2.csv", append=FALSE)
flog.info("Output file enreached")

stop()
# сниппеты
stri_match_first_regex(IPR, "\\(73\\) Патентообладатель\\(и\\):(.+?)\r\n")


stop()

# === пробуем вытащить документ вручную по его id
# прямая адресация по ссылке: http://www1.fips.ru/wps/portal/!ut/p/c5/jY7LDoIwFES_hS-4l2dhWYhpC4hgYhA2pCENYngYVBZ-vbByJTqznJyZgRIWD3JuG_lox0F2cIbSqYSgKY-Yjij2LlIvC0Pqu8h2-pIXThUwyi0SI7LkGKCwfMvgzDdQmP_Q-EUUf9D5-nZ7fc03-hM-9goKKMhn55AQD2lsRzpPXJN5NuSTuo_PqVaQ1bK-qFjNqktlo-DWn854JS9KNe0NbQg1dw!!/?beanMethod=getDocument&queryId=2760601&documId=dae8d132a3b82a6abef5bab3de50c234&checkBoxes=&fromUserId=514
write(ht, "resp.txt", append=FALSE)


m <- iconv(dvTitle, from="UTF8", to="windows-1251")
j3 <- stri_encode(j, from="UTF-8", to="cp1251")

# достаем идентификаторы документов
id <- html_nodes(m, xpath="//a[@class='hitListRow']") %>% html_attr("id")
# сама заявка доступна потом по такому url:
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

