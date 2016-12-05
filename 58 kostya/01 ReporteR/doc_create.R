# Читаем исходные данные
data <- read_csv2("report.csv", skip = 1,
                  col_names = c("Проведенные работы", "Дата и время возникновения инцидента", "Дата и время решения инцидента", "Результаты", "Уровень SLA"), 
                  cols(
                    `Проведенные работы` = col_character(),
                    `Дата и время возникновения инцидента` = col_date(format = "%d.%m.%Y"),
                    `Дата и время решения инцидента` = col_date(format = "%d.%m.%Y"),
                    `Результаты` = col_character(),
                    `Уровень SLA` = col_character()
                  )
)

# проставляем номера строк
data$`Номер записи` <- seq.int(nrow(data))
data %<>% select(`Номер записи`, `Проведенные работы`, `Дата и время возникновения инцидента`, `Дата и время решения инцидента`, Результаты, `Уровень SLA`)


################################################################## Формируем шапку документа
# Create a word document to contain R outputs
doc <- docx()
# Add a title to the document
doc <- addParagraph(doc,
                    pot("Отчет по статистике  SLA  (ОВН)", format = textBold()),
                    par.properties = parProperties(text.align = 'center'))

# doc <- addTitle(doc, value = "Отчет по статистике  SLA  (ОВН)", level = 1)

# # Add a paragraph of text into the Word document
# # Формируем подчёркнутый стиль (там, где будет название)
# underlined_style <- textNormal(underlined = TRUE)

# Формируем сам текст
my_text <- 
  'Состав работ проведенных в период с 01 июля 2016г. по 31 июля 2016г.
в рамках ДС № 1/Д150227064-ДС1 от «24» декабря 2015 г,  ДС № 2/Д150227064-ДС2
от «22» июля 2016 г. к договору №  Д150227064 от «29» апреля 2015г.'

doc <- addParagraph(doc, my_text)
# doc <- addParagraph(doc, pot('Устранение ТТ', underlined_style))

# Содержание
# Add a title
doc <- addTitle(doc, "Содержание")
doc <- addTOC(doc)
doc <- addPageBreak(doc) # go to the next page


######################################### Первый блок данных
# делаем заголовок первого уровня
doc <- addTitle(doc, value = "Устранение ТТ")


######################################### Формируем таблицу
MyFTable <- vanilla.table(data)
# Задаём стиль
MyFTable <- setZebraStyle(MyFTable, odd = '#eeeeee', even = 'white')
# MyFTable <- FlexTable(data)

doc <- addFlexTable( doc , MyFTable )

########################################### Итого для таблицы ##########################
total_text <- paste0("Итого на конец периода: ", nrow(data), " - выполненные работы")

doc <- addParagraph(doc, pot(total_text, format = textBold()))
doc <- addPageBreak(doc) # Разрыв страницы


########################################## Второй блок данных
# делаем заголовок первого уровня
doc <- addTitle(doc, value = "Проведённые плановые работы")
