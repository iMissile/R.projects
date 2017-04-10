getOksData <- function(filename, sheetname = "") {

  # хак по считыванию типов колонок
  raw <- read_excel(filename)
  col_types <- rep("text", ncol(raw))
  # col_types <- readxl:::xlsx_col_types(filename)
  # col_types <- rep("text", length(col_types))
  
  #col_types <- rep("text", length(col_types))
  raw <- read_excel(filename,
                    col_types=col_types,
                    col_names=FALSE, 
                    skip = 1) # в первой строке просто написано "Отчетная форма"
  
  # имеем проблему, колонки с NA вместо имени
  # можно писать в одно преобразование, но специально разбил на шаги
  # трансформируем колонки
  df0 <- raw %>%
    repair_names(prefix = "repaired_", sep = "")
  
  # теперь вручную сформируем названия колонок
  # названия колонок размазаны по строкам 1-3. 
  # 1-ая -- группирующая, 2-я -- детализирующая, 3-я -- единица измерения (в начале таблицы там имена колонок)
  # Надо их слить и переименовать колонки, причем приоритет имеет строка 3, как уточняющая
  names_df <- tibble(name_c1=tidyr::gather(df0[1, ], key=name, value=v)$v,
                     name_c2=tidyr::gather(df0[2, ], key=name, value=v)$v,
                     name_c3=tidyr::gather(df0[3, ], key=name, value=v)$v) %>%
    # http://www.markhneedham.com/blog/2015/06/28/r-dplyr-update-rows-with-earlierprevious-rows-values/
    mutate(name_c1_ext=na.locf(name_c1, na.rm=FALSE)) %>% # в начале таблицы заголовки полей идут с 3-го уровня
    mutate(name_c2_ext=na.locf(name_c2, na.rm=FALSE))
  
  cnames <- names_df %>% 
    mutate(name.fix=str_c(str_replace_na(name_c1_ext, ""), # заменяем NA на "", см хелп на str_c
                          str_replace_na(name_c2_ext, ""), 
                          str_replace_na(name_c3, ""), sep = ":: ")) %>%
    mutate(name.fix=str_replace_all(name.fix, "\\r|\\n", " ")) %>% # перевод строки
    mutate(name.fix=str_replace_all(name.fix, "\\s+", " ")) %>% # дубли пробелов
    mutate(name.fix=str_replace_all(name.fix, "^(:: )+|(:: )+$", "")) # NA строк в начале или конце
  
  # dput(cnames$name.fix)
  # write(cnames$name.fix, "name_fix.txt", append=FALSE)
  
  # проверим кривые имена, дубли
  cnames %>% 
    group_by(name.fix) %>% 
    filter(n()>1)
  
  # аналитику необходимо вести по колонкам с английскими именами
  df1 <- df0
  repl.df <- frame_data(
    ~pattern, ~replacement,
    "Определение проекта(ОИП)", "oip_full",
    "Дата ввода ОФ", "date",
    "Стоимость стройки по ПД в базовых ценах:: Общий результат:: * 1 RUB", "pd_cost",
    "Стоимость стройки по ПД в базовых ценах:: Подрядные работы, в т.ч. материалы:: * 1 RUB", "smp_cost",
    "Стоимость стройки по ПД в базовых ценах:: Оборудование:: * 1 RUB", "plant_cost"
  )
  
  names(df1) <- stri_replace_all_fixed(cnames$name.fix,
                                       pattern = repl.df$pattern,
                                       replacement = repl.df$replacement,
                                       vectorize_all = FALSE)
  df1 %<>% repair_names(prefix = "repaired_", sep = "")
  
  # выбираем только интересующие колонки
  df2 <- df1 %>% select(oip_full, date, pd_cost, smp_cost, plant_cost) %>%
    #filter(row_number() > 6) %>% # удаляем весь верхний шлак
    # filter(complete.cases(.)) %>% # удаляем строки, содержащие пустые данные
    # выделяем только проектные строчки, автоматом отсекаем все заголовки и пр.
    # http://stackoverflow.com/questions/22850026/filtering-row-which-contains-a-certain-string-using-dplyr
    filter(str_detect(oip_full, '\\d{3}-\\d{7}\\.\\d{4}')) %>% # 022-2000791.0250
    separate(oip_full, into=c("oip", "sub_oip"), sep="[.]", remove=FALSE) %>%
    mutate(year=year(dmy(date, quiet=TRUE))) %>% # кривые даты превратились в NA, их и заменим
    replace_na(list(pd_cost='0', smp_cost='0', plant_cost='0', year=2015)) %>%
    #http://stackoverflow.com/questions/27027347/mutate-each-summarise-each-in-dplyr-how-do-i-select-certain-columns-and-give
    mutate_each(funs(as.numeric), pd_cost, smp_cost, plant_cost) %>%
    distinct() # уберем идентичные строчки
  
  df2
}

getExcelColName <- function (n) {
  m <- n - 1
  if_else(m < 26, LETTERS[m + 1], paste0(LETTERS[as.integer(m / 26)], LETTERS[as.integer(m %%26 ) + 1]))
}

getExcelFileList <- function(path=".", pattern="") {
  basename_matches <- dir(path, pattern, full.names=TRUE)
  basename_matches
  # пропускаем локи открытого файла
  partial_matches <- stri_match_all_regex(basename_matches, pattern='^.*/(?!\\~)[^/]*?\\.xl.*$') # тут list
  full_matches <- na.omit(unlist(partial_matches))
  # для корректной работы надо потом прогнать path <- readxl:::check_file(path) !!!!
  
  # stri_encode(full_matches, to=stri_enc_get())
  # full_matches <- partial_matches[!sapply(partial_matches, function(x) all(is.na(x)))] # удаляем NA
  #str(full_matches)
  stri_conv(full_matches, from="UTF-8", to_raw=FALSE)
}

loadExcelReportList <- function(file_list) {
  # хелпер для отчетов 1 и 3
  # browser()
  ret <-
    foreach(fname = iter(file_list),
            .packages = 'futile.logger',
            .combine = rbind) %do% {
              flog.info(paste0("Parsing report file: ", fname))
              cat(fname)
              # browser()
              
              # адресацию ведем по именам колонок, как в Excel, поэтому ручные названия все дропаем
              # хак по считыванию типов колонок
              
              file_format <- readxl:::excel_format(fname)
              flog.info(paste0("File format: ", file_format))
              raw <- read_excel(fname, col_names=FALSE)
              flog.info(paste0("Columns = ", ncol(raw)))

              # col_types <- readxl:::xlsx_col_types(fname)
              col_types <- rep("text", ncol(raw))
              col_names <- str_c("grp_", seq_along(col_types))
              
              raw <- read_excel(fname,
                                col_types = col_types,
                                col_names = col_names,
                                skip = 0) # в первой строке просто написано "Отчетная форма"
              
              # выбираем только те строки в которых есть упоминание про ОИП
              ret <- raw %>%
                # filter(str_detect(oip_full, '\\d{3}-\\d{7}\\.\\d{4}')) %>% # 022-2000791.0250
                mutate(fname = fname)
            }
}

loadReportsType1 <- function(path){
  # отчет в разрезе ОИП (отчет №1); 022-2016-03кв; несколько файлов эксель
  file_list <- getExcelFileList(path, pattern="*№\\s*1")
  flog.info(paste0("Report #1 file list: ", file_list))
  
  raw <- loadExcelReportList(file_list)
  raw %>% filter(str_detect(grp_1, '\\d{3}-\\d{7}')) # 022-2000791
}

loadReportsType2 <- function(fname){
  # выгрузка из САП (отчет №2); отчет с детализацией по ОКС; один файл эксель
  flog.info(paste0("Parsing report #2 file: ", fname))
  
  # адресацию ведем по именам колонок, как в Excel, поэтому ручные названия все дропаем
  # хак по считыванию типов колонок
  raw <- read_excel(fname)
  
  # col_types <- readxl:::xlsx_col_types(fname)
  col_types <- rep("text", ncol(raw))
  col_names <- str_c("col_", getExcelColName(seq_along(col_types)))
  flog.info(paste0("Columns = ", ncol(raw)))
  
  raw <- read_excel(fname,
                    col_types=col_types,
                    col_names=col_names, 
                    skip = 0) # в первой строке просто написано "Отчетная форма"
  
  # выбираем только те строки в которых есть упоминание про ОИП
  ret <- raw %>%
    # filter(str_detect(oip_full, '\\d{3}-\\d{7}\\.\\d{4}')) %>% # 022-2000791.0250
    filter(str_detect(col_A, '\\d{3}-\\d{7}')) # 022-2000791
  
}

loadReportsType3 <- function(path){
  # отчет в разрезе ОКС (отчет №3); отчет о СС ИПР; несколько файлов эксель
  file_list <- getExcelFileList(path, pattern="*№\\s*3")
  flog.info(paste0("Report #3 file list: ", file_list))
  # browser()
  raw <- loadExcelReportList(file_list)
  raw %>% filter(str_detect(grp_1, '\\d{3}-\\d{7}')) # 022-2000791
}

