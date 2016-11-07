library(tibble)
library(readr)
library(dplyr)
library(tidyr)
library(archivist)
library(iterators)
library(foreach)
library(doParallel)
library(futile.logger)
library(future)
library(microbenchmark)
# library(snowfall)
# http://stackoverflow.com/questions/20930112/how-to-log-using-futile-logger-from-within-a-parallel-method-in-r/
# http://stackoverflow.com/questions/38828344/how-to-log-when-using-foreach-print-or-futile-logger

common_log_name <- "large_dataset.log"
flog.appender(appender.file(common_log_name))
flog.threshold(TRACE)
flog.info("============= Experiment started ===============")



# d <- read.csv2(file = file.choose()), header = T, stringsAsFactors = F)
d <- readr::read_delim(file = "./data2/alcall_details_bristol_w31_1.csv", delim = ";",
                       col_types = "icccidc",
                       locale = locale(encoding = "windows-1251", decimal_mark = "."))
# locale: https://blog.rstudio.org/2015/10/28/readr-0-2-0/
z_obj <- data.frame()
for (i in 1:20) {
  z_obj <- rbind(z_obj, d)
}

object.size(z_obj)
#rm(d);gc()

res_time <- system.time(write.csv2(x = z_obj, file = "./export/write.csv2.csv", quote = T, row.names = F))
flog.info("write.csv2")
flog.info(capture.output(res_time))
#        user       system        total 
#       12.55         0.25        12.86

res_time <- system.time(t <- readr::format_csv(x = z_obj))
flog.info("readr::format_csv")
flog.info(capture.output(res_time))
#        user       system        total 
#        4.25         0.36         4.98

# z_obj -- data.frame
res_time <- system.time(readr::write_csv(x = z_obj, path = "./export/write_csv_df.csv"))
flog.info("readr::write_csv")
flog.info(capture.output(res_time))
#        user       system        total 
#        4.87         1.73         6.67

# zz_obj <- as_tibble(z_obj)
# res_time <- system.time(readr::write_csv(x = zz_obj, path = "./export/write_csv_tibble.csv"))
#        user       system        total 
#        5.02         1.65         6.70


# ============ попробуем то же самое, но в параллель

nworkers <- detectCores() - 1
#registerDoParallel(detectCores() - 1)
registerDoParallel(nworkers)
getDoParWorkers()

# регистрируем отдельный логгер на исполнителя
# http://stackoverflow.com/questions/38828344/how-to-log-when-using-foreach-print-or-futile-logger
loginit <- function(logfile) flog.appender(appender.file(logfile))
foreach(input=rep(common_log_name, nworkers), 
        .packages='futile.logger') %dopar% loginit(input)

# готовим выборки для потоков
nested_z_obj <- as_tibble(z_obj) %>%
  mutate(thread = row_number() %% nworkers) %>%
  mutate(workerID = thread) %>%
  group_by(thread) %>%
  nest()

# str(nested_z_obj$data[[2]])

gc()
mycombinefunc <- function(...){flog.info("custom combine")}


flog.info("-- Смотрим параллельный подход")
microbenchmark(it = iter(nested_z_obj$data))

res_time <- system.time(
  out <- foreach(it = iter(nested_z_obj$data), .combine='mycombinefunc', 
                 .multicombine=TRUE, 
                 .packages=c('readr', 'futile.logger', 'dplyr')) %dopar% {
    # cat(capture.output(str(it)))
    res_time <- system.time(t <- readr::format_csv(x = it))
    flog.info("readr::format_csv in thread")
    flog.info(capture.output(res_time))
    t
  })
flog.info("readr::format_csv after foreach")
flog.info(capture.output(res_time))

# ===== вывод в различные файлы в различных потоках
# взято отсюда: http://stackoverflow.com/questions/12994063/r-parallel-computing-with-snowfall-writing-to-files-from-separate-workers

gc()
nworkers <- detectCores() - 1
#registerDoParallel(detectCores() - 1)
registerDoParallel(nworkers)
getDoParWorkers()

# регистрируем отдельный логгер на исполнителя
# http://stackoverflow.com/questions/38828344/how-to-log-when-using-foreach-print-or-futile-logger
loginit <- function(logfile) {
  flog.appender(appender.file(logfile))
}

foreach(input=rep(common_log_name, nworkers), 
        .packages='futile.logger') %dopar% loginit(input)


flog.info("-- parallel writing")

res_time <- system.time(
  out <- foreach(it=iter(nested_z_obj$data), 
                 .packages=c('readr', 'futile.logger', 'dplyr')) %dopar% {
                   # cat(capture.output(str(it)))
                   oname <- sprintf('./export/worker_csv_%d.csv', unique(it$workerID))
                   flog.info(paste0("out file ", oname, " init"))
                   res_time <- system.time(readr::write_csv(x=it, path=oname))
                   flog.info("readr::write_csv in thread")
                   flog.info(capture.output(res_time))
                   NULL
                 })
flog.info("readr::write_csv after foreach")
flog.info(capture.output(res_time))
registerDoSEQ() # http://stackoverflow.com/questions/25097729/un-register-a-doparallel-cluster
stop()

# ===== а теперь вручную с помощью пакета future, без последующего объединения
multiThreadFormat <- function() {
  f1 <- future({ singleThreadFormat() }) %plan% multiprocess
  f2 <- future({ single_thread_mean() }) %plan% multiprocess
  f3 <- future({ single_thread_mean() }) %plan% multiprocess

  
  mean(value(f1), value(f2), value(f3), value(f4))
}

stop()
b <- stringr::str_c(out, collapse="")

# ============ а теперь попробуем просто сбросить структуру в различные виды структур
## --- archivist
# create an empty repo
repo <- "./export/allModels"
createLocalRepo(repo)
system.time(saveToRepo(z_obj, repo))
#        user       system        total 
#        7.29         0.30         7.78

# поглядим
showLocalRepo(repo, "tags")
# shinySearchInLocalRepo(repo)


## --- RDS
system.time(saveRDS(z_obj, "./export/base.rds"))
#        user       system        total 
#        6.11         0.03         6.17 

#system.time(saveRDS(z_obj, "base.rds", ascii = TRUE, compress = FALSE))
#        user       system        total 
#      158.52         2.42       171.86

## --- Feather: fast, interoperable data frame storage
# https://blog.rstudio.org/2016/03/29/feather/

