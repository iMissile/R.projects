library(tibble)
library(readr)
library(dplyr)
library(tidyr)
library(archivist)
library(iterators)
library(foreach)
library(doParallel)

# d <- read.csv2(file = file.choose()), header = T, stringsAsFactors = F)
d <- readr::read_delim(file = "./data2/alcall_details_bristol_w31_1.csv", delim = ";",
                       col_types = "icccidc",
                       locale = locale(encoding = "windows-1251", decimal_mark = "."))
# locale: https://blog.rstudio.org/2015/10/28/readr-0-2-0/
z_obj <- data.frame()
for (i in 1:10) {
  z_obj <- rbind(z_obj, d)
}

object.size(z_obj)
#rm(d);gc()

system.time(write.csv2(x = z_obj, file = "./export/write.csv2.csv", quote = T, row.names = F))
#        user       system        total 
#       12.55         0.25        12.86

system.time(t <- readr::format_csv(x = z_obj))
#        user       system        total 
#        4.25         0.36         4.98

# z_obj -- data.frame
system.time(readr::write_csv(x = z_obj, path = "./export/write_csv_df.csv"))
#        user       system        total 
#        4.87         1.73         6.67

zz_obj <- as_tibble(z_obj)
system.time(readr::write_csv(x = zz_obj, path = "./export/write_csv_tibble.csv"))
#        user       system        total 
#        5.02         1.65         6.70


# ============ попробуем то же самое, но в параллель
cat("Смотрим параллельный подход")
cores = detectCores()
#registerDoParallel(detectCores() - 1)
registerDoParallel(cores)
getDoParWorkers()

# готовим выборки для потоков
nested_z_obj <- as_tibble(z_obj) %>%
  mutate(thread = row_number() %% cores) %>%
  group_by(thread) %>%
  nest()


system.time(res <-
              foreach(it = iter(nested_z_obj$data), .combine = rbind, .packages = 'readr') %do% {
                cat(capture.output(str(it)))
                t <- readr::format_csv(x = it)
                })

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

