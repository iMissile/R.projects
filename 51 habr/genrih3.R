library(readr)
library(archivist)

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
#       15.90         0.43        16.83


system.time(t <- readr::format_csv(x = z_obj))
#        user       system        total 
#        5.42         0.27         6.03


system.time(readr::write_csv(x = z_obj, path = "./export/write_csv.csv"))
#        user       system        total 
#        6.03         1.89         8.30


# ============ а теперь попробуем просто сбросить структуру в различные виды структур
## --- archivist
# create an empty repo
createLocalRepo ("./export/allModels", default = TRUE)

## --- RDS
system.time(saveRDS(z_obj, "./export/base.rds"))
#        user       system        total 
#        6.77         0.03         6.91

#system.time(saveRDS(z_obj, "base.rds", ascii = TRUE, compress = FALSE))
#        user       system        total 
#      158.52         2.42       171.86

## --- Feather: fast, interoperable data frame storage
# https://blog.rstudio.org/2016/03/29/feather/

