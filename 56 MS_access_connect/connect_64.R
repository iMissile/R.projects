library("RODBC") #load package

# эксперимент №1: https://gist.github.com/dainiuxt/23ce714e0deedc8da9a0

Sys.setenv(LANG = "en")
db <- file.path("./data/Access2007PureSQL.accdb") #connect database.
# normalizePath(db, winslash="/")
# db <- file.path("D:/iwork.GH/R.projects/56 MS_access_connect/data")
# db <- file.path("Access2007PureSQL.accdb") #connect database.

#Note the UNIX style slash (/). "\" is "escape character" so all "\"you should replace either with "/" or "\\"
# channel <- odbcConnectAccess2007(db) #internal RODBC function
# outputFile = paste(normalizePath(dirname(inputFile)),"\\", "my_file.ext", sep = "")
# Error in odbcConnectAccess(db): odbcConnectAccess is only usable with 32-bit Windows
channel <- odbcConnectAccess2007(db) #internal RODBC function

table_list <- sqlTables(channel) # запросим список таблиц
dataSetName <- sqlFetch(channel, "Products") #read particular table from Access database file.
close(channel) #do not forget this, otherwise you lock access database from editing.

stop()

library(iptools)
range_boundaries("10.0.1.0/8")
intToBits(167772160)
m <- range_generate("10.0.0.0/8")
object.size(m)