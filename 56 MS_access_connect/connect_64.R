library("RODBC") #load package

# эксперимент №1: https://gist.github.com/dainiuxt/23ce714e0deedc8da9a0

db<-file.path("./data/Access2007PureSQL.accdb") #connect database.
#Note the UNIX style slash (/). "\" is "escape character" so all "\"you should replace either with "/" or "\\"
channel<-odbcConnectAccess2007(db) #internal RODBC function
dataSetName<-sqlFetch(channel,"TableName") #read particular table from Access database file.