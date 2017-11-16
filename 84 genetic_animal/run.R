library(GeneticsPed)
library(pryr)

gcinfo(TRUE)
memory.profile()

rel<-read.csv(file="rel.csv",header = T, sep=";")

rel$SireID[rel$SireID==0] <- NA

rel$DamID[rel$DamID==0] <- NA

rel$Sex <- factor(rel$Sex, labels = c("Male", "Female"))

relat <- subset(rel, rel$Breed==1, select = c("AnimalID","Sex","SireID", "DamID"))

x <- Pedigree(x=relat, subject="AnimalID", ascendant=c("SireID", "DamID"), 
              ascendantSex=c("Male", "Female"), sex="Sex")

x<-extend(x)
Hmisc::describe(x)

Ar<-relationshipAdditive(x, sort=F, names=T)

Ainv <- inverseAdditive(x=x) 
