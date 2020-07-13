# Оптимальная переустановка R с пересбором пакетов
# https://ibecav.github.io/update_libraries/
require(tidyverse)

# Часть 1. Создание списка пакетов 
# Helpful background
allmypackages <- as.data.frame(installed.packages())

allmypackages <- allmypackages %>%
  filter(Priority != "base" | is.na(Priority)) %>%
  select(-c(Enhances:MD5sum, LinkingTo:Suggests)) %>%
  droplevels()

str(allmypackages)

# A function to do the hard work
package_source <- function(pkg){
  x <- as.character(packageDescription(pkg)$Repository)
  if (length(x)==0) {
    y <- as.character(packageDescription(pkg)$GithubRepo)
    z <- as.character(packageDescription(pkg)$GithubUsername)
    if (length(y)==0) {
      return("Other")
    } else {
      return(str_c("GitHub repo = ", z, "/", y))
    }
  } else {
    return(x)
  }
}

# show the first 60 as an example
head(sapply(allmypackages$Package, package_source), 60)

# What's in your libraries?
allmypackages$whereat <- sapply(allmypackages$Package, package_source)
str(allmypackages)

table(allmypackages$whereat)

allmypackages %>% 
  filter(whereat == "Other") %>%
  select(Package, Version)

write.csv(allmypackages, "mypackagelist.csv")

# ======================================
# Часть 2. Инсталляция пакетов по созданному списку
# Go ahead and install R 3.6.0
# post upgrade with output surpessed
install.packages("tidyverse")
library(tidyverse)

oldpackages <- read.csv("mypackagelist.csv")
allmypackages <- as.data.frame(installed.packages())
allmypackages <- allmypackages %>%
  filter(Priority != "base" | is.na(Priority)) %>%
  select(-c(Enhances:MD5sum, LinkingTo:Suggests))
thediff <- anti_join(oldpackages,allmypackages, by = "Package")

# Just do it!

thediff %>%
  filter(whereat == "CRAN") %>%
  pull(Package) %>%
  as.character %>%
  install.packages

# Что осталось за бортом?
thediff %>%
  filter(whereat != "CRAN") %>%
  select(Package, Version, NeedsCompilation, whereat)
