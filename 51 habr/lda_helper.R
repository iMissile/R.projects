library(xml2)
library(readr)
library(tibble)
library(dplyr)
library(purrr)
library(stringi)
library(stringr)

get_contents <- function(doc, tag_name, lang = "en")
{
  paste(
    xml_text(
      xml_find_all(doc, paste0("//", tag_name, "[@lang='", lang, "']//text()", sep = ""))
    ), 
    collapse = " ")
}

patent_path <- "./patents/"

patent_files <- dir(patent_path)
patent_files <- c("Patent Document 0000474124.zip")
n_files <- length(patent_files)
# n_files <- 20 # пока ограничил 20 файлами

v_num <- numeric(n_files)
v_filename <- character(n_files)

v_description_en <- character(n_files)
v_claims_en <- character(n_files)
v_abstract_en <- character(n_files)

v_description_ru <- character(n_files)
v_claims_ru <- character(n_files)
v_abstract_ru <- character(n_files)

for(i in seq(n_files))
{
  print(patent_files[i])
  
  zip_name <- paste0(patent_path, patent_files[i])
  z <- readr::read_file(unz(zip_name, filename = "PatentDocument.xml"))
  lat1.ent <- readr::read_file("xhtml-lat1.ent")
  z2 <- stringi::stri_replace_first_fixed(z, '<?xml version="1.0" encoding="UTF-8"?>',
                                          str_c('<?xml version="1.0" encoding="UTF-8"?>', 
                                         '<!DOCTYPE feed [',
                                         lat1.ent,
                                         ']>'))
  doc1 <- read_xml(z2, options = c("RECOVER"))
    cat("===")
  doc <- xml_ns_strip(doc1) 
  
  v_num[i] <- i
  v_filename[i] <- patent_files[i]
  
  v_description_en[i] <- get_contents(doc, "description", "en")
  v_claims_en[i] <- get_contents(doc, "claims", "en")
  v_abstract_en[i] <- get_contents(doc, "abstract", "en")
  
  v_description_ru[i] <- get_contents(doc, "description", "ru")
  v_claims_ru[i] <- get_contents(doc, "claims", "ru")
  v_abstract_ru[i] <- get_contents(doc, "abstract", "ru")
}

df_patents <- data.frame(v_num, v_filename, 
                         v_description_en, v_claims_en, v_abstract_en, 
                         v_description_ru, v_claims_ru, v_abstract_ru, 
                         stringsAsFactors=FALSE)

write.table(df_patents, "df_patents.csv", fileEncoding = "UTF-8")

# titleEnhanced
# abstractEnhanced
