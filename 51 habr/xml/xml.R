library(XML)
library(xml2)

z <- unz("Patent Document 0001488729.zip", filename = "PatentDocument.xml", 'r') 
doc <- xmlParse("PatentDocument.xml")
# doc <- xmlParse(file = z)
doc2 <- read_xml("Patent Document 0000738660.zip")

all.equal(doc, doc2)

nodes <- getNodeSet(doc2, "//titleAscii[@tsip:lang='en']")


df <- xmlToDataFrame(
  getNodeSet(doc, "//inventor[@name='HUAN W']"),
  colClasses=c("character"),
  stringsAsFactors = F)



# ========== Индекс надежности	Номера элементов
p1 <- 0.99999768
p2 <- p1
p3 <- 0.99999
p4 <- p3
p5 <- 0.99986592
p6 <- p5
p7 <- 0.99999
(p1+p2-p1*p2)*(p3+p4-p3*p4)*(p5+p6-p5*p6)*p7
