library(rJava)
library(ReporteRs)

# Creation of mydoc, a mydocx object
mydoc <- docx( )

# add into mydoc first 10 lines of iris
mydoc <- addFlexTable( mydoc, FlexTable(iris[1:10,] ) )

# add a page break
mydoc <- addPageBreak( mydoc )

# add text with stylename "Normal" into mydoc 
mydoc <- addParagraph( mydoc, value = "Hello World!", stylename = "Normal" )

# add a plot into mydoc 
mydoc <- addPlot( mydoc, function() barplot( 1:8, col = 1:8 ) )

# write the doc 
writeDoc( mydoc, file = "word_simple_example.docx")