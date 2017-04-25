set PATH=c:\Program Files\RStudio\bin\pandoc;%PATH%

PATH

R -e "rmarkdown::render('xDR_analysis.Rmd', output_format='html_document', encoding = 'UTF-8')"