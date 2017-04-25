set PATH=c:\Program Files\RStudio\bin\pandoc;%PATH%

PATH

rem R -e "rmarkdown::render('xDR_analysis.Rmd', output_format='all', encoding = 'UTF-8')"
R -e "rmarkdown::render('xDR_analysis.Rmd', output_format='pdf_document', encoding = 'UTF-8')"