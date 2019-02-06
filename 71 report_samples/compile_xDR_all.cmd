set PATH=c:\Program Files\RStudio\bin\pandoc;%PATH%

PATH

C:\Program Files\R\R-3.5.2\bin\R.exe -e "rmarkdown::render('xDR_analysis.Rmd', output_format='all', encoding = 'UTF-8')"