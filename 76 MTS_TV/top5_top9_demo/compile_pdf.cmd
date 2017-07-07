set PATH=c:\Program Files\RStudio\bin\pandoc;%PATH%

PATH

"C:\Program Files\R\R-3.4.0\bin\R" -e "rmarkdown::render('report1_pdf.Rmd', encoding = 'UTF-8')"