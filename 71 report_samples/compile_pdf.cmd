set PATH=c:\Program Files\RStudio\bin\pandoc;%PATH%

PATH

R -e "rmarkdown::render('ru-tex-pdf-examples.Rmd', encoding = 'UTF-8')"