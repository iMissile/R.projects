set PATH=c:\Program Files\RStudio\bin\pandoc;%PATH%

PATH

R -e "rmarkdown::render('ru-tex-pdf-examples.Rmd', output_format=encoding = 'UTF-8')"