# автоматизируем процедуру установки пакетов
# дл€ этого используем два пакета: pacman & githubinstall
# pacman: https://github.com/trinker/pacman
# githubinstall: https://cran.r-project.org/web/packages/githubinstall/vignettes/githubinstall.html

# Warnings will be truncated to getOption("warning.length") characters, default 1000, indicated by [... truncated]. 


if (!require("pacman")) install.packages("pacman")
if (!require("githubinstall")) install.packages("githubinstall")
if (!require("ghit")) install.packages("ghit")


pacman::p_load("devtools")

## ==================== установка базовых расширений ===========================

pacman::p_load("tidyverse", "dplyr", "tidyr", "ggplot2", "lubridate", "scales", "readr", "reshape2", "curl", "httr", "jsonlite", "stringr", "magrittr", 
"ggmap", "ggthemes", "ggdendro", "ggrepel", "wesanderson", "RColorBrewer", "colorRamps", 
"gtable", "grid", "gridExtra", "shiny", "shinyBS", "shinythemes", "plotly", "sp", "leaflet", "rgl", "DT", "KernSmooth", "akima", 
"arules", "futile.logger", "iterators", "foreach", "doParallel", "broom", "forecast", "rredis", "revealjs", "Rmpfr", "readxl",  
"googlesheets", "iptools", "ggalt", "ggExtra", "bookdown", "replyr", "roxygen2", "uuid", "rlist", "tidyjson", "fasttime", "anytime")



## ==================== установка дополнительных находок ===========================

# tmap: Thematic Maps: https://cran.r-project.org/web/packages/tmap/index.html
# Thematic maps are geographical maps in which spatial data distributions are visualized. This package offers a flexible, layer-based, and easy to use approach to create thematic maps, such as choropleths and bubble maps.
pacman::p_load("tmap")

# osmplotr: Bespoke Images of 'OpenStreetMap' Data: https://cran.r-project.org/web/packages/osmplotr/
# Bespoke images of 'OpenStreetMap' ('OSM') data and data visualisation using 'OSM' objects.
pacman::p_load("osmplotr")

# OpenStreetMap: Access to Open Street Map Raster Images: https://cran.r-project.org/web/packages/OpenStreetMap/index.html
pacman::p_load("OpenStreetMap")

# For object_size function: https://github.com/hadley/pryr
# Pryr open the covers of R. pryr provides tools to pry back the surface of R and dig into the details. It has been developed in conjunction with "Advanced R programming" to make it easier to understand what's going on in R.
pacman::p_load("pryr")

# The R6 package provides a type of class which is similar to RТs standard reference classes, but it is more efficient and doesnТt depend on S4 classes and the methods package.
# https://cran.r-project.org/web/packages/R6/vignettes/Introduction.html
pacman::p_load("R6")

# reticulate: R Interface to Python: https://cran.r-project.org/web/packages/reticulate/index.html 
pacman::p_load("reticulate")

# Geoms to make joyplots using ggplot2, written by Claus O. Wilke : https://github.com/clauswilke/ggjoy
pacman::p_load("ggjoy")

# jQuery UI Interactions and Effects for Shiny https://yang-tang.github.io/shinyjqui/
pacman::p_load("shinyjqui")

# shinyWidgets : Extend widgets available in shiny
pacman::p_load("shinyWidgets")


# ndjson: Wicked-Fast Streaming 'JSON' ('ndjson') Reader: https://cran.r-project.org/web/packages/ndjson/index.html. 
# https://rud.is/b/2017/06/17/replicating-the-apache-drill-yelp-academic-dataset-with-sergeant/
pacman::p_load("ndjson")

# lumberjack: Track Changes in Data the Tidy Way
# http://www.markvanderloo.eu/yaRb/2017/06/23/track-changes-in-data-with-the-lumberjack/
pacman::p_load("lumberjack")

# shinycssloaders: Add CSS Loading Animations to 'shiny' Outputs
# https://cran.r-project.org/web/packages/shinycssloaders/index.html
pacman::p_load("shinycssloaders")
 
# hadley/purrrlyr. Tools at the intersection of purrr and dplyr
# https://github.com/hadley/purrrlyr
pacman::p_load("purrrlyr")

# Using miniCRAN to create a local CRAN repository
# https://cran.r-project.org/web/packages/miniCRAN/vignettes/miniCRAN-introduction.html
pacman::p_load("miniCRAN")


# Databases using R: https://rviews.rstudio.com/2017/05/17/databases-using-r/
# https://github.com/tidyverse/dbplyr
pacman::p_load("RODBC", "RODBCDBI", "dbplyr")
# devtools::install_github("tidyverse/dplyr")
# devtools::install_github("tidyverse/dbplyr")
pacman::p_load("rstats-db/odbc")
pacman::p_load("DBI", "RPostgreSQL")
# config: Manage Environment Specific Configuration Values
# https://db.rstudio.com/deployment/#deploying-with-config-package
pacman::p_load("config")


# fuzzyjoin: Join Tables Together on Inexact Matching
pacman::p_load("fuzzyjoin")

# Blogdown: https://github.com/rstudio/blogdown 
# An open-source (GPL-3) R package to generate static websites based on R Markdown and Hugo. 
devtools::install_github('rstudio/blogdown')

# countrycode: Convert Country Names and Country Codes
pacman::p_load("countrycode")

# tictoc: Functions for timing R scripts, as well as implementations of Stack and List structures
pacman::p_load("tictoc")

# microbenchmark: Accurate Timing Functions
pacman::p_load("microbenchmanrk")


# digest - Create Compact Hash Digests of R Objects
pacman::p_load("digest")

# An R package for creating slideshows with [remark.js](http://remarkjs.com) through R Markdown.
pacman::p_load_gh("yihui/xaringan")

# ggthemr - Themes for ggplot2: https://github.com/cttobin/ggthemr
pacman::p_load_gh("cttobin/ggthemr")

# ggnetwork: Network geometries for ggplot2: https://cran.r-project.org/web/packages/ggnetwork/vignettes/ggnetwork.html
# ggnet (in "GGally"): network visualization with ggplot2: https://github.com/briatte/ggnet
# geomnet is a package built on top of the most recent major ggplot2 release. https://github.com/sctyner/geomnet
# Intergraph: R package with coercion routines for network data objects: http://mbojan.github.io/intergraph/
pacman::p_load("ggnetwork", "GGally", "geomnet", "intergraph")

# Cairo: R graphics device using cairo graphics library for creating high-quality bitmap (PNG, JPEG, TIFF), vector (PDF, SVG, PostScript) and display (X11 and Win32) output
pacman::p_load("Cairo")

# elastic: General Purpose Interface to 'Elasticsearch'
# https://ropensci.org/tutorials/elastic_tutorial.html
pacman::p_load("elastic")

# elasticsearchr: A Lightweight Interface for Interacting with Elasticsearch from R
# elasticsearchr is a lightweight client - by this I mean that it only aims to do Сjust enoughТ work to make using Elasticsearch with R easy and intuitive. 
# https://cran.r-project.org/web/packages/elasticsearchr/vignettes/quick_start.html
pacman::p_load("elasticsearchr")
                         
# assertr is a R package to help you identify common dataset errors: http://www.onthelambda.com/2017/03/20/data-validation-with-the-assertr-package/
pacman::p_load("assertr")

# https://cran.r-project.org/web/packages/hrbrthemes/vignettes/why_hrbrthemes.html
pacman::p_load("hrbrthemes")

# Rstats client for ClickHouse (https://clickhouse.yandex)
devtools::install_github("hannesmuehleisen/clickhouse-r")

# Transforms datetime data into a format ready for analysis: https://edwinth.github.io/blog/padr-intro/
pacman::p_load("padr")

# debugme: Debug R Packages
# Specify debug messages as special string constants, and control debugging of packages via environment variables.
# https://github.com/gaborcsardi/debugme
pacman::p_load("debugme")

# debug: MVB's debugger for R: https://cran.r-project.org/web/packages/debug/index.html
pacman::p_load("debug")

# imager: an R package for image processing: http://dahtah.github.io/imager/imager.html
# Getting started with imager: http://dahtah.github.io/imager/gettingstarted.html
# https://github.com/dahtah/imager
pacman::p_load("imager")

# How to use your favorite fonts in R charts: http://blog.revolutionanalytics.com/2012/09/how-to-use-your-favorite-fonts-in-r-charts.html
pacman::p_load("extrafont")

# PythonInR: Use Python from Within R: https://cran.r-project.org/web/packages/PythonInR/index.html
pacman::p_load("PythonInR")
	
# A colour picker tool for Shiny and for selecting colours in plots (in R) http://deanattali.com/blog/plot-colour-helper/
pacman::p_load("colourpicker")
# or install the latest development version from GitHub:
# pacman::p_load_gh("daattali/colourpicker")

# latex2exp is an R package that parses and converts LaTeX math formulas to RТs plotmath expressions.
# ftp://cran.r-project.org/pub/R/web/packages/latex2exp/vignettes/using-latex2exp.html
pacman::p_load("latex2exp")
# or
# pacman::p_load_gh("stefano-meschiari/latex2exp")

# vtreat: A Statistically Sound data.frame Processor/Conditioner: http://www.win-vector.com/blog/2016/12/data-preparation-long-form-and-tldr-form/
pacman::p_load("vtreat")
                      
# ggplot2 tech themes, scales, and geoms: https://medium.com/airbnb-engineering/using-r-packages-and-education-to-scale-data-science-at-airbnb-906faa58e12d
devtools::install_github("ricardo-bion/ggtech", dependencies=TRUE)	

# plumber: An R package that converts your existing R code to a web API. http://plumber.trestletech.com/
pacman::p_load("plumber")

# rJava: Low-Level R to Java Interface
pacman::p_load("rJava")

# ReporteR
install.packages("ReporteRs", INSTALL_opts = "--no-multiarch")

# ropenscilabs/tabulizer: Bindings for Tabula PDF Table Extractor Library. https://github.com/ropenscilabs/tabulizer
# pacman::p_load_gh(c("ropensci/tabulizerjars", "ropensci/tabulizer"))
# поставилось только так
# on 64-bit Windows
ghit::install_github(c("ropensci/tabulizerjars", "ropensci/tabulizer"), INSTALL_opts = "--no-multiarch")
# elsewhere
# ghit::install_github(c("ropensci/tabulizerjars", "ropensci/tabulizer"))

# commonmark: High Performance CommonMark and Github Markdown Rendering in R https://cran.r-project.org/web/packages/commonmark/index.html
pacman::p_load("commonmark")

# Relational operators for intervals with the intrval R package. http://peter.solymos.org/code/2016/12/02/relational-operators-for-intervals-with-the-intrval-r-package.html
# https://github.com/psolymos/intrval
pacman::p_load_gh("psolymos/intrval")

# D3partitionR: Plotting D3 Hierarchical Plots in R and Shinyhttps://antoineguillot.wordpress.com/2016/12/02/d3partitionr-0-3-0-is-available-on-cran/
pacman::p_load("D3partitionR")

# XLConnect: Excel Connector for R. Provides comprehensive functionality to read, write and format Excel data.
pacman::p_load("XLConnect")

# htmlwidget to make "ggplot" graphics interactive http://davidgohel.github.io/ggiraph/introduction.html
pacman::p_load("ggplot2", "rvg")
pacman::p_load_gh("davidgohel/ggiraph")

# forecastxgb-r-package https://github.com/ellisp/forecastxgb-r-package/
# pacman::p_load_gh("ellisp/forecastxgb-r-package/pkg")
devtools::install_github("ellisp/forecastxgb-r-package/pkg")

# synchronicity: Boost Mutex Functionality in R
# UNIX only package!!!!!!
pacman::p_load("synchronicity")


# udunits2: Udunits-2 Bindings for R
# Provides simple bindings to Unidata's udunits library.
# требуетс€ дл€ пакета ggforce
pacman::p_load("udunits2")
# дл€ linux все гораздо сложнее. ставим либо udunits2, ищем  размещение файла udunits2.h и даем такую хитрую команду:
# install.packages("udunits2", configure.args='--with-udunits2-include=/usr/include/udunits2/')

# Accelerating ggplot2: http://www.data-imaginist.com/2016/Announcing-ggforce/
pacman::p_load_gh("thomasp85/ggforce")

# ggedit Ц interactive ggplot aesthetic and theme editor: https://www.r-statistics.com/2016/11/ggedit-interactive-ggplot-aesthetic-and-theme-editor/
# devtools::install_github("metrumresearchgroup/ggedit",subdir="ggedit")
pacman::p_load_gh("metrumresearchgroup/ggedit/ggedit")

# ezknitr - Avoid the typical working directory pain when using "knitr" 	http://deanattali.com/blog/ezknitr-package/
# pacman::p_load_gh("ezknitr")
# To install the latest developmental version from GitHub:
pacman::p_load_gh("daattali/ezknitr")


# mapmate: https://leonawicz.github.io/mapmate/usage_and_limitations.html
pacman::p_load_gh("leonawicz/mapmate")

# pbmcapply: tracking the progress of mc*apply in R: https://kevinkuang.net/tracking-progress-in-r-ad97998c359f#.2y2rp9kv0
# https://github.com/kvnkuang/pbmcapply
pacman::p_load("pbmcapply")


# revealjs: R Markdown format for "reveal.js" presentations, a framework for easily creating beautiful presentations using HTML.
# https://cran.rstudio.com/web/packages/revealjs/
pacman::p_load("revealjs")

# ensurer: Ensure Values at Runtime
# https://cran.r-project.org/web/packages/ensurer/vignettes/ensurer.html
pacman::p_load("ensurer")

# RZabbix: R Module for Working with the "Zabbix API"
pacman::p_load("RZabbix")

# R functions to split concatenated data, conveniently stack columns of data.frames, and conveniently reshape data.frames.
# https://github.com/mrdwab/splitstackshape
pacman::p_load("splitstackshape")

# Tufte Styles for R Markdown Documents. http://rstudio.github.io/tufte
pacman::p_load_gh("rstudio/tufte")
# or
# pacman::p_load("tufte")

# Syntax Highlighting for R Source Code 
pacman::p_load("highr")

# Crosstalk. [Experimental] Inter-htmlwidget communication (with and without Shiny)
# https://github.com/rstudio/crosstalk
# pacman::p_load_gh("ropensci/plotly@joe/feature/crosstalk")
devtools::install_github("rstudio/DT@joe/feature/crosstalk")

# Announcing ShinyTester Ц a package that helps you build Shiny apps
# http://amitkohli.com/announcing-shinytester-a-package-that-helps-you-build-shiny-apps/
pacman::p_load("ShinyTester")


# R scatter plot htmlwidget based on D3.js https://github.com/juba/scatterD3
pacman::p_load("scatterD3")
# or
pacman::p_load_gh("juba/scatterD3")

# DiagrammeR: Create graph diagrams and flowcharts using R. https://github.com/rich-iannone/DiagrammeR
pacman::p_load_gh("rich-iannone/DiagrammeR")
pacman::p_load_gh("rich-iannone/DiagrammeRsvg")


pacman::p_load("rio")


# thief : Temporal Hierarchical Forecasting: https://mran.revolutionanalytics.com/package/thief/
pacman::p_load("thief")


# Trelliscope: Detailed Visualization of Large Complex Data in R https://github.com/tesseradata/trelliscope
pacman::p_load("trelliscope")
# or
# pacman::p_load_gh("tesseradata/datadr")
# pacman::p_load_gh("tesseradata/trelliscope")

# дл€ решени€ проблем с phantomjs
pacman::p_load("webshot")

# timevis - Create interactive timeline visualizations in R
pacman::p_load("timevis")

# FFTrees: Generate, Visualise, and Compare Fast and Frugal Decision Trees
# https://cran.rstudio.com/web/packages/FFTrees/index.html
pacman::p_load("FFTrees")

# https://deployr.revolutionanalytics.com/documents/getting-started/data-scientist/
pacman::p_load_gh("deployr/deployrUtils")

# Archivist: store selected artifacts as binary files together with their metadata and relations.
# http://pbiecek.github.io/archivist/
# pacman::p_load_gh("pbiecek/archivist")
# or 
# archivist.github: Tools for Archiving, Managing and Sharing R Objects via GitHub
pacman::p_load("archivist")
pacman::p_load("archivist.github")

# corrr: Correlations in R https://cran.r-project.org/web/packages/corrr/vignettes/using-corrr.html
# https://drsimonj.svbtle.com/corrr-021-now-on-cran
pacman::p_load("corrr")

# gganimate: Create easy animations with ggplot2
# https://github.com/dgrtwo/gganimate
pacman::p_load_gh("dgrtwo/gganimate")

# Plotluck - УIТm feeling luckyФ for ggplot: https://mran.revolutionanalytics.com/web/packages/plotluck/vignettes/plotluck.html
pacman::p_load("plotluck")

# Welcome to the Highcharter homepage
# http://jkunst.com/highcharter/, https://github.com/jbkunst/highcharter
pacman::p_load("highcharter")

# https://plot.ly/r/getting-started/
pacman::p_load_gh("ropensci/plotly")

# https://github.com/rstudio/shiny
pacman::p_load("shiny")
# or
# pacman::p_load_gh("rstudio/shiny")

# https://www.shinyapps.io/admin/#/dashboard
# The shinyapps R package is deprecated and has been replaced by the rsconnect package.
# pacman::p_load_gh("rstudio/shinyapps")
devtools::install_github("rstudio/rsconnect")

# http://rstudio.github.io/shinydashboard/get_started.html
pacman::p_load("shinydashboard")

# http://rstudio.github.io/shinythemes/
pacman::p_load("shinythemes")

# https://cran.r-project.org/web/packages/shinyjs/index.html
pacman::p_load_gh("daattali/shinyjs")
# or
# pacman::p_load("shinyjs")

# lightsout - Implementation of the 'Lights Out' Puzzle Game in R: https://github.com/daattali/lightsout
pacman::p_load("lightsout")

# Preparing Network Data in R: http://www.mjdenny.com/Preparing_Network_Data_In_R.html
pacman::p_load("statnet")

# Get started with R igraph: http://igraph.org/r/
pacman::p_load("igraph")


# piratepal Ц An R pirateТs source of palettes inspired by everything from the Mona Lisa to the Evil Dead
# http://nathanieldphillips.com/2015/10/piratepal-an-r-color-palette-function-for-pirates-or-how-to-plot-the-mona-lisa/
pacman::p_load_gh("ndphillips/yarrr")

# Wes Anderson Palettes
pacman::p_load_gh("karthik/wesanderson")
# or
# pacman::p_load("wesanderson")

# colorspace: Color Space Manipulation https://cran.r-project.org/web/packages/colorspace/index.html
pacman::p_load("colorspace")


# artyfarty ggplot2 theme presets https://github.com/Bart6114/artyfarty
pacman::p_load_gh("bart6114/artyfarty")

# The viridis color palettes
# https://cran.r-project.org/web/packages/viridis/vignettes/intro-to-viridis.html
pacman::p_load("viridis")

# http://ramnathv.github.io/rCharts/
pacman::p_load_gh("ramnathv/rCharts")

# https://github.com/ramnathv/slidify
pacman::p_load_gh("ramnathv/slidify")
pacman::p_load_gh("ramnathv/slidifyLibraries")

# https://github.com/ramnathv/htmlwidgets
pacman::p_load_gh("ramnathv/htmlwidgets")

# flexdashboard: Easy interactive dashboards for R
pacman::p_load("flexdashboard")
# install.packages("flexdashboard", type = "source")
# ===========

# https://rstudio.github.io/DT/
pacman::p_load_gh("rstudio/DT")

# http://hrbrmstr.github.io/metricsgraphics/
pacman::p_load_gh("hrbrmstr/metricsgraphics")

# https://github.com/twitter/AnomalyDetection
pacman::p_load_gh("twitter/AnomalyDetection")
# library(AnomalyDetection)

# https://github.com/twitter/BreakoutDetection
pacman::p_load_gh("twitter/BreakoutDetection")

# https://github.com/robjhyndman/anomalous
pacman::p_load_gh("robjhyndman/anomalous")

# https://github.com/cttobin/ggthemr
pacman::p_load_gh("cttobin/ggthemr")

#  http://ipub.com/data-tree-0-3-apple-on-cran/
pacman::p_load("data.tree")

# http://ipub.com/ahp-package/
# Model and analyse complex decision making problems using the Analytic Hierarchy Process (AHP) by Thomas Saaty.
pacman::p_load("ahp")
# or
# pacman::p_load_gh("gluc/ahp")

pacman::p_load("mongolite")

# http://jason.bryer.org/timeline/
pacman::p_load("timeline")
# or
pacman::p_load_gh("jbryer/timeline")

# CRAN
pacman::p_load("sjPlot")
# ломаетс€ при попытке поставить NLoptR:  http://ab-initio.mit.edu/nlopt/nlopt-2.4.2.tar.gz

 # CRAN
# http://directlabels.r-forge.r-project.org/
pacman::p_load("directlabels")

#Read flat files (csv, tsv, fwf) into R
pacman::p_load_gh("hadley/readr")

# Get data out of excel and into R with readxl
# http://blog.rstudio.org/2015/04/15/readxl-0-1-0/
pacman::p_load("readxl")
# or
# pacman::p_load_gh("hadley/readxl")

# https://www.opencpu.org/posts/curl-release-0-8/
# https://github.com/hadley/httr
# https://cran.r-project.org/web/packages/httr/vignettes/quickstart.html
pacman::p_load_gh("hadley/httr")

# http://datadrivensecurity.info/blog/posts/2015/Jul/just-the-text-maam/
pacman::p_load_gh("hadley/xml2")
pacman::p_load_gh("hadley/rvest")

# https://github.com/hadley/testthat
pacman::p_load_gh("hadley/testthat")

# https://github.com/hadley/lazyeval
pacman::p_load("lazyeval")
# or
# pacman::p_load_gh("hadley/lazyeval")

# https://github.com/hadley/secure
pacman::p_load_gh("s-u/PKI") # needed for bug fixes not currently on CRAN
pacman::p_load_gh("hadley/secure")

# http://mockquant.blogspot.jp/2015/07/sparkrext-sparkr-extension-for-closer.html
pacman::p_load_gh("hoxo-m/SparkRext") 
# ломаетс€ при попытке поставить NLoptR:  http://ab-initio.mit.edu/nlopt/nlopt-2.4.2.tar.gz

# Tibbles are a modern reimagining of the data.frame, keeping what time has proven to be effective, and throwing out what is not.
# http://blog.rstudio.org/2016/03/24/tibble-1-0-0/
pacman::p_load("tibble")

# Purrr makes your pure functions purr by completing R"s functional programming tools with important features from other languages, in the style of the JS packages underscore.js, lodash and lazy.js.
pacman::p_load("purrr")
# or
# pacman::p_load_gh("hadley/purrr")

# http://blog.rstudio.org/2015/09/13/tidyr-0-3-0/
pacman::p_load("tidyr")

# Hadley forcats provides tools for categorical variables (forcats is an anagram of factors).
pacman::p_load_gh("hadley/forcats")

# R"s data.table package extends data.frame. More info: https://github.com/Rdatatable/data.table/wiki
pacman::p_load("data.table")
# or
# devtools::install_github("data.table", repos = "https://Rdatatable.github.io/data.table", type = "source")

# http://varianceexplained.org/r/broom-intro/
pacman::p_load("broom")

# Prepare reproducible examples for posting to GitHub issues, Stack Overflow, etc.
# https://github.com/jennybc/reprex
# http://www.njtierney.com/r/rbloggers/2017/01/11/reprex-magic/
pacman::p_load_gh("jennybc/reprex")
# or 
pacman::p_load("reprex")

# simmer - Discrete-Event Simulator for R: http://fishyoperations.com/2017/01/12/extensions-for-simmer.html
pacman::p_load("simmer")

# xtable: Export tables to LaTeX or HTML
pacman::p_load("xtable")

# https://rstudio.github.io/leaflet/
pacman::p_load("leaflet")
# or 
# pacman::p_load_gh("rstudio/leaflet")

# https://rstudio.github.io/packrat/
pacman::p_load_gh("rstudio/packrat")

# Pull data from OpenTSDB into R
# https://github.com/holstius/opentsdbr/
pacman::p_load_gh("holstius/opentsdbr")

# partools: Tools for the 'Parallel' Package
# https://matloff.wordpress.com/2017/07/08/my-presentation-at-user-2017-etc/
# https://github.com/matloff/partools
# Miscellaneous utilities for parallelizing large computations. Alternative to MapReduce. 
# File splitting and distributed operations such as sort and aggregate. 
pacman::p_load("partools")

# http://web.stanford.edu/~hastie/glmnet/glmnet_alpha.html
pacman::p_load("glmnet")

# https://mran.revolutionanalytics.com/packages/info/?rpca
pacman::p_load("rpca")

# https://github.com/hrbrmstr/docxtractr
pacman::p_load_gh("hrbrmstr/docxtractr")

# https://github.com/stefano-meschiari/latex2exp
# pacman::p_load("latex2exp")
pacman::p_load_gh("stefano-meschiari/latex2exp")

# visNetwork: Network Visualization using "vis.js" Library
# https://cran.r-project.org/web/packages/visNetwork/index.html
pacman::p_load("rvest")

# rvest: easy web scraping with R
# http://blog.rstudio.org/2014/11/24/rvest-easy-web-scraping-with-r/
pacman::p_load("visNetwork")

# eeptools: Convenience Functions for Education Data
# http://jaredknowles.com/journal/2015/9/22/version-090-of-eeptools-released
pacman::p_load("eeptools")

# BayesFactor: Computation of Bayes Factors for Common Designs
pacman::p_load("BayesFactor")

# wakefield is designed to quickly generate random data sets. The user passes n (number of rows) and predefined vectors to the r_data_frame function to produce a dplyr::tbl_df object.
# https://trinkerrstuff.wordpress.com/2015/04/30/wakefield-random-data-set-part-ii/
pacman::p_load_gh("trinker/wakefield")

# InfluxDB client
# https://github.com/dleutnant/influxdbr
pacman::p_load_gh("dleutnant/influxdbr")

# tidyjson
pacman::p_load_gh("sailthru/tidyjson")

# TriMatch: Propensity Score Matching of Non-Binary Treatments
# “ам есть функци€
# as.data.frame.list: Convert a list of vectors to a data frame.
# https://cran.r-project.org/web/packages/TriMatch/index.html
pacman::p_load("TriMatch")

# TaskScheduleR. Schedule R scripts/processes with the Windows task scheduler. 
# This allows R users working on Windows to automate R processes on specific timepoints from R itself.
# https://github.com/jwijffels/taskscheduleR
# windows only package
pacman::p_load_gh("bnosac/taskscheduleR")
# On nix hosts: A simple R package for managing your cron jobs.
# nix only package
pacman::p_load_gh("bnosac/cronR")

#
# https://github.com/jbkunst/highcharter
pacman::p_load_gh("jbkunst/highcharter")
# or
# pacman::p_load("highcharter")

# Seasonal decomposition in the ggplot2 universe with ggseas
# http://ellisp.github.io/blog/2016/03/28/ggseas-update/
pacman::p_load("ggseas")

# The validate package is intended to make checking your data easy, maintainable and reproducible.
# http://www.markvanderloo.eu/yaRb/2016/06/24/validate-version-1-5-is-out/
# https://cran.r-project.org/web/packages/validate/vignettes/intro.html
pacman::p_load("validate")

# rdrop2 - Dropbox interface from R a_box
pacman::p_load("rdrop2")
# or Development version
# pacman::p_load_gh("karthik/rdrop2")

# rbokeh - R Interface to Bokeh
# http://hafen.github.io/rbokeh/#installation
pacman::p_load("rbokeh")

# git2r - An R Package to Interface Git
pacman::p_load("git2r")
# or
# pacman::p_load_gh("ropensci/git2r")


stop()

# =====================================================================================
# library(ggplot2) #load first! (Wickham)
# library(lubridate) #load second!


# A better way to pick colours with ggplot2
# http://amitkohli.com/best-way-to-make-a-custom-palette-with-ggplot2/
# pacman::p_load_gh("MarcoDVisser/choosecolor")
# Ќ≈ –јЅќ“ј≈“


# Rperform is a package that allows R developers to track quantitative performance metrics of their code.
# https://github.com/analyticalmonk/rperform
pacman::p_load_gh("analyticalmonk/Rperform")

# The githubinstall package provides a way to install packages on GitHub by only their package names just like install.packages().
# https://cran.r-project.org/web/packages/githubinstall/vignettes/githubinstall.html
pacman::p_load("githubinstall")

# You can also install the package from GitHub.

# pacman::p_load("devtools") # if you have not installed "devtools" package
# pacman::p_load_gh("hoxo-m/githubinstall")

# The pacman package is an R package management tool that combines the functionality of base library related functions into intuitively named functions.
# https://github.com/trinker/pacman
pacman::p_load("pacman")
# or
# pacman::p_load_gh("trinker/pacman")


# http://vnijs.github.io/blog/2015/05/introducing-radiant.html
install.packages("radiant", repos = "http://vnijs.github.io/radiant_miniCRAN/") 
# run
##library(radiant); radiant("marketing")

# Feather: A Fast On-Disk Format for Data Frames for R and Python, powered by Apache Arrow
# http://blog.rstudio.org/2016/03/29/feather/
devtools::install_github("wesm/feather/R")

# littler provides the r program, a simplified command-line interface for GNU R.
# http://dirk.eddelbuettel.com/code/littler.html
pacman::p_load("littler")


# An Introduction to The printr Package. The printr (read УprinterФ or Уprint RФ) package is a companion package to knitr.
# http://yihui.name/printr/
install.packages(
  "printr",
  type = "source",
  repos = c("http://yihui.name/xran", "http://cran.rstudio.com")
)

# репозиторий работает криво, поэтому под линухом делаем так:
# wget https://yihui.name/xran/src/contrib/printr_0.0.6.tar.gz
#### install.packages(<pathtopackage>, repos = NULL, type="source")
# install.packages("printr_0.0.6.tar.gz", repos = NULL, type="source")


# пакеты, либо недоступные под Linux, либо криво встающие
# ========================================================

# excel.link: Convenient Data Exchange with Microsoft Excel
# отстутсвует в *nix репозитории 
pacman::p_load("excel.link")

# gWidgetstcltk: Toolkit implementation of gWidgets for tcltk package
# Port of the gWidgets API to the tcltk package. Requires Tk 8.5 or greater.
# используетс€ extremevalues. ѕод nix рушитс€ при компил€ции 'no DISPLAY variable so Tk is not available'
# требует установки всей X11 подсистемы (куча пакетов)
#	sudo yum install xorg-x11-server-Xvfb
#	sudo -i xvfb-run R --no-save
#	chooseCRANmirror(graphics=FALSE, ind=37)
#	pacman::p_load("gWidgetstcltk")
install.packages('gWidgets', dep=TRUE)
pacman::p_load("gWidgetstcltk")
pacman::p_load("extremevalues")
