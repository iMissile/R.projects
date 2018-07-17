FROM rocker/r-base

# ищем по пакетам: https://www.debian.org/distrib/packages#search_packages
RUN apt-get update -q && \
    apt-get -y --no-install-recommends install \
    chrony \
    wget \
    tree \
    libcurl4-openssl-dev \
    libxml2-dev \
    libcairo2-dev \
    libsasl2-dev \
    libpng-dev \
    libjpeg-dev \
    python \
    python-dev \
    libegl1-mesa \
    libegl1-mesa-dev \
    libglu1-mesa \
    libglu1-mesa-dev \
    libgmp-dev \
    libmpfr-dev \
    libxt-dev \
    gtk2-engines \
    libv8-dev \
    libudunits2-0 \
    # x11-xserver-utils \
    unixODBC* \
    postgresql \
    libmariadb-dev \
    libmysqlclient-dev \
    gfortran-6 \
    texlive* \
    ufw \
    dejavu* \
    rrdtool \
    psmisc \
    lrzsz \
    gdal* \
    libproj-dev \
    r-cran-rprotobuf \
    libprotobuf-dev \
    libgeos-dev

# install packages
RUN install2.r --error --deps TRUE \
    tidyverse \
    lubridate \
    glue \
    scales \
    forcats \
    readxl \
    magrittr \
    stringi \
    futile.logger \
    jsonlite \
    Cairo \
    RColorBrewer \
    extrafont \
    hrbrthemes \
    shiny \
    shinyjqui \
    shinythemes \
    shinyBS \
    shinyjs \
    shinyWidgets \
    shinycssloaders \
    anytime \
    tictoc \
    re2r \
    officer \
    openxlsx \
    assertr \
    checkmate \
    promises \
    future
