FROM rocker/r-ver:4.3.0
RUN apt-get update && \
    apt-get install -y  \
        git-core \
        libcurl4-openssl-dev \
        libgit2-dev \
        libpq-dev \
        libicu-dev \
        libssl-dev \
        libfontconfig-dev \
        libcairo2-dev \
        libxml2-dev \
        make \
        pandoc \
        pandoc-citeproc && \
    rm -rf /var/lib/apt/lists/*
# First Packages with Standard-Installtion
RUN echo 'options(repos = c(CRAN = "https://cloud.r-project.org"), download.file.method = "libcurl", Ncpus = 4)' >>"${R_HOME}/etc/Rprofile.site"
RUN R -e 'install.packages("stringr")'
# Second Packages WITHOUT Compiling | Faster Installation
RUN echo 'options(repos = c(CRAN = "https://packagemanager.rstudio.com/all/__linux__/focal/latest"))' >>"${R_HOME}/etc/Rprofile.site"
RUN R -e 'install.packages("shiny")'
RUN R -e 'install.packages("shinyBS")'
RUN R -e 'install.packages("bslib")'
RUN R -e 'install.packages("bsplus")'
RUN R -e 'install.packages("shinyWidgets")'
RUN R -e 'install.packages("shinycssloaders")'
RUN R -e 'install.packages("shiny.router")'
RUN R -e 'install.packages("shinyjs")'
RUN R -e 'install.packages("reactable")'
RUN R -e 'install.packages("sortable")'
RUN R -e 'install.packages("visNetwork")'
RUN R -e 'install.packages("DBI")'
RUN R -e 'install.packages("RSQLite")'
RUN R -e 'install.packages("box")'
RUN R -e 'install.packages("dplyr")'
RUN R -e 'install.packages("cicerone")'
RUN R -e 'install.packages("DT")'
RUN R -e 'install.packages("jsonlite")'
RUN R -e 'install.packages("purrr")'
RUN R -e 'install.packages("slickR")'
RUN R -e 'install.packages("yaml")'
RUN R -e 'install.packages("rlist")'
RUN R -e 'install.packages("data.table")'
RUN R -e 'install.packages("tidyr")'
RUN R -e 'install.packages("tibble")'
RUN R -e 'install.packages("duckdb")'
EXPOSE 80
CMD R -e "options('shiny.port'=80,shiny.host='0.0.0.0');shiny::runApp('app');"
