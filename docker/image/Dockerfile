FROM rocker/r-ver:4.3.0
RUN apt-get update && apt-get install -y  git-core libcurl4-openssl-dev libgit2-dev libpq-dev libicu-dev libssl-dev libfontconfig-dev libcairo2-dev libxml2-dev make pandoc pandoc-citeproc && rm -rf /var/l>
RUN echo "options(repos = c(CRAN = 'https://cran.rstudio.com/'), download.file.method = 'libcurl', Ncpus = 4)" >> /usr/local/lib/R/etc/Rprofile.site
RUN R -e 'install.packages("shiny")'
RUN R -e 'install.packages("shinyBS")'
RUN R -e 'install.packages("shiny.router")'
EXPOSE 80
CMD R -e "options('shiny.port'=80,shiny.host='0.0.0.0');shiny::runApp('app');"
