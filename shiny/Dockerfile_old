FROM rocker/r-devel

MAINTAINER Chris Haid "chaid@kippchicago.org"

# Winston Chang wrote the following to get shiny up and running.
RUN apt-get update && apt-get install -y -t unstable \
    sudo \
    gdebi-core \
    pandoc \
    pandoc-citeproc \
    libcurl4-gnutls-dev \
    libcairo2-dev/unstable \
    libxt-dev \
    libssl-dev \
    libxml2-dev

# Download and install libssl 0.9.8
RUN wget --no-verbose http://ftp.us.debian.org/debian/pool/main/o/openssl/libssl0.9.8_0.9.8o-4squeeze14_amd64.deb && \
    dpkg -i libssl0.9.8_0.9.8o-4squeeze14_amd64.deb && \
    rm -f libssl0.9.8_0.9.8o-4squeeze14_amd64.deb

# Download and install shiny server
RUN wget --no-verbose https://s3.amazonaws.com/rstudio-shiny-server-os-build/ubuntu-12.04/x86_64/VERSION -O "version.txt" && \
    VERSION=$(cat version.txt)  && \
    wget --no-verbose "https://s3.amazonaws.com/rstudio-shiny-server-os-build/ubuntu-12.04/x86_64/shiny-server-$VERSION-amd64.deb" -O ss-latest.deb && \
    gdebi -n ss-latest.deb && \
    rm -f version.txt ss-latest.deb


RUN R -e "install.packages(c('devtools', 'dplyr', 'ggplot2', 'lubridate', 'purrr', 'RCurl', 'rmarkdown', 'shiny', 'xtable', 'Cairo', 'shinydashboard', 'DT'), repos='https://cran.rstudio.com/')"

RUN R -e "devtools::install_github('hadley/readr')"

RUN R -e "devtools::install_github('jennybc/googlesheets')"

# Set timezone to Chicago
RUN cp /usr/share/zoneinfo/America/Chicago /etc/localtime && \
    echo "America/Chicago" > /etc/timezone

# RUN cp -R /usr/local/lib/R/site-library/shiny/examples/* /srv/shiny-server/
RUN rm -rf /usr/local/lib/R/site-library/shiny/examples

EXPOSE 3838

# Copy front matter to /srv/shiny-server
RUN rm /srv/shiny-server/index.html
COPY ./front-matter /srv/shiny-server/


COPY ./recruit/ /srv/shiny-server/recruit

COPY ./assets/ /var/www/

COPY ./war /srv/shiny-server/war

COPY shiny-server.sh /usr/bin/shiny-server.sh

CMD ["/usr/bin/shiny-server.sh"]
