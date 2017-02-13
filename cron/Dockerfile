FROM rocker/hadleyverse
MAINTAINER Chris Haid <chaid@kippchicago.org>


RUN apt-get update -y && apt-get install -y  \
    libbz2-dev \
    libpcre3-dev \
    liblzma-dev

# add R packages necessary for jobs to run.

RUN R -e "install.packages(c('ETLUtils', 'RJDBC'))"

RUN R -e "devtools::install_github('imanuelcostigan/RSQLServer')"

RUN R -e "devtools::install_github('jennybc/googlesheets')"

RUN R -e "devtools::install_github('almartin82/mapvizieR')"

RUN R -e "devtools::install_github('rstudio/flexdashboard')"

RUN R -e "devtools::install_github('rstudio/DT')"

RUN R -e "install.packages('zoo')"

RUN R -e "devtools::install_github('hadley/dplyr')"

RUN R -e "devtools::install_github('chrishaid/silounloadr')"

RUN R -e "devtools::install_github('chrishaid/deanslistr')"

RUN R -e "install.packages('futile.logger')"

RUN R -e "install.packages('googleCloudStorageR')"

RUN R -e "install.packages('janitor')"


# add Python for scripts that require python


# install cron and update tz

RUN apt-get update &&\
    apt-get install -y --no-install-recommends \
    cron && \
    cp /usr/share/zoneinfo/America/Chicago /etc/localtime && \
    echo "America/Chicago" > /etc/timezone


#RUN install supervisor
RUN   apt-get install -y supervisor

# add jobs
#RUN mkdir /jobs
COPY jobs /jobs

# add crontab
COPY crontab /etc/cron.d/idea-cron

# ADD supervisrd.conf
COPY supervisord.conf /etc/supervisor/conf.d/supervisord.conf

# set permissions

RUN chmod 0644 /etc/cron.d/idea-cron

RUN mkdir /data && chmod 777 /data

# Create the log file to be able to run tail
RUN touch /var/log/cron.log

# load crontab
RUN crontab /etc/cron.d/idea-cron

# Run the command on container startup


CMD ["/usr/bin/supervisord", "-c", "/etc/supervisor/conf.d/supervisord.conf"]
#CMD /usr/bin/supervisrd/supervisord -c  /etc/supervisor/conf.d/supervisord.conf &&\
#    cron && tail -f /var/log/cron.log && \
#    chown -R rstudio:staff /data
