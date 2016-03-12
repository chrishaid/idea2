FROM rocker/hadleyverse
MAINTAINER Chris Haid <chaid@kippchicago.org>


# add R packages necessary for jobs to run.

RUN R -e "install.packages(c('ETLUtils', 'RJDBC'))" && \
    R -e "devtools::install_github('chrishaid/RSQLServer')" && \
    R -e "devtools::install_github('almartin82/mapvizieR')"

# add Python for scripts that require python


# install cron and update tz

RUN apt-get update &&\
    apt-get install -t unstable -y --no-install-recommends \
    cron && \
    cp /usr/share/zoneinfo/America/Chicago /etc/localtime && \
    echo "America/Chicago" > /etc/timezone

#RUN apt-get install supervisor
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

# Run the command on container startup
CMD ["/usr/bin/supervisord", "-c", "/etc/supervisor/conf.d/supervisord.conf"]
#CMD /usr/bin/:d/supervisord -c  /etc/supervisor/conf.d/supervisord.conf &&\
#    cron && tail -f /var/log/cron.log && \
#    chown -R rstudio:staff /data
