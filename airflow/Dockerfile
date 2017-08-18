FROM kippchicago/idea2-cron:airflow
MAINTAINER Chris Haid <chaid@kippchicago.org>

# Never prompts the user for choices on installation/configuration of packages
ENV DEBIAN_FRONTEND noninteractive
ENV TERM linux

# Airflow
ARG AIRFLOW_VERSION=1.8.0
ARG AIRFLOW_HOME=/etc/airflow

#Postgres
ENV POSTGRES_HOST=postgres
ENV POSTGRES_PORT=5432
ENV POSTGRES_USER=airflow
ENV POSTGRES_PASSWORD=airflow
ENV POSTGRES_DB=airflow



# Define en_US.
ENV LANGUAGE en_US.UTF-8
ENV LANG en_US.UTF-8
ENV LC_ALL en_US.UTF-8
ENV LC_CTYPE en_US.UTF-8
ENV LC_MESSAGES en_US.UTF-8
ENV LC_ALL en_US.UTF-8

RUN set -ex \
    && buildDeps=' \
        python-dev \
        libkrb5-dev \
        libsasl2-dev \
        libssl-dev \
        libffi-dev \
        build-essential \
        libblas-dev \
        liblapack-dev \
        libpq-dev \
        git \
    ' \
    && apt-get update -yqq \
    && apt-get install -yqq --no-install-recommends \
        $buildDeps \
        python-pip \
        python-requests \
        apt-utils \
        curl \
        netcat \
        locales \
        vim \
        python3-pip \
        python3-setuptools \
    && sed -i 's/^# en_US.UTF-8 UTF-8$/en_US.UTF-8 UTF-8/g' /etc/locale.gen \
    && locale-gen \
    && update-locale LANG=en_US.UTF-8 LC_ALL=en_US.UTF-8 \
    && useradd -ms /bin/bash -d ${AIRFLOW_HOME} airflow \
    && python -m pip install -U pip \
    && pip install Cython \
    && pip install pytz \
    && pip install pyOpenSSL \
    && pip install ndg-httpsclient \
    && pip install pyasn1 \
    && pip install airflow[crypto,postgres,hdfs,jdbc]==$AIRFLOW_VERSION \
    && pip3 install requests oauth2client gcloud retrying python-dateutil ConfigParser\
    #&& pip install celery[redis]==3.1.17 \
    && apt-get remove --purge -yqq $buildDeps \
    && apt-get clean \
    && rm -rf \
        /var/lib/apt/lists/* \
        /tmp/* \
        /var/tmp/* \
        /usr/share/man \
        /usr/share/doc \
        /usr/share/doc-base

COPY entrypoint.sh /entrypoint.sh
COPY config/airflow.cfg ${AIRFLOW_HOME}/airflow.cfg

# COPY supervisrd.conf
COPY supervisord.conf /etc/supervisor/conf.d/supervisord.conf

# add jobs
COPY ./jobs /jobs

# Need to copy datarobot helpers to site-packages
#RUN mkdir /usr/local/lib/python3.5/site-packages
RUN mv /jobs/silo/powerschool/datarobot_helpers /usr/local/lib/python3.5/dist-packages/datarobot_helpers

RUN mkdir /var/log/airflow
RUN mkdir /etc/airflow/dags

# add dags
COPY ./dags /etc/airflow/dags

# This next line is necessary to get everything up and running nicely.
# Dags need to be in the dag folder for the scheduler, db, and webserver to paly nicely
#RUN cp -rf /usr/local/lib/python2.7/dist-packages/airflow/example_dags/* ${AIRFLOW_HOME}/dags/
RUN chown -R airflow: ${AIRFLOW_HOME}
RUN chown -R airflow: /etc/supervisor/
RUN chown -R airflow: /var/log/
#RUN chown -R airflow: /jobs

#add
RUN echo 'airflow ALL=(ALL) NOPASSWD: ALL' > /etc/sudoers

EXPOSE 8080

#ENV FERNET_KEY=kYaLE7G8dtKsVMfoIuXGl2kLvvDOzkEg0lI9ssKy7N4=

USER airflow
WORKDIR ${AIRFLOW_HOME}
ENTRYPOINT ["/entrypoint.sh"]
#CMD ["/usr/bin/supervisord", "-c", "/etc/supervisor/conf.d/supervisord.conf"]
