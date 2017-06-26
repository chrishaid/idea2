#!/usr/bin/env bash

AIRFLOW_HOME="/etc/airflow"
CMD="airflow"
TRY_LOOP="20"

: ${POSTGRES_HOST:="postgres"}
: ${POSTGRES_PORT:="5432"}
: ${POSTGRES_USER:="airflow"}
: ${POSTGRES_PASSWORD:="airflow"}
: ${POSTGRES_DB:="airflow"}

# Wait for Postgresql

i=0
while ! nc -z $POSTGRES_HOST $POSTGRES_PORT >/dev/null 2>&1 < /dev/null; do
  i=$((i+1))
  echo "$(date) - waiting for ${POSTGRES_HOST}:${POSTGRES_PORT}... $i/$TRY_LOOP"
  if [ $i -ge $TRY_LOOP ]; then
    echo "$(date) - ${POSTGRES_HOST}:${POSTGRES_PORT} still not reachable, giving up"
    exit 1
  fi
  sleep 10
done

echo "Initialize database..."
 $CMD initdb
 exec exec /usr/bin/supervisord -c /etc/supervisor/conf.d/supervisord.conf
