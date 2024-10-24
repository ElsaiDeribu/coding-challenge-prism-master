FROM postgres:14.1-alpine

ENV AUTO_INIT=/docker-entrypoint-initdb.d

COPY ./create-tables.sql ${AUTO_INIT}/create-tables.sql
ENV POSTGRES_PASSWORD=draftbit

RUN set -e \
  # Initialize DB \
  && nohup bash -c "docker-entrypoint.sh postgres &" \
  # Wait for initialization to complete via poll \
  && until psql -U postgres -c '\l'; do sleep 3; done \
  # Gently shutdown postgres to avoid repair-on-start \
  && su - postgres -c "pg_ctl stop --pgdata /var/lib/postgresql/data"
