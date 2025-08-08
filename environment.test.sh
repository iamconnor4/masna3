source environment.local.sh

export MASNA3_DB_DATABASE="masna3_test"

export MASNA3_DB_CONNSTRING="host=masna3-database dbname=masna3_test user=postgres password=postgres port=5432"

export MASNA3_ZIPKIN_AGENT_HOST="localhost"
export MASNA3_ZIPKIN_AGENT_PORT="8080"

export MASNA3_DB_CONNSTRING="host=${MASNA3_DB_HOST} dbname=${MASNA3_DB_DATABASE} port=${MASNA3_DB_PORT} \
  user=${MASNA3_DB_USER} password=${MASNA3_DB_PASSWORD}"
