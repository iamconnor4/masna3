# libpq-compatible connection string
export MASNA3_DB_CONNSTRING="host=database dbname=masna3_dev user=postgres password=postgres port=5433"
# Number of connections across all sub-pools
export MASNA3_DB_POOL_CONNECTIONS="50"
#Timeout for each connection
export MASNA3_DB_TIMEOUT="10"
# `true` if the service is deployed somewhere, `false` for local development / tests
export MASNA3_DEPLOYED="false"
# URL domain
export MASNA3_DOMAIN="localhost"
# HTTP port
export MASNA3_HTTP_PORT="8085"
# Where do the logs go
export MASNA3_LOGGING_DESTINATION="stdout"
# Is Prometheus metrics export enabled (default false)
export MASNA3_PROMETHEUS_ENABLED="true"
# Public key for Biscuit authorisation
export MASNA3_PUBLIC_KEY=""
# Secret key for Biscuit authorisation
export MASNA3_SECRET_KEY=""
# Is Zipkin trace collection enabled? (default false)
export MASNA3_ZIPKIN_ENABLED="false"
# The hostname of the Zipkin collection agent
export MASNA3_ZIPKIN_AGENT_HOST=""
# The port of the Zipkin collection agent
export MASNA3_ZIPKIN_AGENT_PORT=""

export MASNA3_AWS_KEY_ID="fake-key-id"

export MASNA3_AWS_SECRET_KEY="fake-id"

export MASNA3_AWS_REGION="Paris"

export MASNA3_AWS_BUCKET="masna3-dev"

export DOCKER_BUILDKIT=1
