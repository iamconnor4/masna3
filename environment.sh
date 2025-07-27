# libpq-compatible connection string
export MASNA3_DB_CONNSTRING=""
# Number of connections across all sub-pools
export MASNA3_DB_POOL_CONNECTIONS=""
#Timeout for each connection
export MASNA3_DB_TIMEOUT=""
# `true` if the service is deployed somewhere, `false` for local development / tests
export MASNA3_DEPLOYED=""
# URL domain
export MASNA3_DOMAIN=""
# HTTP port
export MASNA3_HTTP_PORT=""
# Where do the logs go
export MASNA3_LOGGING_DESTINATION=""
# Is Prometheus metrics export enabled (default false)
export MASNA3_PROMETHEUS_ENABLED=""
# Public key for Biscuit authorisation
export MASNA3_PUBLIC_KEY=""
# Secret key for Biscuit authorisation
export MASNA3_SECRET_KEY=""
# Is Zipkin trace collection enabled? (default false)
export MASNA3_ZIPKIN_ENABLED=""
# The hostname of the Zipkin collection agent
export MASNA3_ZIPKIN_AGENT_HOST=""
# The port of the Zipkin collection agent
export MASNA3_ZIPKIN_AGENT_PORT=""

export DOCKER_BUILDKIT=1
