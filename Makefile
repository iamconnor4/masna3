init: ## Initialise the project for local development
	@./scripts/init.sh

build: ## Build the project
	@cabal build all

clean: ## Remove compilation artifacts
	@cabal clean

repl: ## Start a REPL
	@cabal repl all

test:  ## Run the test suite
	./scripts/run-tests.sh

watch-test: ## Load the tests in ghcid and reload them on file change
	./scripts/run-tests.sh --watch

style: ## Run the code stylers
	@cd server ; cabal-gild --mode=format --io=masna3.cabal
	@cd api ; cabal-gild --mode=format --io=masna3-api.cabal
	@cabal-gild --mode=format --io=cabal.project
	@fourmolu -q --mode inplace server api
	@find server api -name "*.hs" | xargs -P $(PROCS) -I {} hlint --refactor-options="-i" --refactor {}

style-quick: ## Run the code stylers are changed files
	@cd server ; cabal-gild --mode=format --io=masna3.cabal
	@cd api ; cabal-gild --mode=format --io=masna3-api.cabal
	@cabal-gild --mode=format --io=cabal.project
	@git diff origin --name-only api server | xargs -P $(PROCS) -I {} fourmolu -q -i {}
	@git diff origin --name-only api server | xargs -P $(PROCS) -I {} hlint --refactor-options="-i" --refactor {}

tags: ## Generate ctags for the project with `ghc-tags`
	@ghc-tags -c api server

docker-build: ## Build and start the container cluster
	@docker compose build devel

docker-up: ## Start the container cluster
	@docker compose up -d

docker-stop: ## Stop the container cluster without removing the containers
	@docker compose stop

docker-down: ## Stop and remove the container cluster
	@docker compose down

docker-enter: ## Enter the docker environment
	docker compose exec devel bash

migration: ## Generate timestamped database migration boilerplate files
	@if test -z "$$name"; then \
	  echo "Usage: make migration name=some-name"; \
	else \
	  migName="`date -u '+%Y%m%d%H%M%S'`_$$name"; \
	  fname="migrations/$$migName.sql"; \
	  touch "$$fname"; \
	  echo "Created $$fname";\
	fi

db-setup: db-create db-init db-migrate ## Setup the dev database

db-create: ## Create the database
	@createdb -h $(MASNA3_DB_HOST) -p $(MASNA3_DB_PORT) -U $(MASNA3_DB_USER) $(MASNA3_DB_DATABASE)

db-drop: ## Drop the database
	@dropdb -f --if-exists -h $(MASNA3_DB_HOST) -p $(MASNA3_DB_PORT) -U $(MASNA3_DB_USER) $(MASNA3_DB_DATABASE)

db-init: ## Create the database schema
	@migrate init "$(MASNA3_DB_CONNSTRING)"

db-migrate: ## Apply database migrations
	@migrate migrate "$(MASNA3_DB_CONNSTRING)" migrations

db-reset: db-drop db-setup ## Reset the dev database

help: ## Display this help message
	@grep -E '^[a-zA-Z_-]+:.*?## .*$$' $(MAKEFILE_LIST) | awk 'BEGIN {FS = ":.* ?## "}; {printf "\033[36m%-30s\033[0m %s\n", $$1, $$2}'

PROCS := $(shell nproc)

.PHONY: all $(MAKECMDGOALS)

.DEFAULT_GOAL := help
