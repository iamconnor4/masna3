init: ## Initialise the project for local development
	@./scripts/init.sh

build: ## Build the project
	@cabal build all

clean: ## Remove compilation artifacts
	@cabal clean

repl: ## Start a REPL
	@cabal repl all

test: ## Run the test suite
	@cabal test all

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

help: ## Display this help message
	@grep -E '^[a-zA-Z_-]+:.*?## .*$$' $(MAKEFILE_LIST) | awk 'BEGIN {FS = ":.* ?## "}; {printf "\033[36m%-30s\033[0m %s\n", $$1, $$2}'

PROCS := $(shell nproc)

.PHONY: all $(MAKECMDGOALS)

.DEFAULT_GOAL := help
