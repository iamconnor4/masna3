build: ## Build the project
	@cabal build all

clean: ## Remove compilation artifacts
	@cabal clean

repl: ## Start a REPL
	@cabal repl all

test: ## Run the test suite
	@cabal test all

style: ## Run the code stylers
	@cabal-gild --mode=format --io=server/masna3.cabal
	@cabal-gild --mode=format --io=api/masna3-api.cabal
	@cabal-gild --mode=format --io=cabal.project
	@fourmolu -q --mode inplace server api
	@find server api -name "*.hs" | xargs -P $(PROCS) -I {} hlint --refactor-options="-i" --refactor {}

help: ## Display this help message
	@grep -E '^[a-zA-Z_-]+:.*?## .*$$' $(MAKEFILE_LIST) | awk 'BEGIN {FS = ":.* ?## "}; {printf "\033[36m%-30s\033[0m %s\n", $$1, $$2}'

PROCS := $(shell nproc)

.PHONY: all $(MAKECMDGOALS)

.DEFAULT_GOAL := help
