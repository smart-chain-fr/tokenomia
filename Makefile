# The plutus-pab commands, contracts and hoogle environment
# are made availible by the nix shell defined in shell.nix.
# In most cases you should execute Make after entering nix-shell.

.PHONY: hoogle nix_build build test watch ghci \
	readme_contents format lint requires_nix_shell

usage:
	@echo "usage: make <command>"
	@echo
	@echo "Available commands:"
	@echo "  hoogle              -- Start local hoogle"
	@echo "  nix_build           -- Run nix build -L"
	@echo "  build               -- Run cabal v2-build"
	@echo "  watch               -- Track files: bot-plutus-interface.cabal, src/* and run 'make build' on change"
	@echo "  test                -- Run cabal v2-test"
	@echo "  ghci                -- Run cabal v2-repl bot-plutus-interface"
	@echo "  format              -- Apply source code formatting with fourmolu"
	@echo "  format_check        -- Check source code formatting without making changes"
	@echo "  cabalfmt            -- Apply cabal formatting with cabal-fmt"
	@echo "  cabalfmt_check      -- Check cabal files for formatting errors without making changes"
	@echo "  nixpkgsfmt          -- Apply nix formatting with nixfmt"
	@echo "  nixpkgsfmt_check    -- Check nix files for format errors"
	@echo "  lint                -- Check the sources with hlint"
	@echo "  readme_contents     -- Add table of contents to README"
	@echo "  clear_build         -- Deletes the build files for this specific project"

hoogle: requires_nix_shell
	hoogle server --local --port=8070 > /dev/null &

ifdef FLAGS
GHC_FLAGS = --ghc-options "$(FLAGS)"
endif

nix_build:
	nix build -L .#check.x86_64-linux

build: requires_nix_shell
	cabal v2-build $(GHC_FLAGS)

watch: requires_nix_shell
	while sleep 1; do find src bot-plutus-interface.cabal | entr -cd make build; done

test: requires_nix_shell
	cabal v2-test

ghci: requires_nix_shell
	cabal v2-repl $(GHC_FLAGS) bot-plutus-interface

# Source dirs to run fourmolu on
FORMAT_SOURCES := $(shell fd -e hs)

# Extensions we need to tell fourmolu about
FORMAT_EXTENSIONS := -o -XTemplateHaskell -o -XTypeApplications -o -XImportQualifiedPost -o -XPatternSynonyms -o -fplugin=RecordDotPreprocessor

# Run fourmolu formatter
format: requires_nix_shell
	@fourmolu --mode inplace --check-idempotence $(FORMAT_EXTENSIONS) $(FORMAT_SOURCES)

# Check formatting (without making changes)
format_check: requires_nix_shell
	@fourmolu --mode check --check-idempotence $(FORMAT_EXTENSIONS) $(FORMAT_SOURCES)

# Cabal package definitions
CABAL_SOURCES := $(shell fd -e cabal)

cabalfmt: requires_nix_shell
	cabal-fmt --inplace $(CABAL_SOURCES)

cabalfmt_check: requires_nix_shell
	cabal-fmt --check $(CABAL_SOURCES)

# Nix files to format
NIX_SOURCES := $(shell fd -e nix)

nixpkgsfmt: requires_nix_shell
	nixpkgs-fmt $(NIX_SOURCES)

nixpkgsfmt_check: requires_nix_shell
	nixpkgs-fmt --check $(NIX_SOURCES)

lint: requires_nix_shell
	@hlint $(FORMAT_SOURCES)

lint_fix: requires_nix_shell
	echo $(FORMAT_SOURCES) | xargs -t -n1 hlint --refactor --refactor-options="--inplace"

readme_contents:
	echo "this command is not nix-ified, you may receive an error from npx"
	npx markdown-toc ./README.md --no-firsth1

# Target to use as dependency to fail if not inside nix-shell
requires_nix_shell:
	@ [ "$(IN_NIX_SHELL)" ] || echo "The $(MAKECMDGOALS) target must be run from inside nix-shell"
	@ [ "$(IN_NIX_SHELL)" ] || (echo "    run 'nix-shell --pure' first" && false)
