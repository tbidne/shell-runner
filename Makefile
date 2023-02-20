.PHONY: build clean repl watch ;\
	doctest ;\
	cic ci formatc format lint lintc ;\
	haddock haddockc hackage

# core

T = ""

clean:
	cabal clean

build:
	if [ -z "$(T)" ]; then \
		cabal build; \
	else \
		cabal build $(T); \
	fi

doctest:
	cabal build --write-ghc-environment-files=always; \
	RUN_DOCTEST=1 cabal test doctest; \
	rm .ghc.environment.*

repl:
	if [ -z "$(T)" ]; then \
		cabal repl shrun; \
	else \
		cabal repl $(T); \
	fi

watch:
	if [ -z "$(T)" ]; then \
		ghcid --command "cabal repl shrun"; \
	else \
		ghcid --command "cabal repl $(T)"; \
	fi

# ci

cic: formatc lintc haddockc

ci: lint format haddockc

# formatting

formatc:
	nix run github:tbidne/nix-hs-tools/0.8#nixpkgs-fmt -- --check ;\
	nix run github:tbidne/nix-hs-tools/0.8#cabal-fmt -- --check ;\
	nix run github:tbidne/nix-hs-tools/0.8#ormolu -- --mode check

format:
	nix run github:tbidne/nix-hs-tools/0.8#nixpkgs-fmt ;\
	nix run github:tbidne/nix-hs-tools/0.8#cabal-fmt -- --inplace ;\
	nix run github:tbidne/nix-hs-tools/0.8#ormolu -- --mode inplace

# linting

lint:
	nix run github:tbidne/nix-hs-tools/0.8#hlint -- --refact

lintc:
	nix run github:tbidne/nix-hs-tools/0.8#hlint

haddock:
	cabal haddock --haddock-hyperlink-source --haddock-quickjump ;\
	mkdir -p docs/ ;\
	find docs/ -type f | xargs -I % sh -c "rm -r %" ;\
	cp -r dist-newstyle/build/x86_64-linux/ghc-9.2.5/shrun-0.8/opt/doc/html/shrun/* docs/

haddockc:
	nix run github:tbidne/nix-hs-tools/0.8#haddock-cov -- \
	. \
	-m Shrun.Configuration.Env.Types 75 \
	-m Shrun.Data.FilePathDefault 70 \
	-m Shrun.Prelude 75 \
