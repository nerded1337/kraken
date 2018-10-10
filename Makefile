NUM_JOBS=1

shell.nix: kraken.cabal
	cabal2nix ./. --shell > $@

build: shell.nix
	nix-shell $^ --run "cabal new-build -j$(NUM_JOBS)"

.PHONY: build
