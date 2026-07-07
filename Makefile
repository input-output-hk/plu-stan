# plu-stan convenience Makefile
#
# The primary purpose of this Makefile is to install / check the native system
# libraries (libsodium, secp256k1, blst) required to build the plutus/cardano
# test fixtures. See scripts/install-system-deps.sh for details.

.DEFAULT_GOAL := help

# Install prefix used by scripts/install-system-deps.sh (override with PREFIX=...).
PREFIX ?= /usr/local

.PHONY: help system-deps system-deps-check build test

help: ## Show this help
	@echo "plu-stan make targets:"
	@echo ""
	@echo "  system-deps        Install native libs (libsodium, secp256k1, blst)"
	@echo "  system-deps-check  Check the native libs are present + print versions"
	@echo "  build              cabal build all"
	@echo "  test               cabal test all"
	@echo "  help               Show this help (default)"

system-deps: ## Install native system libraries needed by the fixtures
	bash scripts/install-system-deps.sh

system-deps-check: ## Verify native libraries are installed (VRF-fork aware); nonzero exit if any missing
	# Delegates to the installer's --check mode so libsodium is validated for the
	# VRF (crypto_vrf_*) API, not just plain `pkg-config --exists libsodium`.
	bash scripts/install-system-deps.sh --check

# Belt-and-suspenders: export PKG_CONFIG_PATH / LD_LIBRARY_PATH inline so these
# targets work even before `make system-deps` symlinks the .pc files onto the
# default pkg-config path (e.g. right after building the libs into $(PREFIX)).
build: ## cabal build all
	PKG_CONFIG_PATH="$(PREFIX)/lib/pkgconfig:$$PKG_CONFIG_PATH" \
	LD_LIBRARY_PATH="$(PREFIX)/lib:$$LD_LIBRARY_PATH" \
	cabal build all

test: ## cabal test all
	PKG_CONFIG_PATH="$(PREFIX)/lib/pkgconfig:$$PKG_CONFIG_PATH" \
	LD_LIBRARY_PATH="$(PREFIX)/lib:$$LD_LIBRARY_PATH" \
	cabal test all
