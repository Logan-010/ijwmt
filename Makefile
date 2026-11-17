.PHONY: build
build: 
	cabal build exe:hyoh --project-file=cabal.project.release
	strip --strip-all $$(cabal list-bin exe:hyoh --project-file=cabal.project.release)

.PHONY: build-dev
build-dev: 
	cabal build exe:hyoh

.PHONY: run
run: build
	cabal run exe:hyoh --project-file=cabal.project.release

.PHONY: run-dev
run-dev: build-dev
	cabal run exe:hyoh

.PHONY: fmt
fmt:
	ormolu app/*.hs -i
	cabal-fmt *.cabal -i

.PHONY: lint
lint:
	hlint app/*.hs
