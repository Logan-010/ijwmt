.PHONY: build
build: 
	cabal build exe:ijwmt --project-file=cabal.project.release
	strip --strip-all $$(cabal list-bin exe:ijwmt --project-file=cabal.project.release)

.PHONY: build-dev
build-dev: 
	cabal build exe:ijwmt

.PHONY: run
run: build
	cabal run exe:ijwmt --project-file=cabal.project.release

.PHONY: run-dev
run-dev: build-dev
	cabal run exe:ijwmt

.PHONY: test
test:
	cabal test --test-show-details=direct

.PHONY: fmt
fmt:
	ormolu app/**/*.hs -i
	cabal-fmt *.cabal -i

.PHONY: lint
lint:
	hlint app/*.hs
