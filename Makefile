dev:
	node_modules/.bin/webpack-dev-server --mode development --open

install:
	yarn install

test-unit:
	node_modules/.bin/jest --clearCache
	yarn test --testPathIgnorePatterns "/integration/"

test:
	node_modules/.bin/jest --clearCache
	./scripts/start_bitcoind.sh
	yarn test

bsb:
	node_modules/.bin/bsb -clean-world
	node_modules/.bin/bsb -make-world -w

build:
	rm -rf dist/*
	NODE_ENV=production node_modules/.bin/webpack --mode production
	cp public/* dist/

bsb-once:
	node_modules/.bin/bsb -make-world
ci:
	node_modules/.bin/jest --clearCache
	./scripts/start_bitcoind.sh
	CI=true yarn test --runInBand
	./scripts/stop_bitcoind.sh

.PHONY: ci build
