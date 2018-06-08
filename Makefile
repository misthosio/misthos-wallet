web-dev:
	./node_modules/.bin/next -p 3001
web-build:
	./node_modules/.bin/next build
web-export: web-build
	./node_modules/.bin/next export -o web-dist

dev:
	./node_modules/.bin/webpack-dev-server --mode development --open

install:
	yarn install

test-unit:
	./node_modules/.bin/jest --clearCache
	NODE_ENV=test ./node_modules/.bin/jest --env=jsdom --testPathIgnorePatterns "/integration/" --watch

test:
	./node_modules/.bin/jest --clearCache
	./scripts/start_bitcoind.sh
	NODE_ENV=test ./node_modules/.bin/jest --env=jsdom --watch

bsb:
	node_modules/.bin/bsb -clean-world
	node_modules/.bin/bsb -make-world -w

build:
	./scripts/build.sh

bsb-once:
	node_modules/.bin/bsb -make-world
ci:
	node_modules/.bin/jest --clearCache
	./scripts/start_bitcoind.sh
	NODE_ENV=test CI=true ./node_modules/.bin/jest --env=jsdom --runInBand
	./scripts/stop_bitcoind.sh

.PHONY: ci build
