install:
	yarn install

dev:
	bsb -clean-world
	yarn start

test:
	node_modules/.bin/jest --clearCache
	./scripts/start_bitcoind.sh
	yarn test --runInBand

bsb:
	node_modules/.bin/bsb -clean-world
	node_modules/.bin/bsb -make-world -w

build:
	rm -rf ./node_modules/uri-js/dist/esnext
	yarn build

ci:
	node_modules/.bin/jest --clearCache
	./scripts/start_bitcoind.sh
	CI=true yarn test --runInBand
	./scripts/stop_bitcoind.sh

.PHONY: ci
