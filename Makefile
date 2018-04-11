install:
	yarn install

dev:
	bsb -clean-world
	yarn start

test:
	jest --clearCache
	./scripts/start_bitcoind.sh
	yarn test --runInBand

bsb:
	bsb -clean-world
	bsb -make-world -w
