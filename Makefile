dev:
	yarn start
test:
	jest --clearCache
	./__tests__/helpers/start_bitcoind.sh
	yarn test --runInBand

bsb:
	bsb -clean-world
	bsb -make-world -w
