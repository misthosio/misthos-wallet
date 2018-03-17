dev:
	yarn start
test:
	jest --clearCache
	./__tests__/helpers/start_bitcoind.sh
	yarn test --runInBand
