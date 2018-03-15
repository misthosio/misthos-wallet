dev:
	yarn start
test:
	./__tests__/helpers/start_bitcoind.sh
	yarn test
