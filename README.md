## Setup Local Dev
### Deps
```
git clone https://github.com/misthosio/misthos
cd misthos
brew install node
brew install yarn
brew install bitcoin
make install
```

### Test
```
make test
```
Will start bitcoind and continuously run tests on changed files.

### Build

```
make bsb
```
Will continuously rebuild all project files (if you only want to check the compilation without running the tests)

### Serve
```
make dev
```
Will serve the app to `localhost:3000`

```
make web-dev
```
Will serve the website to `localhost:3001`
