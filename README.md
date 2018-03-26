## Setup Local Dev
### Deps
```
git clone https://github.com/misthosio/misthos
cd misthos
brew install node
❯ node --version
v9.7.1
brew install yarn
npm -g install bs-platform
❯ yarn --version
1.5.1
brew install bitcoin
> Bitcoin Core RPC client version v0.16.0.0-g4b4d7eb
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
