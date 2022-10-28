# Compiling on macOS M1

# Prerequisites
- [brew](https://brew.sh)

Install ghcup and llvm@12

```sh
$ brew install ghcup
$ brew install llvm@12
```

Setup environment to use llvm we installed earlier

```sh
export C_INCLUDE_PATH="`xcrun --show-sdk-path`/usr/include/ffi"
export CPPFLAGS="-I/opt/homebrew/opt/llvm@12/include"
export LDFLAGS="-L/opt/homebrew/opt/llvm@12/lib -Wl,-rpath,/opt/homebrew/opt/llvm@12/lib"

export PATH="$PATH:/opt/homebrew/opt/llvm@12/bin"
```

Install Haskell toolchain compatible with ghc 9.0.2

```sh
$ ghcup install ghc 9.0.2
$ ghcup set ghc 9.0.2
$ ghcup install stack recommended
$ ghcup set stack recommended
$ ghcup install cabal recommended
$ ghcup set cabal recommended
$ ghcup install hls 1.7.0.0
$ ghcup set hls 1.7.0.0
```

Setup stack to use only system libraries. Add this lines to ~/.stack/config.yml
```
install-ghc: false
system-ghc: true
```

Setup ghc to use llvm@12. Edit ~/.ghcup/ghc/9.0.2/lib/ghc-9.0.2/lib/settings, find line `,("C compiler command", "gcc")` and substitute gcc with clang.

# Compiling
```sh
$ stack build
```
