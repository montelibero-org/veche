## Haskell Setup

1. If you haven't already, [install Stack](https://haskell-lang.org/get-started)
	* On POSIX systems, this is usually `curl -sSL https://get.haskellstack.org/ | sh`
2. Install the `yesod` command line tool: `stack install yesod-bin --install-ghc`
3. Build libraries: `stack build`

If you have trouble, refer to the [Yesod Quickstart guide](https://www.yesodweb.com/page/quickstart) for additional detail.

## Development

Start a development server with:

```sh
$ ./veche-web/server-dev-front.sh
```

As your code changes, your site will be automatically recompiled and redeployed to localhost.

Run whole application without auto-reload:
```sh
$ ./veche-web/server-dev-full.sh
```

Or web frontend only.

## Tests

```sh
stack test
```

## Update Letsencrypt certificates

```sh
# service keter stop
# certbot certonly --standalone
> veche.montelibero.org
# service keter start
```
