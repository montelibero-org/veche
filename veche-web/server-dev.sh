#!/bin/bash
set -eux -o pipefail

stack exec --package=yesod-bin -- yesod devel --extra-stack-arg=--ghc-options=-O2
