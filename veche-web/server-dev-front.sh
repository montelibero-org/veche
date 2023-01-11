#!/bin/bash
set -eux -o pipefail

stack exec --package=yesod-bin -- yesod devel
