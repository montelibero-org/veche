#!/bin/bash
set -eux -o pipefail

mkdir -p release
stack --docker --local-bin-path=release install
