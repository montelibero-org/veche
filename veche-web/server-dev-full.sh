#!/bin/bash
set -eux -o pipefail

./server.sh config/dev-settings.yml "$@"
