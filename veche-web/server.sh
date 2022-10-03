#!/bin/bash
set -eux -o pipefail

. ~/.config/veche.env
export VECHE_TELEGRAM_TOKEN
stack run veche -- "$@"
