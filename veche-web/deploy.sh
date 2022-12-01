#!/bin/bash
set -eux -o pipefail

stack --docker --docker-image=fpco/stack-build:lts-19   \
    exec --package=yesod-bin --                         \
    yesod keter

scp veche.keter root@veche.montelibero.org:/opt/keter/incoming/
