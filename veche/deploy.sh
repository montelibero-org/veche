#!/bin/bash
set -eux -o pipefail

stack --docker exec --package=yesod-bin -- yesod keter
scp veche.keter root@veche.cblp.su:/opt/keter/incoming/
