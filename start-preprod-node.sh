#!/usr/bin/env bash

mkdir -p $TOKENOMIA/cardano-node/preprod
mkdir -p $TOKENOMIA/cardano-node/preprod/logs

wget -nc -q -P $TOKENOMIA/cardano-node/preprod https://book.world.dev.cardano.org/environments/preprod/config.json
wget -nc -q -P $TOKENOMIA/cardano-node/preprod https://book.world.dev.cardano.org/environments/preprod/topology.json
wget -nc -q -P $TOKENOMIA/cardano-node/preprod https://book.world.dev.cardano.org/environments/preprod/byron-genesis.json
wget -nc -q -P $TOKENOMIA/cardano-node/preprod https://book.world.dev.cardano.org/environments/preprod/shelley-genesis.json
wget -nc -q -P $TOKENOMIA/cardano-node/preprod https://book.world.dev.cardano.org/environments/preprod/alonzo-genesis.json

echo $(jq --arg path "$TOKENOMIA/cardano-node/preprod/logs/node0.json" \
    '.defaultScribes|=[["FileSK", $path]] | .setupScribes|=[{"scKind":"FileSK","scFormat":"ScJson","scName":$path,"scRotation":null}]' \
    $TOKENOMIA/cardano-node/preprod/config.json) > $TOKENOMIA/cardano-node/preprod/config.json

wget -nc -q -P $TOKENOMIA/cardano-node https://raw.githubusercontent.com/cardano-community/guild-operators/master/scripts/cnode-helper-scripts/gLiveView.sh
wget -nc -q -P $TOKENOMIA/cardano-node https://raw.githubusercontent.com/cardano-community/guild-operators/2176da445a4028c93094a7fbb7c5831663462ea5/scripts/cnode-helper-scripts/env

(cardano-node run \
    --config $TOKENOMIA/cardano-node/preprod/config.json \
    --topology $TOKENOMIA/cardano-node/preprod/topology.json \
    --database-path $TOKENOMIA/cardano-node/preprod/db \
    --socket-path $TOKENOMIA/cardano-node/node.sock \
    --port 6000) > /dev/null 2>&1 &