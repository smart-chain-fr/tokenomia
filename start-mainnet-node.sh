#!/usr/bin/env bash

mkdir -p $TOKENOMIA/cardano-node/mainnet
mkdir -p $TOKENOMIA/cardano-node/mainnet/logs

wget -nc -q -P $TOKENOMIA/cardano-node/mainnet https://book.world.dev.cardano.org/environments/mainnet/config.json
wget -nc -q -P $TOKENOMIA/cardano-node/mainnet https://book.world.dev.cardano.org/environments/mainnet/topology.json
wget -nc -q -P $TOKENOMIA/cardano-node/mainnet https://book.world.dev.cardano.org/environments/mainnet/byron-genesis.json
wget -nc -q -P $TOKENOMIA/cardano-node/mainnet https://book.world.dev.cardano.org/environments/mainnet/shelley-genesis.json
wget -nc -q -P $TOKENOMIA/cardano-node/mainnet https://book.world.dev.cardano.org/environments/mainnet/alonzo-genesis.json

echo $(jq --arg path "$TOKENOMIA/cardano-node/mainnet/logs/node0.json" \
    '.defaultScribes|=[["FileSK", $path]] | .setupScribes|=[{"scKind":"FileSK","scFormat":"ScJson","scName":$path,"scRotation":null}]' \
    $TOKENOMIA/cardano-node/mainnet/config.json) > $TOKENOMIA/cardano-node/mainnet/config.json

wget -nc -q -P $TOKENOMIA/cardano-node https://raw.githubusercontent.com/cardano-community/guild-operators/master/scripts/cnode-helper-scripts/gLiveView.sh
wget -nc -q -P $TOKENOMIA/cardano-node https://raw.githubusercontent.com/cardano-community/guild-operators/2176da445a4028c93094a7fbb7c5831663462ea5/scripts/cnode-helper-scripts/env

(cardano-node run \
    --config $TOKENOMIA/cardano-node/mainnet/config.json \
    --topology $TOKENOMIA/cardano-node/mainnet/topology.json \
    --database-path $TOKENOMIA/cardano-node/mainnet/db \
    --socket-path $TOKENOMIA/cardano-node/node.sock \
    --port 6000) > /dev/null 2>&1 &