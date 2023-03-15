#!/usr/bin/env bash

network=$1

die() {
  echo >&2 "$@"
  exit 1
}

case $network in
  "mainnet")
    ;;
  "preprod")
    ;;
  "preview")
    ;;
  *)
    die "Wrong network."
    ;;
esac

network_path="$TOKENOMIA/cardano-node/$network"

mkdir -p "$network_path/logs"

wget -nc -q -P "$network_path" "https://book.world.dev.cardano.org/environments/$network/config.json"
wget -nc -q -P "$network_path" "https://book.world.dev.cardano.org/environments/$network/topology.json"
wget -nc -q -P "$network_path" "https://book.world.dev.cardano.org/environments/$network/byron-genesis.json"
wget -nc -q -P "$network_path" "https://book.world.dev.cardano.org/environments/$network/shelley-genesis.json"
wget -nc -q -P "$network_path" "https://book.world.dev.cardano.org/environments/$network/alonzo-genesis.json"

tmpfile=$(mktemp config.json.XXX)
jq --arg path "$network_path/logs/node0.json" \
  '.defaultScribes|=[["FileSK", $path]] | .setupScribes|=[{"scKind":"FileSK","scFormat":"ScJson","scName":$path,"scRotation":null}]' \
  "$network_path/config.json" > "$tmpfile" \
  && mv "$tmpfile" "$network_path/config.json"

wget -nc -q -P "$TOKENOMIA/cardano-node" https://raw.githubusercontent.com/cardano-community/guild-operators/master/scripts/cnode-helper-scripts/gLiveView.sh
wget -nc -q -P "$TOKENOMIA/cardano-node" https://raw.githubusercontent.com/cardano-community/guild-operators/2176da445a4028c93094a7fbb7c5831663462ea5/scripts/cnode-helper-scripts/env

cardano-node run \
  --config "$network_path/config.json" \
  --topology "$network_path/topology.json" \
  --database-path "$network_path/db" \
  --socket-path "$TOKENOMIA/cardano-node/node.sock" \
  --port 6000 > /dev/null 2>&1 &