# Run a cardano-node instance on your machine

We provide a helper script to automatically start a node on your machine for the following networks:
- preprod: [start-preprod-node.sh](../../start-preprod-node.sh)
- mainnet: [start-mainnet-node.sh](../../start-mainnet-node.sh)

## Start the node

You just need to make sure the script you want to use is executable:
```shell
nhenin@ubuntu ~/d/tokenomia (main)> chmod 755 $TOKENOMIA/start-preprod-node.sh
```

And run it:
```shell
nhenin@ubuntu ~/d/tokenomia (main)> $TOKENOMIA/start-preprod-node.sh
```

This will create a `cardano-node` folder at the root of this github repository with all configuration files.
You will have to wait for the node to synchronize with the current state of the blockchain network. It is relatively fast for the preprod network (~30 min).

Note: Whenever you need the node, you can simply run the script, it will take less time as it will only fetch the last blocks and resume.

## Monitoring sync status

You can follow the sync progress using [gLiveView](https://cardano-community.github.io/guild-operators/Scripts/gliveview/):
```shell
nhenin@ubuntu ~/d/tokenomia (main)> chmod 755 $TOKENOMIA/cardano-node/gLiveView.sh
nhenin@ubuntu ~/d/tokenomia (main)> $TOKENOMIA/cardano-node/gLiveView.sh
```

## Stopping the node

Just kill the process:
```shell
nhenin@ubuntu ~/d/tokenomia (main)> pkill cardano-node
```
