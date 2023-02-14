# Tokenomia setup

## Instantiate the node: ./testdata.json
### Docker
```
 docker run --rm \
  -e NETWORK=testnet \
  -v "$PWD"/.node/socket:/ipc \
  -v "$PWD"/.node/data:/data \
  inputoutput/cardano-node:1.34.0
```

After the node has synced (confirm by comparing slot to most recent Tx on cardanoscan), open a new terminal and run the following command to give the user permission to write in order to set an environment variable:
```
sudo chown $(whoami) "$PWD"/.node/socket/node.socket
```

### Environment
We need to set 2 env variables:
```
export CARDANO_NODE_SOCKET_PATH="$PWD"/.node/socket/node.socket
```
Then either of the follow depending on your network:
```
export BLOCKFROST_TOKEN_TESTNET_PATH=/path/to/testnet/API/key
export BLOCKFROST_TOKEN_MAINNET_PATH=/path/to/mainnet/API/key
```
### Shell
Run `nix-shell`

To run tokenomia, use:

`cabal run tokenomia:exe:tokenomia-cli`

To open a shell to interact with the node:

```
docker run -it \
--entrypoint bash \
-v "$PWD"/.node/socket:/ipc \
-v "$PWD"/.node/data:/data \
-e CARDANO_NODE_SOCKET_PATH=/ipc/node.socket \
inputoutput/cardano-node
```

Test the CLI has connected to your node
```
cardano-cli query tip --testnet-magic 1
```
*Note, change `--testnet-magic 1` to `--mainnet` or whatever network you're running on*