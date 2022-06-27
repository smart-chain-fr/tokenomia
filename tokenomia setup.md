 # Tokenomia setup

 ### to instantiate the node: ./testdata.json

 docker run --rm \
  -e NETWORK=testnet \
  -v "$PWD"/.node/socket:/ipc \
  -v "$PWD"/.node/data:/data \
  inputoutput/cardano-node:1.34.0

 after the node has synced (confirm by comparing slot to most recent Tx on cardanoscan), open a new terminal and run the following command to give the user permission to write in order to set an environment variable:

  sudo chown $(whoami) "$PWD"/.node/socket/node.socket

Then we need to set 2 env variables:

 export CARDANO_NODE_SOCKET_PATH="$PWD"/.node/socket/node.socket
 export BLOCKFROST_TOKEN_TESTNET_PATH=/path/to/testnet/API/key
 export BLOCKFROST_TOKEN_MAINNET_PATH=/path/to/mainnet/API/key

 Then run nix-shell

 To run tokenomia, use:

 cabal run tokenomia:exe:tokenomia-cli

 To open a shell to interact with the node:

  docker run -it \
  --entrypoint bash \
  -v "$PWD"/.node/socket:/ipc \
  -v "$PWD"/.node/data:/data \
  -e CARDANO_NODE_SOCKET_PATH=/ipc/node.socket \
  inputoutput/cardano-node

  and can use the shell with commands like:

  cardano-cli query tip --testnet-magic 1097911063
