# Connect to a remote cardano-node instance using socat

`cardano-cli` talks to a `cardano-node` using a UNIX socket. `socat` can establish a tunnel between two machines to access a remote socket.

## On the remote instance

To forward a UNIX socket over TCP, you can use the following command, using `nohup` to put the process in background:
```shell
nhenin@ubuntu ~/d/tokenomia (main)> nohup socat TCP4-LISTEN:4000,fork UNIX-CONNECT:/opt/cardano/cnode/sockets/node0.socket 2>&1 &
```

Note: Change the port you want to expose and adapat the socket path to that of your machine.


## On the local machine

Enter the following command, adjusting the path where you want the socket to be created and specifying the IP of your remote machine and the port `socat` is listening on the other side:
```shell
nhenin@ubuntu ~/d/tokenomia (main)> socat UNIX-LISTEN:/home/nhenin/cardano/cardano-testnet-node.socket,mode=777,reuseaddr,fork TCP:53.152.20.151:4000 &
```

If you have `cardano-cli` installed on the local machine, you can test the connection.
First, export the cardano socket variable with the same path you specified in the previous command:
```shell
nhenin@ubuntu ~/d/tokenomia (main)> export CARDANO_NODE_SOCKET_PATH=<SOME_PATH>/node.sock
```
Then, try the following `cardano-cli` command:
```shell
nhenin@ubuntu ~/d/tokenomia (main)> cardano-cli query tip --testnet-magic 1
```

Note: `--testnet-magic 1` is for the preprod network, change accordingly (e.g. `--mainnet` for mainnet).
