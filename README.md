
<img src="./tokenomia-logo.png" width="500"  />

Tokenomia is a command-line tool for working with the [Cardano blockchain](https://cardano.org/). It aims to simplify the use of [Native Tokens](https://docs.cardano.org/native-tokens/learn) and [Plutus Contracts](https://docs.cardano.org/plutus/learn-about-plutus) and will serve as a light replacement for the [Plutus Application Backend (PAB)](https://github.com/input-output-hk/plutus-apps/tree/main/plutus-pab) during its development.

Tokenomia was first built for the [Cardashift](https://cardashift.com/) ICO to create Cardashift's $CLAP token and manage funds during the ICO (ADA reception and CLAP distribution).

[Cardashift](https://cardashift.com/) is a community-driven startup platform that raises funds, builds and accelerates startups that solve social and environmental problems.

## What you can do on (Testnet/Mainnet)

	1- [Wallet]  - List Registered Wallets
	2- [Wallet]  - Display wallet utxos
	3- [Wallet]  - Display wallet utxos within child address index range
	4- [Wallet]  - Create a new Wallet
	5- [Wallet]  - Derive Child Adresses
	6- [Wallet]  - Create a unique collateral for transfer
	7- [Wallet]  - Remove an existing Wallet
	8- [Wallet]  - Restore Wallets from your 24 words seed phrase (Shelley Wallet)
	9- [Token]   - Mint with CLAP type policy (Fix Total Supply | one-time Minting and open Burning Policy)
	10-[Token]   - Burn Tokens with CLAP type policy
	11-[Token]   - Transfer Tokens
	12-[Ada]     - Transfer ADAs
	13-[Vesting] - Verify Sendings
	14-[Vesting] - Generate Database and airdrop outputs
	15-[Node]    - Status
	16-[Node]    - Translate Slot To Time
	17-[Node]    - Translate Time To Slot

## Dependencies

Tokenomia needs a running and synchronized [`cardano-node`](https://github.com/input-output-hk/cardano-node) instance to interact with the Cardano blockchain.

Tokenomia relies on [`cardano-cli`](https://github.com/input-output-hk/cardano-node/tree/master/cardano-cli) to query the blockchain using a `cardano-node` socket.

Tokenomia also uses [`cardano-address`](https://github.com/input-output-hk/cardano-addresses) to manage its wallets.

Certain features require a [Blockfrost](https://blockfrost.io/) API key.

We are currently running on `cardano-node` `1.35.4`, with `cardano-cli` `1.35.4` and `cardano-address` `3.12.0`.

The project is built using GHC `8.10.7`.

## Architecture

Tokenomia was a fork of the now deprecated [plutus-starter](https://github.com/input-output-hk/plutus-starter) repository and relied on IOHK's "old style" repository architecture with a lot of github repositories dependency and build instructions depending on Nix.

IOHK is now migrating to a new architecture for [Cardano Haskell Packages (CHaP)](https://input-output-hk.github.io/cardano-haskell-packages/) to greatly reduce github repositories dependency. They are also standardizing their Nix approach using [Nix Flakes](https://nixos.wiki/wiki/Flakes). The reference implementation for this new architecture is [plutus-apps](https://github.com/input-output-hk/plutus-apps).

Following IOHK in their migration, we decided to do things differently:

Tokenomia dependencies follow [cardano-node](https://github.com/input-output-hk/cardano-node/blob/master/cabal.project) and [plutus-apps](https://github.com/input-output-hk/plutus-apps/blob/main/cabal.project). The [marlowe-cardano](https://github.com/input-output-hk/marlowe-cardano/blob/main/cabal.project) repository is the most up-to-date project with the same dependencies, so we took inspiration of its dependencies management (sources and versions) for Tokenomia. We use a [cabal.project.freeze](cabal.project.freeze) file to freeze Haskell dependencies for more reproductible builds.

IOHK's architecture largely depends on Nix, most of their Haskell repositories are "Nixified" using [haskell.nix](https://input-output-hk.github.io/haskell.nix/). As we are relatively new to Nix, we chose to decouple our Haskell project from the Nix infrastructure, this way it is possible for a user to run Tokenomia without Nix. However, we still want to benefit from Nix Flakes to build a reproductible development environment (with its [flake.lock](flake.lock) file) to ease our day to day work and quickly onboard new collaborators.

## Development Tasks (Only tested on Ubuntu)

### Environment Setup using Nix (recommended)

Using Nix is the easiest way to get started with Tokenomia. Our [flake.nix](flake.nix) file builds a development shell with all necessary tools included:
- GHC
- cabal-install
- Haskell Language Server
- VSCodium (the "free" version of VSCode) with some Haskell and Nix extensions installed
- `cardano-node`
- `cardano-cli`
- `cardano-address`

1) Install Nix by going to the Nix [download page](https://nixos.org/download.html).

2) Enable Flakes by following [this instructions](https://nixos.wiki/wiki/Flakes).

3) Enter the Nix shell:

```shell
nhenin@ubuntu ~/d/tokenomia (main)> nix develop
```

Note: If it is the first time you work with Nix or the Cardano tooling, setup will take a really long time (~ 8 hours) and use a bit of space (~ 30 Go). This is because there are a lot of dependencies and most of them are not cached and need to be built.

4) Connect to a `cardano-node`:
- You can launch a local instance using our helper script (see [local-cardano-node.md](doc/dev/local-cardano-node.md)).
- You can connect to an existing instance running on your machine by exporting the instance socket path.
```shell
nhenin@ubuntu ~/d/tokenomia (main)> export CARDANO_NODE_SOCKET_PATH=<SOME_PATH>/node.sock
```
- You can connect to a remote instance you have access to by using a tool like [socat](http://www.dest-unreach.org/socat/) (see [remote-cardano-node.md](doc/dev/remote-cardano-node.md)).


### Environement Setup "by hand" (not recommended)

1) Install GHC `8.10.7` and cabal-install (you can use the [ghcup installer](https://www.haskell.org/ghcup/)).
2) Install [`cardano-node`](https://github.com/input-output-hk/cardano-node), [`cardano-cli`](https://github.com/input-output-hk/cardano-node/tree/master/cardano-cli) and [`cardano-address`](https://github.com/input-output-hk/cardano-addresses). You can build them or download binaries for your distribution on the release pages.


### Run tokenomia-cli

Wait for your `cardano-node` instance to be synchronized with the current state of the blockchain.
Then, you can simply:
```shell
nhenin@ubuntu ~/d/tokenomia (main)> nix develop
...
[nix-shell:~/dev/tokenomia]$ cabal run tokenomia-cli
```
