
<img src="./tokenomia-logo.png" width="500"  />

Tokenomia is built for the Cardashift [ICO](https://ico.cardashift.com/), it aims to simplify the use of Native Tokens and Smart Contracts above the Cardano Platform.
[Cardashift](https://cardashift.com/) is a community-driven startup platform that raises funds, builds and accelerates startups that solve social and environmental problems.

## What you can do on (Testnet/Mainnet)
 
	1-  [Wallet]  - List Registered Wallets
	2-  [Wallet]  - Display a specific wallet
	3-  [Wallet]  - Create a new Wallet
	4-  [Wallet]  - Derive Child Adresses
	5-  [Wallet]  - Create a unique collateral for transfer
	6-  [Wallet]  - Remove an existing Wallet
	7-  [Wallet]  - Restore Wallets from your 24 words seed phrase (Shelley Wallet)
	8-  [Token]   - Mint with CLAP type policy (Fix Total Supply | one-time Minting and open Burning Policy)
	9-  [Token]   - Burn Tokens with CLAP type policy
	10- [Token]   - Transfer Tokens
	11- [Ada]     - Transfer ADAs
	12- [Node]    - Status



## Development Tasks (Only tested on Ubuntu)

### Environment Setup

- The same used in the  [plutus starter project](https://github.com/input-output-hk/plutus-starter).
- You need to install `cardano-cli` 1.30.1  as well, see [cardano-node project](https://github.com/input-output-hk/cardano-node)  

### Run tokenomia-cli

```shell
nhenin@ubuntu ~/d/tokenomia (main)> nix-shell 
...
[nix-shell:~/dev/tokenomia]$ cabal run tokenomia:exe:tokenomia-cli
```


