
<img src="./tokenomia-logo.png" width="500"  />

Tokenomia aims to simplify the use of Native Tokens and Smart Contracts above the Cardano Platform for the needs of [Smart Chain](https://smart-chain.fr/en/) Developments.

## What you can do on (Testnet/Mainnet)
 
	1-  [Wallet]  - List Registered Wallets
	2-  [Wallet]  - Display a specific wallets
	3-  [Wallet]  - Create a new Wallet
	4-  [Wallet]  - Derive Child Adresses
	5-  [Wallet]  - Create a unique collateral for transfer
	6-  [Wallet]  - Remove an existing Wallet
	7-  [Wallet]  - Restore Wallets from your 24 words seed phrase (Shelley Wallet)
	8-  [Token]   - Mint with CLAP type policy (Fix Total Supply | one-time Minting and open Burning Policy)
	9-  [Token]   - Burn Tokens with CLAP type policy
	10- [Token]   - Transfer Tokens
	11- [Ada]     - Transfer ADAs
	12- [Vesting] - Vest Funds
	13- [Vesting] - Retrieve Funds
	14- [Node]    - Status
	15- [ICO]     - Funds Validation Dry Run
	16- [ICO]     - Funds Validation Run
	17- [ICO]     - Funds Exchange Dry Run
	18- [ICO]     - Funds Exchange Run
	19- [ICO]     - Funds Simulation (Dispatch on child addresses ADAs)



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


