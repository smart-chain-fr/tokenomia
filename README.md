
<img src="./tokenomia-logo.png" width="500"  />

Tokenomia aims to simplify the use of Native Tokens and Smart Contracts above the Cardano Platform for the needs of [Smart Chain](https://smart-chain.fr/en/) Developments.

## What you can do on (Testnet/Mainnet)

|Shelley Wallet Management 
---------- | 
List registered wallets
Create a wallet
Remove an existing Wallet
Restore Wallets from your 24 words seed phrase (e.g from Deadalus Wallet)
Create a collateral 

|Native Token
---------- | 
Mint and Burn (Fix Total Supply, one-time Minting and open Burning Policy) 
Transfer tokens

|ADA
---------- | 
Transfer ADA

|Vesting
---------- | 
Vest Funds (using the official plutus-use-case vesting validator)
Retrieve Funds


## Technical Approach 

Regarding our personal objectives at Smart Chain,  we are aiming to release services on Cardano Mainnet soon and so we can't wait for the PAB to come...
From this particular context, we are exactly doing what Nigel Hemsley is describing during the [Mid Month Development Update - October](https://www.youtube.com/watch?v=XzdTyV5Jejc&t=2s&ab_channel=IOHK) which was a part of the Capstone Hackathon Subject for EMEA Europe Region.

We are using a mix of different approaches : 
- The normal way where we directly use cardano-api and cardano-cli
- The shorter way where we are calling the command shells via haskell code (using shh)

After a few iterations now, we start seeing the patterns we were looking for by bypassing the Contract Monad (Also explained by Nigel Hemsley in the update video) : 
- Preconditions/Assertions
- Coin Selection
- Tx Building

A good simple example could be the creation of collateral for a wallet ([here](https://github.com/smart-chain-fr/tokenomia/blob/main/src/Tokenomia/Wallet/Collateral.hs)) 

### Some Challenges we have faced 

- Validator error Opacity :  When you have a validator failing, this is the kind information you will get :  
```shell 
Command failed: transaction build  Error: The following scripts have execution failures:
the script for transaction input 1 (in the order of the TxIds) failed with:
The Plutus script evaluation failed: An error has occurred:  User error:
The provided Plutus code called 'error'.
```
 
**Solution** : Since the validators are deterministics, it's sufficient to retrieve only the information given to the validator into cardano-cli.  
So, We have traced into cardano-ledger-specs this way to be able to improve the transparency ([here](https://github.com/nhenin/cardano-ledger-specs/commit/2329c83e44f2f83ef29a97466820eb2b4ec58d97)). 

- The Vesting :  without the chain-index, we had to keep locally some information (e.g : the params embedded into the vesting validators)

### Tokenomia "DB"

`tokenomia-cli` is saving information at `~/.tokenomia-cli/` into the following folders 
- `keys`  
    - You'll find all the information about the wallets you are using in tokenomia
    - e.g : `Zeus` and `Athena` are 2 "wallets" used in the Testenet generated into the `testnet/keys` folders.
- `monetary-policies` : you'll find the monetary policy plutus scripts of the Native Tokens you mint via Tokenomia 
- `transactions` folder is used for building transactions 
- `validators` : you'll find the vesting validators and a record of all the vesting you'll have done via Tokenomia (required for retrieving the funds vested)
- `tmp` : mainnly used for recording datums and redeemers data

here is a simple example of the structure : 

```shell
-- testnet
    |-- keys
    |   |-- Athena
    |   |   |-- mnemonics.txt
    |   |   |-- payment-signing.skey
    |   |   |-- payment-signing.xsk
    |   |   |-- payment-verification.vkey
    |   |   |-- payment-verification.xvk
    |   |   |-- payment.addr
    |   |   |-- public-key.hash
    |   |   |-- root.xsk
    |   |   `-- stake.xvk
    |   |-- Jupiter
    |   |   |-- mnemonics.txt
    |   |   |-- payment-signing.skey
    |   |   |-- payment-signing.xsk
    |   |   |-- payment-verification.vkey
    |   |   |-- payment-verification.xvk
    |   |   |-- payment.addr
    |   |   |-- public-key.hash
    |   |   |-- root.xsk
    |   |   `-- stake.xvk
    |   `-- Zeus
    |       |-- mnemonics.txt
    |       |-- payment-signing.skey
    |       |-- payment-signing.xsk
    |       |-- payment-verification.vkey
    |       |-- payment-verification.xvk
    |       |-- payment.addr
    |       |-- public-key.hash
    |       |-- root.xsk
    |       `-- stake.xvk
    |-- monetary-policies
    |   `-- d14fee196d96d37f3ea149fac4b3ce326f724b706b70d67b45ef76de.plutus
    |-- parameters
    |   `-- parameters-testnet.json
    |-- tmp
    |   |-- 1457377353943883957.txt
    |   |-- 1785756855668231913.txt
    |   |-- 1968830820112713122.txt
    |   |-- 2569789401013764603.txt
    |   |-- 4122642902711131427.txt
    |   |-- 498428157451304989.txt
    |   |-- 5017803380174220559.txt
    |   |-- 5724876749071085794.txt
    |   |-- 5867046558793242961.txt
    |   |-- 7047104766165311267.txt
    |   `-- 7569565383380314696.txt
    |-- transactions
    |   |-- 19460e5550bfad26c17a557b32d421ba.raw
    |   |-- 19460e5550bfad26c17a557b32d421ba.signed
    |   |-- 19a6ec39ea303e12e1b6e43421ef6a19.raw
    |   |-- 19a6ec39ea303e12e1b6e43421ef6a19.signed
    |   |-- 24b74da7af102de6307ddff796498cf1.raw
    `-- validators
        |-- 2c47033d5be3d6659c4b419f75098d32919df881d9aa8e9ef721caeb.plutus
        `-- vesting.index
-- mainnet ...        
```

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


