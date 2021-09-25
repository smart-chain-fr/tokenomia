![](./tokenomia-logo.png)

## Table of content
* [Setup](./README.md#setup)
* [Build](./README.md#build)
* [Run the project](./README.md#run-the-project)
* [How to use it](./README.md#how-to-use-it)

## Setup

Please follow the instructions in the [SETUP.md](./SETUP.md) file.

## Build
Go to the Tokenomia source folder and run :
```sh
nix-shell
```
It will read each instruction within the `shell.nix` file and execute it.

> :bulb: Slow building ?

If you have a slow building, maybe your nix cache isn't setup properly. To do so:
* On non-NixOS, edit `/etc/nix/nix.conf` and add the following lines :
```bash
substituters        = https://hydra.iohk.io https://iohk.cachix.org 	https://cache.nixos.org/
trusted-public-keys = hydra.iohk.io:f/Ea+s+dFdN+3Y/G+FDgSq+a5NEWhJGzdjvKNGv0/EQ= iohk.cachix.org-1:DpRUyj7h7V830dp/i6Nti+NEO2/nhblbov/8MW7Rqoo= cache.nixos.org-1:6NCHdD59X431o0gWypbMrAURkbJ16ZPMQFGspcDShjY=</code>
```

* On NixOS, set the following NixOS options:
```bash
nix = {
	binaryCaches = [ "https://hydra.iohk.io" "https://iohk.cachix.org" ];
      	binaryCachePublicKeys = [ "hydra.iohk.io:f/Ea+s+dFdN+3Y/G+FDgSq+a5NEWhJGzdjvKNGv0/EQ=" "iohk.cachix.org-1:DpRUyj7h7V830dp/i6Nti+NEO2/nhblbov/8MW7Rqoo=" ];
}
```

## Run the project

Then, to run the project :

```shell
cabal run tokenomia:exe:tokenomia-cli
```
> :bulb: `tokenomia` is the name of the project, `exe` is here to mention that you want to run an executable, and `tokenomia-cli` is the name of this one.
   You can find the name of the executable on the `tokenomia.cabal`.

> Fails ?

If you have the following traceback :
```haskell
src/Tokenomia/Adapter/Cardano/CardanoCLI.hs:1:1: error:
	Exception when trying to run compile-time code:
	Attempted to load 'cardano-cli', but it is not executable
	CallStack (from HasCallStack):
		error, called at src/Shh/Internal.hs:801:20 in shh-0.7.1.4-5zfKxJG0cmAE4nnSSaQsoW:Shh.Internal
		Code: load SearchPath ["cat", "echo", "mkdir", "md5sum", ....]
		  |
		1 | {-# LANGUAGE LambdaCase #-}
		  | ^
		cabal: Failed to build tokenomia-0.1.0.0 (which is required by
		exe:tokenomia-cli from tokenomia-0.1.0.0).
```

If you have already installed cardano-cli be sure that you have reloaded your path.
eg ```sh
   source ~/.bashrc
   ```
or ```sh
   export PATH="$HOME/.cabal/bin:$PATH"
   ```

The same goes for cardano-address.


## How to use it

Command | Description
---------- | ---------
`WalletList` | List registered wallets
`WalletAdd` | Add a wallet
`WalletRemove` | Remove a wallet (TODO)
`TokenMint` | Mint a token
`TokenBurn` | Burn a token (TODO)
`Transfer` | Transfer a token
`ReceiveByFaucet` | Retrieve testnet ADAs

## Contributors

| [<img src="https://github.com/nhenin.png?size=85" width=85><br><sub>Nicolas Henin</sub>](https://github.com/nhenin) | [<img src="https://github.com/NaadiQmmr.png?size=85" width=85><br><sub>Adina Cazalens</sub>](https://github.com/NaadiQmmr) | [<img src="https://github.com/augucharles.png?size=85" width=85><br><sub>Charles Augu</sub>](https://github.com/augucharles) |
| :---: | :---: | :---: |
