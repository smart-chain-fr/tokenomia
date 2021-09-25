# :wrench: SETUP.md

Here is the Setup guide to help you download the depencies in order to build the project.

>  Note: this guide are only avaible for Unix users.

## Table of content
* [Cabal](./SETUP.md#cabal)
* [Nix](./SETUP.md#nix)
* [Setup vsCode plugins](./SETUP.md#setup-vscode-plugins-(optional))

### Cabal
```sh
sudo apt-get install cabal
```
And update it :
```sh
cabal update
```

### Nix
```sh
curl https://nixos.org/releases/nix/nix-2.2.2/install | sh
```
> If your installation fails, please try the [following instructions](https://nix-tutorial.gitlabpages.inria.fr/nix-tutorial/installation.html)

The following lines should appear :
```sh
Installation finished!  To ensure that the necessary environment
variables are set, either log in again, or type

  . /home/$(whoami)/.nix-profile/etc/profile.d/nix.sh

in your shell.
```
Please run the `  . /home/$(whoami)/.nix-profile/etc/profile.d/nix.sh` line in order to load nix environment variables.

[More about Nix commands.](https://nix-tutorial.gitlabpages.inria.fr/nix-tutorial/getting-started.html)


### cardano-cli
* Download the <a href="https://github.com/input-output-hk/cardano-node/tree/master/cardano-cli">Cardano project</a> by doing :
	`git clone https://github.com/input-output-hk/cardano-node/tree/master/cardano-cli`
* Go in the cardano-cli folder : `cd cardano-node/cardano-cli`
* Run : `cabal build && cabal install`


### cardano-addresses
* Download the <a href="https://github.com/input-output-hk/cardano-addresses/releases">latest</a> archive version of cardano-addresses binary
* Extract the archive
eg : ```sh
     tar -xvzf cardano-addresses-3.6.0-linux64.tar.gz --one-top-level
     ```
* Copy the binary on cabal binaries folder alongside cardano-cli
```sh
cp cardano-addresses-3.6.0-linux64/bin/cardano-address ~/.cabal/bin
```
* Make the binary executable
```sh
chmod +x ~/.cabal/bin/cardano-address
```

### Setup vscode plugins (optional)
If you want to see the code of this project on VsCode, please make sure you have the following plugins installed :

* Haskell, with the extension id: haskell.haskell
* Haskell syntax Highlighting, with the extension id : justusadam.language-haskell
* haskell-linter, with the extension id : hoovercj.haskell-linter

If you have any trouble with indexing the files, please download this extension : jtanx.ctagsx,
and run : 

```bash
cabal install haskdogs hasktags
```
Then on the `tokenomia` source folder, run : 
```bash
haskdogs
```

Go back to [README.md](./README.md)
