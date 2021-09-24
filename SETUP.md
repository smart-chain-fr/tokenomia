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
