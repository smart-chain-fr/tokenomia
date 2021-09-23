# :wrench: SETUP.md

Here is the Setup guide to help you download the depencies in order to build the project.

>  Note: this guide are only avaible for Unix users.

## Table of content
* [Cabal](./SETUP.md#cabal)
* [Nix](./SETUP.md#nix)

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

Go back to [README.md](./README.md)
