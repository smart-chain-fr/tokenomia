# Haskdogs

[`haskdogs`](https://hackage.haskell.org/package/haskdogs) is a tool to download and cache dependencies source code and generate a `tags` file to provide `jump to definition` features for your code editor (e.g. VSCodium). [`ctagsx`](https://github.com/jtanx/ctagsx) extension is used within VSCodium to read this `tags` file.

## Generating a `tags` file

The first time you enter the Nix shell, `haskdogs` will automatically be run (cf. [flake.nix](../../flake.nix) `shellHook`). However, the `tags` file will be incomplete.

`haskdogs` relies on `ghc-pkg` database to download package source code. `ghc-pkg` database only contains packages necessary to build GHC and not the ones from cabal projects (those are in `$HOME/.cabal` folder). So you will have to tell `haskdogs` to use the `cabal` database.

But before doing that, because some Cardano packages are on [`CHaP`](https://input-output-hk.github.io/cardano-haskell-packages/) and not on [Hackage](https://hackage.haskell.org/), you will have to add `CHaP` to `cabal` source repositories to tell `cabal` where to look for packages.

Add the following in your `$HOME/.cabal/config` file:
```
repository cardano-haskell-packages
  url: https://input-output-hk.github.io/cardano-haskell-packages
  secure: True
  root-keys:
    3e0cce471cf09815f930210f7827266fd09045445d65923e6d0238a6cd15126f
    443abb7fb497a134c343faf52f0b659bd7999bc06b7f63fa76dc99d631f9bea1
    a86a1f6ce86c449c46666bda44268677abf29b5b2d2eb5ec7af903ec2f117a82
    bcec67e8e99cabfa7764d75ad9b158d72bfacf70ca1d0ec8bc6b4406d1bf8413
    c00aae8461a256275598500ea0e187588c35a5d5d7454fb57eac18d9edb86a56
    d4a35cd3121aa00d18544bb0ac01c3e1691d618f462c46129271bccf39f7e8ee
```

Run this command at the root of Tokenomia:
```shell
[nix-shell:~/dev/tokenomia]$ haskdogs --ghc-pkg-args "--user-package-db=$HOME/.cabal/store/ghc-8.10.7/package.db/"
```

This should take ~ 30 min.

## Incremental update

If you want to update your `tags` file, run this command:
```shell
[nix-shell:~/dev/tokenomia]$ haskdogs --ghc-pkg-args "--user-package-db=$HOME/.cabal/store/ghc-8.10.7/package.db/" --hasktags-args "-x -c -a" | sort -u -o tags tags
```
