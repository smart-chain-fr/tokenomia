{
  description = "Tokenomia";
  nixConfig.bash-prompt = "\\[\\e[0m\\][\\[\\e[0;2m\\]nix-develop \\[\\e[0;1m\\]tokenomia \\[\\e[0;93m\\]\\w\\[\\e[0m\\]\\[\\e[0;94m\\]$(__git_ps1)\\[\\e[0m\\]]\\[\\e[0m\\]$ \\[\\e[0m\\]";

  # inputs is an attribute set of all the dependencies of the flake
  inputs = {
    # latest packages list
    nixpkgs.url = "github:nixos/nixpkgs/nixpkgs-unstable";
    # tool that helps having standardized nix flake files
    flake-parts.url = "github:hercules-ci/flake-parts";
    libsodium = {
      url = "github:input-output-hk/libsodium/66f017f1";
      flake = false;
    };
    secp256k1 = {
      url = "github:bitcoin-core/secp256k1/ac83be33";
      flake = false;
    };
    cardano-addresses.url = "github:input-output-hk/cardano-addresses";
    cardano-node.url = "github:input-output-hk/cardano-node/1.35.4";
  };

############################################################################

  # outputs is a function of one argument that takes an attribute set
  # of all the realized inputs, and outputs another attribute set
  # i.e. uses the inputs to build some outputs (packages, apps, shells,..)

  outputs = { self, nixpkgs, flake-parts, libsodium, secp256k1, cardano-addresses, cardano-node }@inputs:

    # mkFlake is the main function of flake-parts to build a flake with standardized arguments
    flake-parts.lib.mkFlake { inherit self; inherit inputs; } {

      # list of systems to be built upon
      systems = nixpkgs.lib.systems.flakeExposed;

      # make a build for each system
      perSystem = { system, pkgs, lib, config, self', inputs', ... }: {

        packages = {
          # Cardano relies on custom version of libsodium and secp256k1 crytpographic libraries.
          # So we cannot get them from nixpkgs, we have to install them manually.
          # The build phases are explained in the cardano-node documentation:
          # https://developers.cardano.org/docs/get-started/installing-cardano-node/
          libsodium = import ./nix/libsodium.nix { inherit pkgs; inherit libsodium; };
          secp256k1 = import ./nix/secp256k1.nix { inherit pkgs; inherit secp256k1; };
          # Haskell toolchain (GHC, cabal, HLS)
          ghc = pkgs.haskell.compiler.ghc8107;
          cabal-install = pkgs.cabal-install;
          haskell-language-server = pkgs.haskell-language-server.override {
            supportedGhcVersions = [ "8107" ];
          };
          # code editor
          vscodium = import ./nix/vscodium.nix { inherit pkgs; };
        };

        # Here is the definition of the nix-shell we use for development
        # It comes with all necessary packages + other nice to have tools
        devShells.default =
          let
            # libs necessary to build the cardano packages
            libs = [
              self'.packages.libsodium
              pkgs.pkg-config
              self'.packages.secp256k1
              pkgs.zlib
              pkgs.lzma
              pkgs.postgresql
            ];
            # cardano tools binaries, used within tokenomia
            cardanoTools = [
              cardano-addresses.defaultPackage.${system}
              cardano-node.packages.${system}.cardano-cli
            ];
            # tools for Haskell development
            devTools = [
              self'.packages.ghc
              self'.packages.cabal-install
              self'.packages.haskell-language-server
              self'.packages.vscodium
              # necessary for vscodium integrated terminal
              pkgs.bashInteractive
              # haskdogs and hasktags download and tag dependencies source files
              # so we can enjoy "jump to definition" in deps
              pkgs.haskellPackages.hasktags
              pkgs.haskellPackages.haskdogs
            ];
            # other useful tools
            otherTools = [
              pkgs.jq
              pkgs.socat
            ];
          in
            pkgs.mkShell {
              buildInputs = libs ++ cardanoTools ++ devTools ++ otherTools;
              LD_LIBRARY_PATH = pkgs.lib.makeLibraryPath libs;
              shellHook = ''
                source <(curl -s https://raw.githubusercontent.com/git/git/master/contrib/completion/git-prompt.sh);
                export LANG=C.utf8;
                cabal update;
                ! test -f tags && haskdogs;
                codium .
              '';
            };
      };
    };

############################################################################################

  # nixConfig is an attribute set of values which reflect the values given to nix.conf.
  # This can extend the normal behavior of a user's nix experience by adding flake-specific
  # configuration, such as a binary cache.
  nixConfig = {
    extra-substituers = [
      "https://cache.iog.io"
      "https://cache.nixos.org"
      "https://iohk.cachix.org"
    ];
    extra-trusted-public-keys = [
      "hydra.iohk.io:f/Ea+s+dFdN+3Y/G+FDgSq+a5NEWhJGzdjvKNGv0/EQ="
      "iohk.cachix.org-1:DpRUyj7h7V830dp/i6Nti+NEO2/nhblbov/8MW7Rqoo="
      "cache.nixos.org-1:6NCHdD59X431o0gWypbMrAURkbJ16ZPMQFGspcDShjY="
    ];
    allow-import-from-derivation = "true";
  };
}
