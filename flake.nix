{
  description = "tokenomia";
  nixConfig.bash-prompt = "\\[\\e[0m\\][\\[\\e[0;2m\\]nix-develop \\[\\e[0;1m\\]tokenomia \\[\\e[0;93m\\]\\w\\[\\e[0m\\]]\\[\\e[0m\\]$ \\[\\e[0m\\]";

  inputs = {
    haskell-nix.url = "github:mlabs-haskell/haskell.nix";

    nixpkgs.follows = "haskell-nix/nixpkgs-unstable";

    iohk-nix.url = "github:input-output-hk/iohk-nix";
    iohk-nix.flake = false; # Bad Nix code

    flake-compat = {
      url = "github:edolstra/flake-compat";
      flake = false;
    };

    # all inputs below here are for pinning with haskell.nix
    blockfrost-haskell = {
      url =
        "github:smart-chain-fr/blockfrost-haskell/175cf5da3c173f89dbeec06c64d136be215d2439";
      flake = false;
    };
    cardano-addresses = {
      url =
        "github:input-output-hk/cardano-addresses/d2f86caa085402a953920c6714a0de6a50b655ec";
      flake = false;
    };
    cardano-base = {
      url =
        "github:input-output-hk/cardano-base/4ea7e2d927c9a7f78ddc69738409a5827ab66b98";
      flake = false;
    };
    cardano-config = {
      url =
        "github:input-output-hk/cardano-config/1646e9167fab36c0bff82317743b96efa2d3adaa";
      flake = false;
    };
    cardano-crypto = {
      url =
        "github:input-output-hk/cardano-crypto/07397f0e50da97eaa0575d93bee7ac4b2b2576ec";
      flake = false;
    };
    cardano-ledger = {
      url =
        "github:input-output-hk/cardano-ledger/1a9ec4ae9e0b09d54e49b2a40c4ead37edadcce5";
      flake = false;
    };
    cardano-ledger-specs = {
      url =
        "github:input-output-hk/cardano-ledger-specs/bf008ce028751cae9fb0b53c3bef20f07c06e333";
      flake = false;
    };
    cardano-node = {
      url =
        "github:input-output-hk/cardano-node/b6ca519f97a0e795611a63174687e6bb70c9f752";
      flake = false;
    };
    cardano-prelude = {
      url =
        "github:input-output-hk/cardano-prelude/fd773f7a58412131512b9f694ab95653ac430852";
      flake = false;
    };
    cardano-wallet = {
      url =
        "github:input-output-hk/cardano-wallet/ae7569293e94241ef6829139ec02bd91abd069df";
      flake = false;
    };
    # We don't actually need this. Removing this might make caching worse?
    flat = {
      url =
        "github:input-output-hk/flat/ee59880f47ab835dbd73bea0847dab7869fc20d8";
      flake = false;
    };
    ekg-forward = {
      url =
        "github:input-output-hk/ekg-forward/297cd9db5074339a2fb2e5ae7d0780debb670c63";
      flake = false;
    };
    goblins = {
      url =
        "github:input-output-hk/goblins/cde90a2b27f79187ca8310b6549331e59595e7ba";
      flake = false;
    };
    iohk-monitoring-framework = {
      url =
        "github:input-output-hk/iohk-monitoring-framework/46f994e216a1f8b36fe4669b47b2a7011b0e153c";
      flake = false;
    };
    optparse-applicative = {
      url =
        "github:input-output-hk/optparse-applicative/7497a29cb998721a9068d5725d49461f2bba0e7a";
      flake = false;
    };
    ouroboros-network = {
      url =
        "github:input-output-hk/ouroboros-network/1f4973f36f689d6da75b5d351fb124d66ef1057d";
      flake = false;
    };
    # Patched plutus for metadata support. We need this until `plutus-apps` will update `plutus`,
    # rewrite of `plutus-ledger-constraints`, and possibly some bpi adjustments afterwards.
    # tldr: Dependency hell
    plutus = {
      url =
        "github:mlabs-haskell/plutus/3f089ccf0ca746b399c99afe51e063b0640af547";
      flake = false;
    };
    plutus-apps = {
      url =
        "github:mlabs-haskell/plutus-apps/f6abf06f8eefb1f7d382f4d372b96cd39c11f83a";
      flake = false;
    };
    purescript-bridge = {
      url =
        "github:input-output-hk/purescript-bridge/6a92d7853ea514be8b70bab5e72077bf5a510596";
      flake = false;
    };
    servant-purescript = {
      url =
        "github:input-output-hk/servant-purescript/a0c7c7e37c95564061247461aef4be505a853538";
      flake = false;
    };
    Win32-network = {
      url =
        "github:input-output-hk/Win32-network/3825d3abf75f83f406c1f7161883c438dac7277d";
      flake = false;
    };
  };

  outputs = { self, nixpkgs, haskell-nix, iohk-nix, ... }@inputs:
    let
      defaultSystems = [ "x86_64-linux" "x86_64-darwin" ];

      perSystem = nixpkgs.lib.genAttrs defaultSystems;

      nixpkgsFor = system:
        import nixpkgs {
          overlays = [ haskell-nix.overlay (import "${iohk-nix}/overlays/crypto") ];
          inherit (haskell-nix) config;
          inherit system;
        };
      nixpkgsFor' = system: import nixpkgs { inherit system; };

      cabalProjectLocal = ''
        allow-newer:
           -- Copied from plutus-core
           size-based:template-haskell
           , ouroboros-consensus-byron:formatting
           , beam-core:aeson
           , beam-sqlite:aeson
           , beam-sqlite:dlist
           , beam-migrate:aeson

        -- Copied from plutus-core
        constraints:
          -- big breaking change here, inline-r doens't have an upper bound
          singletons < 3.0
          -- bizarre issue: in earlier versions they define their own 'GEq', in newer
          -- ones they reuse the one from 'some', but there isn't e.g. a proper version
          -- constraint from dependent-sum-template (which is the library we actually use).
          , dependent-sum > 0.6.2.0
      '';

      haskellModules = [
        ({ pkgs, ... }:
          {
            packages = {
              marlowe.flags.defer-plugin-errors = true;
              plutus-use-cases.flags.defer-plugin-errors = true;
              plutus-ledger.flags.defer-plugin-errors = true;
              plutus-contract.flags.defer-plugin-errors = true;
              cardano-crypto-praos.components.library.pkgconfig = pkgs.lib.mkForce [ [ pkgs.libsodium-vrf ] ];
              cardano-crypto-class.components.library.pkgconfig = pkgs.lib.mkForce [ [ pkgs.libsodium-vrf ] ];
              cardano-wallet-core.components.library.build-tools = [
                pkgs.buildPackages.buildPackages.gitMinimal
              ];
              cardano-config.components.library.build-tools = [
                pkgs.buildPackages.buildPackages.gitMinimal
              ];
            };
          }
        )
      ];

      extraSources = [
        {
          src = inputs.blockfrost-haskell;
          subdirs = [
            "blockfrost-client"
            "blockfrost-api"
            "blockfrost-client-core"
            "blockfrost-pretty"
          ];
        }
        {
          src = inputs.cardano-addresses;
          subdirs = [ "core" "command-line" ];
        }
        {
          src = inputs.cardano-base;
          subdirs = [
            "base-deriving-via"
            "binary"
            "binary/test"
            "cardano-crypto-class"
            "cardano-crypto-praos"
            "cardano-crypto-tests"
            "measures"
            "orphans-deriving-via"
            "slotting"
            "strict-containers"
          ];
        }
        {
          src = inputs.cardano-crypto;
          subdirs = [ "." ];
        }
        {
          src = inputs.cardano-ledger;
          subdirs = [
            "eras/alonzo/impl"
            "eras/byron/chain/executable-spec"
            "eras/byron/crypto"
            "eras/byron/crypto/test"
            "eras/byron/ledger/executable-spec"
            "eras/byron/ledger/impl"
            "eras/byron/ledger/impl/test"
            "eras/shelley/impl"
            "eras/shelley-ma/impl"
            "eras/shelley/test-suite"
            "libs/cardano-data"
            "libs/cardano-ledger-core"
            "libs/cardano-ledger-pretty"
            "libs/cardano-protocol-tpraos"
            "libs/compact-map"
            "libs/non-integral"
            "libs/set-algebra"
            "libs/small-steps"
            "libs/small-steps-test"
          ];
        }
        {
          src = inputs.cardano-ledger-specs;
          subdirs = [
            "byron/ledger/impl"
            "cardano-ledger-core"
            "cardano-protocol-tpraos"
            "eras/alonzo/impl"
            "eras/byron/chain/executable-spec"
            "eras/byron/crypto"
            "eras/byron/crypto/test"
            "eras/byron/ledger/executable-spec"
            "eras/byron/ledger/impl/test"
            "eras/shelley/impl"
            "eras/shelley-ma/impl"
            "libs/non-integral"
            "libs/small-steps"
            "semantics/small-steps-test"
          ];
        }
        {
          src = inputs.cardano-node;
          subdirs = [
            "cardano-api"
            "cardano-node"
            "cardano-cli"
            "trace-resources"
            "trace-forward"
            "trace-dispatcher"
          ];
        }
        {
          src = inputs.cardano-config;
          subdirs = [ "." ];
        }
        {
          src = inputs.cardano-prelude;
          subdirs = [ "cardano-prelude" "cardano-prelude-test" ];
        }
        {
          src = inputs.cardano-wallet;
          subdirs = [
            "lib/text-class"
            "lib/strict-non-empty-containers"
            "lib/core"
            "lib/test-utils"
            "lib/numeric"
          ];
        }
        {
          src = inputs.ekg-forward;
          subdirs = [ "." ];
        }
        {
          src = inputs.flat;
          subdirs = [ "." ];
        }
        {
          src = inputs.goblins;
          subdirs = [ "." ];
        }
        {
          src = inputs.iohk-monitoring-framework;
          subdirs = [
            "iohk-monitoring"
            "tracer-transformers"
            "contra-tracer"
            "plugins/backend-aggregation"
            "plugins/backend-ekg"
            "plugins/backend-monitoring"
            "plugins/backend-trace-forwarder"
            "plugins/scribe-systemd"
          ];
        }
        {
          src = inputs.optparse-applicative;
          subdirs = [ "." ];
        }
        {
          src = inputs.ouroboros-network;
          subdirs = [
            "monoidal-synchronisation"
            "typed-protocols"
            "typed-protocols-cborg"
            "typed-protocols-examples"
            "ouroboros-network"
            "ouroboros-network-testing"
            "ouroboros-network-framework"
            "ouroboros-consensus"
            "ouroboros-consensus-byron"
            "ouroboros-consensus-cardano"
            "ouroboros-consensus-shelley"
            "io-sim"
            "io-classes"
            "network-mux"
            "ntp-client"
          ];
        }
        {
          src = inputs.plutus;
          subdirs = [
            "plutus-core"
            "plutus-ledger-api"
            "plutus-tx"
            "plutus-tx-plugin"
            "prettyprinter-configurable"
            "stubs/plutus-ghc-stub"
            "word-array"
          ];
        }
        {
          src = inputs.plutus-apps;
          subdirs = [
            "doc"
            "freer-extras"
            "playground-common"
            "plutus-chain-index"
            "plutus-contract"
            "plutus-ledger"
            "plutus-pab"
            "plutus-playground-server"
            "plutus-use-cases"
            "quickcheck-dynamic"
            "web-ghc"
          ];
        }
        {
          src = inputs.purescript-bridge;
          subdirs = [ "." ];
        }
        {
          src = inputs.servant-purescript;
          subdirs = [ "." ];
        }
        {
          src = inputs.Win32-network;
          subdirs = [ "." ];
        }
      ];

      projectFor = system:
        let
          pkgs = nixpkgsFor system;
          pkgs' = nixpkgsFor' system;
          cardanoInputs = [
            project.hsPkgs.cardano-cli.components.exes.cardano-cli
            project.hsPkgs.cardano-node.components.exes.cardano-node
            project.hsPkgs.cardano-addresses-cli.components.exes.cardano-address
          ];
          project = pkgs.haskell-nix.cabalProject' {
            src = ./.;
            inherit cabalProjectLocal extraSources;
            name = "tokenomia";
            compiler-nix-name = "ghc8107";
            shell = {
              additional = ps: [
                ps.plutus-pab
                ps.cardano-cli
                ps.blockfrost-pretty
                ps.blockfrost-client
                ps.blockfrost-api
                ps.blockfrost-client-core
              ];
              withHoogle = true;
              tools.haskell-language-server = { };
              exactDeps = true;
              nativeBuildInputs = with pkgs'; [
                cabal-install
                haskellPackages.cabal-fmt
                haskellPackages.implicit-hie
                haskellPackages.fourmolu
                haskellPackages.apply-refact
                hlint
                jq
                websocat
                fd
                nixpkgs-fmt
                curl
              ] ++ cardanoInputs;
              shellHook = '' export PATH="'' + builtins.concatStringsSep ":" (map (inp: inp.outPath + "/bin") cardanoInputs) + '':$PATH" '';
            };
            modules = haskellModules ++ [
              ({ config, pkgs, ... }: {
                packages.tokenomia.components.library.build-tools = [
                  config.hsPkgs.cardano-cli.components.exes.cardano-cli
                  config.hsPkgs.cardano-node.components.exes.cardano-node
                  config.hsPkgs.cardano-addresses-cli.components.exes.cardano-address
                  pkgs.curl
                ];
              })
            ];
          };
        in
        project;

      formatCheckFor = system:
        let
          pkgs = nixpkgsFor system;
        in
        pkgs.runCommand "format-check"
          { nativeBuildInputs = [ self.devShell.${system}.nativeBuildInputs ]; } ''
          cd ${self}
          export LC_CTYPE=C.UTF-8
          export LC_ALL=C.UTF-8
          export LANG=C.UTF-8
          export IN_NIX_SHELL='pure'
          make format_check cabalfmt_check nixpkgsfmt_check lint
          mkdir $out
        '';

    in
    {
      inherit cabalProjectLocal extraSources haskellModules;

      project = perSystem projectFor;
      flake = perSystem (system: (projectFor system).flake { });

      defaultPackage = perSystem (system:
        let lib = "tokenomia:lib:tokenomia";
        in self.flake.${system}.packages.${lib});

      packages = perSystem (system: self.flake.${system}.packages);

      apps = perSystem (system: self.flake.${system}.apps);

      devShell = perSystem (system: self.flake.${system}.devShell);

      # This will build all of the project's executables and the tests
      check = perSystem (system:
        (nixpkgsFor system).runCommand "combined-check"
          {
            nativeBuildInputs = builtins.attrValues self.checks.${system}
              ++ builtins.attrValues self.flake.${system}.packages
              ++ [ self.devShell.${system}.inputDerivation ];
          } "touch $out");
      # NOTE `nix flake check` will not work at the moment due to use of
      # IFD in haskell.nix
      checks = perSystem (system: self.flake.${system}.checks // {
        formatCheck = formatCheckFor system;
      });
    };
}
