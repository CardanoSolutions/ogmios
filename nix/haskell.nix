{ src
, inputs
, pkgs
, plutus
, static
, doCoverage ? false
, deferPluginErrors ? true
, ...
}:

let
  plutusPkgs = plutus.pkgs;

  musl64 = pkgs.pkgsCross.musl64;

  pkgSet = if static then musl64 else pkgs;

  project = {
    inherit src;

    name = "ogmios";

    compiler-nix-name = "ghc8107";

    shell = {
      # Make sure to keep this list updated after upgrading git dependencies!
      additional = ps: with ps; [
        cardano-api
        cardano-binary
        cardano-crypto-class
        cardano-crypto-praos
        cardano-crypto-tests
        cardano-slotting
        strict-containers
        cardano-prelude
        contra-tracer
        iohk-monitoring
        io-classes
        io-sim
        ouroboros-consensus
        ouroboros-consensus-byron
        ouroboros-consensus-byronspec
        ouroboros-consensus-shelley
        ouroboros-consensus-cardano
        ouroboros-consensus-cardano-test
        ouroboros-network
        ouroboros-network-framework
        typed-protocols
        typed-protocols-cborg
        flat
        hjsonpointer
        hjsonschema
        wai-routes
      ];

      withHoogle = true;

      tools.cabal = "latest";

      exactDeps = true;

      nativeBuildInputs = with pkgs;
        [
          # Haskell Tools
          entr
          ghcid
          git

          # Use plutus for these packages for now, the versions from haskell.nix
          # nixpkgs are too new and require builds
          plutusPkgs.haskellPackages.fourmolu

          plutus.plutus.haskell-language-server
          plutus.plutus.hlint
          jq
          nixfmt

          # hls doesn't support preprocessors yet so this has to exist in PATH
          haskellPackages.record-dot-preprocessor

          # Graphviz Diagrams for documentation
          graphviz
          pkg-config
          plutusPkgs.libsodium-vrf
        ] ++ (
          lib.optionals (!stdenv.isDarwin) [
            rPackages.plotly
            R
            systemdMinimal
          ]
        );
    };

    modules = [{
      packages = {
        eventful-sql-common.doHaddock = false;
        eventful-sql-common.ghcOptions = [
          ''
            -XDerivingStrategies -XStandaloneDeriving -XUndecidableInstances
                    -XDataKinds -XFlexibleInstances -XMultiParamTypeClasses''
        ];

        cardano-crypto-praos.components.library.pkgconfig =
          plutusPkgs.lib.mkForce [ [ plutusPkgs.libsodium-vrf ] ];
        cardano-crypto-class.components.library.pkgconfig =
          plutusPkgs.lib.mkForce [ [ plutusPkgs.libsodium-vrf ] ];
      } // pkgs.lib.mkIf static {
        ogmios.components.exes.ogmios.configureFlags = pkgs.lib.optionals
          musl64.stdenv.hostPlatform.isMusl [
          "--disable-executable-dynamic"
          "--disable-shared"
          "--ghc-option=-optl=-pthread"
          "--ghc-option=-optl=-static"
          "--ghc-option=-optl=-L${musl64.gmp6.override { withStatic = true; }}/lib"
          "--ghc-option=-optl=-L${musl64.zlib.override { static = true; }}/lib"
        ];
      };
    }];

    extraSources = [
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
        subdirs = [
          "."
        ];
      }
      {
        src = inputs.cardano-ledger;
        subdirs = [
          "eras/alonzo/impl"
          "eras/alonzo/test-suite"
          "eras/byron/chain/executable-spec"
          "eras/byron/crypto"
          "eras/byron/crypto/test"
          "eras/byron/ledger/executable-spec"
          "eras/byron/ledger/impl"
          "eras/byron/ledger/impl/test"
          "eras/shelley/impl"
          "eras/shelley/test-suite"
          "eras/shelley-ma/impl"
          "eras/shelley-ma/test-suite"
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
        src = inputs.cardano-node;
        subdirs = [
          "cardano-api"
        ];
      }
      {
        src = inputs.cardano-prelude;
        subdirs = [
          "cardano-prelude"
          "cardano-prelude-test"
        ];
      }
      {
        src = inputs.flat;
        subdirs = [
          "."
        ];
      }
      {
        src = inputs.goblins;
        subdirs = [
          "."
        ];
      }
      {
        src = inputs.hedgehog-extras;
        subdirs = [
          "."
        ];
      }
      {
        src = inputs.hjsonpointer;
        subdirs = [
          "."
        ];
      }
      {
        src = inputs.hjsonschema;
        subdirs = [
          "."
        ];
      }
      {
        src = inputs.iohk-monitoring-framework;
        subdirs = [
          "contra-tracer"
          "iohk-monitoring"
          "plugins/backend-aggregation"
          "plugins/backend-ekg"
          "plugins/backend-monitoring"
          "plugins/backend-trace-forwarder"
          "plugins/scribe-systemd"
          "tracer-transformers"
        ];
      }
      {
        src = inputs.ouroboros-network;
        subdirs = [
          "io-classes"
          "io-sim"
          "monoidal-synchronisation"
          "network-mux"
          "ouroboros-consensus"
          "ouroboros-consensus-test"
          "ouroboros-consensus-byron"
          "ouroboros-consensus-byronspec"
          "ouroboros-consensus-byron-test"
          "ouroboros-consensus-cardano"
          "ouroboros-consensus-protocol"
          "ouroboros-consensus-shelley"
          "ouroboros-consensus-shelley-test"
          "ouroboros-consensus-cardano"
          "ouroboros-consensus-cardano-test"
          "ouroboros-consensus-mock"
          "ouroboros-network"
          "ouroboros-network-framework"
          "ouroboros-network-testing"
          "strict-stm"
          "typed-protocols"
          "typed-protocols-cborg"
          "typed-protocols-examples"
        ];
      }
      {
        src = inputs.plutus;
        subdirs = [
          "freer-extras"
          "plutus-core"
          "plutus-ledger-api"
          "plutus-tx"
          "prettyprinter-configurable"
          "stubs/plutus-ghc-stub"
          "word-array"
        ];
      }
      {
        src = inputs.wai-routes;
        subdirs = [
          "."
        ];
      }
      {
        src = inputs.Win32-network;
        subdirs = [
          "."
        ];
      }
    ];
  };
in
pkgSet.haskell-nix.cabalProject project
