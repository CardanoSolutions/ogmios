{ src
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

  };
in
pkgSet.haskell-nix.cabalProject project
