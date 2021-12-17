{ src
, pkgs
, plutus
, doCoverage ? false
, deferPluginErrors ? true
, ...
}:

let
  plutusPkgs = plutus.pkgs;
in
pkgs.haskell-nix.cabalProject {
  inherit src;

  name = "ogmios";

  # cabalProjectFileName = "cabal.project";

  # Plutus uses a patched GHC. And so shall we.
  compiler-nix-name = "ghc810420210212";

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


  modules = [
    {
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
      };
    }
  ];

}
