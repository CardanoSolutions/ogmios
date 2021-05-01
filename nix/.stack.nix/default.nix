{
  extras = hackage:
    {
      packages = {
        "async-timer" = (((hackage.async-timer)."0.2.0.0").revisions).default;
        "base16-bytestring" = (((hackage.base16-bytestring)."1.0.1.0").revisions).default;
        "bech32" = (((hackage.bech32)."1.1.0").revisions).default;
        "bech32-th" = (((hackage.bech32-th)."1.0.2").revisions).default;
        "binary" = (((hackage.binary)."0.8.7.0").revisions).default;
        "Cabal" = (((hackage.Cabal)."3.2.1.0").revisions).default;
        "canonical-json" = (((hackage.canonical-json)."0.6.0.0").revisions).default;
        "containers" = (((hackage.containers)."0.5.11.0").revisions).default;
        "dns" = (((hackage.dns)."3.0.4").revisions).default;
        "gray-code" = (((hackage.gray-code)."0.3.1").revisions).default;
        "libsystemd-journal" = (((hackage.libsystemd-journal)."1.4.4").revisions).default;
        "markov-chain-usage-model" = (((hackage.markov-chain-usage-model)."0.0.0").revisions).default;
        "micro-recursion-schemes" = (((hackage.micro-recursion-schemes)."5.0.2.2").revisions).default;
        "moo" = (((hackage.moo)."1.2").revisions).default;
        "network" = (((hackage.network)."3.1.2.1").revisions).default;
        "nothunks" = (((hackage.nothunks)."0.1.2").revisions).default;
        "parsec" = (((hackage.parsec)."3.1.14.0").revisions).default;
        "partial-order" = (((hackage.partial-order)."0.2.0.0").revisions).default;
        "quickcheck-state-machine" = (((hackage.quickcheck-state-machine)."0.7.0").revisions).default;
        "regex-posix-clib" = (((hackage.regex-posix-clib)."2.7").revisions).default;
        "statistics-linreg" = (((hackage.statistics-linreg)."0.3").revisions).default;
        "streaming-binary" = (((hackage.streaming-binary)."0.2.2.0").revisions).default;
        "text" = (((hackage.text)."1.2.4.0").revisions).default;
        "transformers-except" = (((hackage.transformers-except)."0.1.1").revisions).default;
        "Unique" = (((hackage.Unique)."0.4.7.6").revisions).default;
        "Win32" = (((hackage.Win32)."2.6.2.0").revisions).default;
        ogmios = ./ogmios.nix;
        cardano-client = ./cardano-client.nix;
        fast-bech32 = ./fast-bech32.nix;
        git-th = ./git-th.nix;
        hspec-json-schema = ./hspec-json-schema.nix;
        json-wsp = ./json-wsp.nix;
        json-via-show = ./json-via-show.nix;
        cardano-binary = ./cardano-binary.nix;
        cardano-binary-test = ./cardano-binary-test.nix;
        cardano-crypto-class = ./cardano-crypto-class.nix;
        cardano-crypto-tests = ./cardano-crypto-tests.nix;
        cardano-crypto-praos = ./cardano-crypto-praos.nix;
        cardano-slotting = ./cardano-slotting.nix;
        cardano-crypto = ./cardano-crypto.nix;
        byron-spec-chain = ./byron-spec-chain.nix;
        cardano-crypto-wrapper = ./cardano-crypto-wrapper.nix;
        cardano-crypto-test = ./cardano-crypto-test.nix;
        byron-spec-ledger = ./byron-spec-ledger.nix;
        cardano-ledger = ./cardano-ledger.nix;
        cardano-ledger-test = ./cardano-ledger-test.nix;
        small-steps = ./small-steps.nix;
        small-steps-test = ./small-steps-test.nix;
        cardano-ledger-shelley-ma = ./cardano-ledger-shelley-ma.nix;
        cardano-ledger-shelley-ma-test = ./cardano-ledger-shelley-ma-test.nix;
        shelley-spec-non-integral = ./shelley-spec-non-integral.nix;
        shelley-spec-ledger = ./shelley-spec-ledger.nix;
        shelley-spec-ledger-test = ./shelley-spec-ledger-test.nix;
        goblins = ./goblins.nix;
        io-sim = ./io-sim.nix;
        io-sim-classes = ./io-sim-classes.nix;
        network-mux = ./network-mux.nix;
        ouroboros-consensus = ./ouroboros-consensus.nix;
        ouroboros-consensus-test = ./ouroboros-consensus-test.nix;
        ouroboros-consensus-byron = ./ouroboros-consensus-byron.nix;
        ouroboros-consensus-byronspec = ./ouroboros-consensus-byronspec.nix;
        ouroboros-consensus-byron-test = ./ouroboros-consensus-byron-test.nix;
        ouroboros-consensus-shelley = ./ouroboros-consensus-shelley.nix;
        ouroboros-consensus-shelley-test = ./ouroboros-consensus-shelley-test.nix;
        ouroboros-consensus-cardano = ./ouroboros-consensus-cardano.nix;
        ouroboros-consensus-cardano-test = ./ouroboros-consensus-cardano-test.nix;
        ouroboros-network = ./ouroboros-network.nix;
        ouroboros-network-framework = ./ouroboros-network-framework.nix;
        typed-protocols = ./typed-protocols.nix;
        typed-protocols-examples = ./typed-protocols-examples.nix;
        Win32-network = ./Win32-network.nix;
        cardano-api = ./cardano-api.nix;
        cardano-config = ./cardano-config.nix;
        cardano-prelude = ./cardano-prelude.nix;
        cardano-prelude-test = ./cardano-prelude-test.nix;
        contra-tracer = ./contra-tracer.nix;
        iohk-monitoring = ./iohk-monitoring.nix;
        lobemo-backend-aggregation = ./lobemo-backend-aggregation.nix;
        lobemo-backend-ekg = ./lobemo-backend-ekg.nix;
        lobemo-backend-monitoring = ./lobemo-backend-monitoring.nix;
        lobemo-backend-trace-forwarder = ./lobemo-backend-trace-forwarder.nix;
        lobemo-scribe-systemd = ./lobemo-scribe-systemd.nix;
        tracer-transformers = ./tracer-transformers.nix;
        hjsonpointer = ./hjsonpointer.nix;
        hjsonschema = ./hjsonschema.nix;
        wai-routes = ./wai-routes.nix;
        };
      compiler.version = "8.10.4";
      compiler.nix-name = "ghc8104";
      };
  resolver = "lts-17.6";
  modules = [
    ({ lib, ... }:
      {
        packages = {
          "cardano-crypto-praos" = {
            flags = { "external-libsodium-vrf" = lib.mkOverride 900 false; };
            };
          };
        })
    {
      packages = {
        "$locals" = { ghcOptions = [ "-ddump-to-file -ddump-hi" ]; };
        };
      }
    ];
  compiler = "ghc-8.10.4";
  }