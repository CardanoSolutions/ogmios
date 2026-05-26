{ inputs, ... }: {
  perSystem = { shellFor, pkgs, ... }: {
    devShells.default = shellFor {
      packages = p: [ p.ogmios ];

      nativeBuildInputs = [
        pkgs.jq
        pkgs.gh
      ];

      tools = {
        cabal = "latest";
        ghcid = "latest";
        haskell-language-server = {
          src = inputs.haskellNix.inputs."hls-2.10";
          configureArgs = "--disable-benchmarks --disable-tests";
        };
      };

      shellHook = ''
        export LANG="en_US.UTF-8"
        git submodule update --init --recursive
      '';

      withHoogle = true;
    };
  };
}
