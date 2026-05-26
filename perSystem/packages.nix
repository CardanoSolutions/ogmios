{
  perSystem = { hsPkgs, ... }:
    let
      ogmios = hsPkgs.ogmios;
    in
    {
      packages.ogmios = ogmios.components.library;
      packages.ogmios-exe = ogmios.components.exes.ogmios;
      checks.ogmios-unit = ogmios.components.tests.unit;
    };
}
