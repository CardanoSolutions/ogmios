{ config, lib, withSystem, ... }: {
  flake.hydraJobs = lib.genAttrs config.systems (lib.flip withSystem (
    {
      config,
      pkgs,
      ...
    }: let
      required    = {inherit (config) packages checks;};
      nonRequired = {inherit (config) devShells;};
      jobs        = required // nonRequired;
    in
      jobs
      // {
        required = pkgs.releaseTools.aggregate {
          name = "required";
          constituents = lib.collect lib.isDerivation required;
        };
        nonrequired = pkgs.releaseTools.aggregate {
          name = "nonrequired";
          constituents = lib.collect lib.isDerivation nonRequired;
        };
      }
  ));
}
