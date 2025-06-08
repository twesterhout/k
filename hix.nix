inputs: { config, build, lib, pkgs, ... }:

let
  sourceFilter = root: with lib.fileset; toSource {
    inherit root;
    fileset = fileFilter (file: lib.any file.hasExt [ "cabal" "hs" "md" ]) root;
  };
  pname = "k";
  defaultGhc = inputs.nixpkgs.legacyPackages.${config.system}.haskellPackages.ghc;
  defaultGhcVersion = "ghc" + lib.replaceStrings [ "." ] [ "" ] defaultGhc.version;
in
{
  compiler = defaultGhcVersion;
  ghcVersions = [ defaultGhcVersion ];
  systems = builtins.attrNames inputs.nixpkgs.legacyPackages;
  cabal = {
    author = "twesterhout";
    build-type = "Simple";
    license = "BSD-3-Clause";
    license-file = "LICENSE.md";
    version = "0.1.0.0";
    meta = {
      maintainer = "Tom Westerhout <14264576+twesterhout@users.noreply.github.com>";
      homepage = "https://github.com/twesterhout/${pname}";
    };
    language = "GHC2021";
    default-extensions = [
      "DeriveAnyClass"
      "DeriveGeneric"
      "DerivingVia"
      "DuplicateRecordFields"
      "LambdaCase"
      "OverloadedRecordDot"
      "ScopedTypeVariables"
      "ViewPatterns"
    ];
    ghc-options = [
      "-Weverything"
      "-Wno-missing-exported-signatures"
      "-Wno-missing-kind-signatures"
      "-Wno-missing-local-signatures"
      "-Wno-missing-signatures"
      "-Wno-unsafe"
    ];
  };
  packages.${pname} = {
    src = sourceFilter ./.;
    library = {
      enable = true;
      source-dirs = "src";
      # modules = [ "Language.K" ];
      dependencies = [ "array" "bytestring" "containers" "directory" "parsec" "pretty" "text" ];
    };
    executable = {
      enable = true;
      source-dirs = "app";
      dependencies = [ "k" "text" ];
    };
  };
  envs.dev = {
    env.DIRENV_IN_ENVRC = "";
    setup-pre = /*bash*/ ''
      nix run .#gen-cabal
      nix run .#tags
    '';
    buildInputs = with config.pkgs; [
    ];
  };
}
