{ compiler ? "default" }:

let
  pkgs = import <nixpkgs> { };

in
  { dsl = pkgs.haskellPackages.callPackage ./default.nix { inherit compiler; };
  }
