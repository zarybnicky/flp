{ pkgs ? import <nixpkgs> {} }:
pkgs.haskellPackages.extend (pkgs.haskell.lib.packageSourceOverrides {
  braun-heap = ./braun-heap;
  turing = ./turing;
})
