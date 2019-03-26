{ pkgs ? import <nixpkgs> {} }:
pkgs.haskell.packages.ghc844.extend (pkgs.haskell.lib.packageSourceOverrides {
  braun-heap = ./braun-heap;
  turing = ./turing;
})
