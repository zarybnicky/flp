{ pkgs ? import <nixpkgs> {} }:
pkgs.haskell.packages.ghc844.extend (pkgs.haskell.lib.packageSourceOverrides {
  braun-heap = ./braun-heap;
  braun-heap-cli = ./braun-heap-cli;
  turing = ./turing-haskell;
})
