{ pkgs ? import <nixpkgs> {} }:
pkgs.haskell.packages.ghc844.extend (pkgs.haskell.lib.packageSourceOverrides {
  braun-heap-singletons = ./braun-heap-singletons;
  braun-heap-typelits = ./braun-heap-typelits;
  braun-heap-cli = ./braun-heap-cli;
  turing = ./turing-haskell;
})
