{ pkgs ? import <nixpkgs> {} }:
pkgs.haskellPackages.extend (pkgs.haskell.lib.packageSourceOverrides {
  binary-heap = ./binary-heap;
})
