{ pkgs ? import <nixpkgs> {} }:
let drv = pkgs.haskellPackages.callCabal2nix "boules2d" ./. {};
in if pkgs.lib.inNixShell then drv.env else drv

