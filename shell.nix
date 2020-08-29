{ nixpkgs ? import <nixpkgs> {} }:
let
  inherit (nixpkgs) pkgs;
  inherit (pkgs) haskellPackages;
  inherit (pkgs) nodePackages;
  
  project = import ./release.nix;
in
pkgs.stdenv.mkDerivation {
  name = "shell";
  buildInputs = project.env.nativeBuildInputs ++ [
    pkgs.cabal2nix
    haskellPackages.cabal-install
    haskellPackages.ghcid
    haskellPackages.hlint
    haskellPackages.ormolu
    pkgs.nodejs-12_x
    pkgs.nodePackages.prettier
    pkgs.nodePackages.eslint
    (pkgs.yarn.override { nodejs = pkgs.nodejs-12_x; })
  ];
}
