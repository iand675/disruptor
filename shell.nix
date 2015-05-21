with (import <nixpkgs> {}).pkgs;
let pkg = haskellngPackages.callPackage
            ({ mkDerivation, atomic-primops, base, stdenv, vector, primitive, QuickCheck, criterion, deepseq, errors }:
             mkDerivation {
               pname = "disruptor";
               version = "0.1.0.0";
               src = ./.;
               buildDepends = [ atomic-primops base vector primitive QuickCheck criterion deepseq errors ];
               homepage = "https://github.com/iand675/disruptor";
               description = "Port of the LMAX disruptor pattern to Haskell";
               license = stdenv.lib.licenses.mit;
             }) {};
in
  pkg.env
