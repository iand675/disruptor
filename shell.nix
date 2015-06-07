with (import <nixpkgs> {}).pkgs;
let pkg = haskellngPackages.callPackage
            ({ mkDerivation, atomic-primops, base, errors, ghc-prim, primitive
             , QuickCheck, stdenv, tasty, tasty-quickcheck, vector
             }:
             mkDerivation {
               pname = "disruptor";
               version = "0.1.0.0";
               src = ./.;
               buildDepends = [
                 atomic-primops base errors ghc-prim primitive vector
               ];
               testDepends = [
                 base primitive QuickCheck tasty tasty-quickcheck vector
               ];
               homepage = "https://github.com/iand675/disruptor";
               description = "Port of the LMAX disruptor pattern to Haskel";
               license = stdenv.lib.licenses.mit;
             }) {};
in
  pkg.env
