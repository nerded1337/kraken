{ nixpkgs ? import <nixpkgs> {}, compiler ? "default", doBenchmark ? false }:

let

  inherit (nixpkgs) pkgs;

  f = { mkDerivation, aeson, aeson-casing, base, base64-bytestring
      , byteable, bytestring, cryptohash, data-default, either, envy
      , hashable, hspec, http-api-data, http-client-tls, microlens
      , QuickCheck, scientific, servant, servant-client, stdenv, text
      , time, transformers, unordered-containers, vector
      }:
      mkDerivation {
        pname = "kraken";
        version = "0.1.0.0";
        src = ./.;
        isLibrary = true;
        isExecutable = true;
        libraryHaskellDepends = [
          aeson aeson-casing base base64-bytestring byteable bytestring
          cryptohash data-default either envy hashable http-api-data
          http-client-tls microlens scientific servant servant-client text
          time transformers unordered-containers vector
        ];
        executableHaskellDepends = [
          base bytestring data-default text transformers
        ];
        testHaskellDepends = [ base hspec QuickCheck ];
        homepage = "http://github.com/cmahon/kraken";
        description = "Kraken exchange API binding";
        license = stdenv.lib.licenses.bsd3;
      };

  haskellPackages = if compiler == "default"
                       then pkgs.haskellPackages
                       else pkgs.haskell.packages.${compiler};

  variant = if doBenchmark then pkgs.haskell.lib.doBenchmark else pkgs.lib.id;

  drv = variant (haskellPackages.callPackage f {});

in

  if pkgs.lib.inNixShell then drv.env else drv
