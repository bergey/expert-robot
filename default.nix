{ compiler ? "default", doBenchmark ? false }:

let

    fetchNixpkgs = import ./fetchNixpkgs.nix;
    nixpkgs = fetchNixpkgs (builtins.fromJSON (builtins.readFile ./nixpkgs-snapshot.json));
    pkgs = import nixpkgs { config = {
        allowUnfree = true;
        };
    };


  f = { mkDerivation, alex, array, base, bytestring, stdenv, text
      }:
      mkDerivation {
        pname = "expert-robot";
        version = "0.1.0.0";
        src = ./.;
        isLibrary = false;
        isExecutable = true;
        executableHaskellDepends = [ array base bytestring text ];
        executableToolDepends = [ alex ];
        homepage = "https://github.com/bergey/expert-robot#readme";
        license = stdenv.lib.licenses.bsd3;
      };

  haskellPackages = if compiler == "default"
                       then pkgs.haskellPackages
                       else pkgs.haskell.packages.${compiler};

  variant = if doBenchmark then pkgs.haskell.lib.doBenchmark else pkgs.lib.id;

  drv = variant (haskellPackages.callPackage f {});

in

  if pkgs.lib.inNixShell then drv.env else drv
