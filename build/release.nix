let

    fetchNixpkgs = import ./fetchNixpkgs.nix;
    nixpkgs = fetchNixpkgs (builtins.fromJSON (builtins.readFile ./nixpkgs-snapshot.json));
    pkgs = import nixpkgs { } ;

in
    pkgs.haskellPackages.callPackage ./default.nix { }
