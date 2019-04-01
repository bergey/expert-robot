let
    fetchNixpkgs = import ./fetchNixpkgs.nix;
    nixpkgs = fetchNixpkgs (builtins.fromJSON (builtins.readFile ./nixpkgs-snapshot.json));
    pkgs = import nixpkgs { } ;

in rec { 
    expert-robot = pkgs.haskellPackages.callPackage ./default.nix { };
    env = pkgs.lib.overrideDerivation expert-robot.env (old: {
            buildInputs = old.buildInputs ++ [ pkgs.cabal-install ];
        });
}
