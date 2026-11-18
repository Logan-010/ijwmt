{
  description = "Ijwmt flake";

  inputs = {
    nixpkgs.url = "github:NixOS/nixpkgs/nixos-unstable";
  };

  outputs =
    { self, nixpkgs }:
    let
      systems = [
        "x86_64-linux"
        "aarch64-linux"
        "x86_64-darwin"
        "aarch64-darwin"
      ];

      forAllSystems = nixpkgs.lib.genAttrs systems;
    in
    {
      packages = forAllSystems (
        system:
        let
          pkgs = import nixpkgs { inherit system; };

          #Nix has mostly old ass hackage packages so many need to be updated
          #hs = pkgs.haskellPackages.override {
          #  Skip library test suites and their test-only dependencies.
          #  overrides = hself: hsuper: {
          #    pkgname = pkgs.haskellPackages.callHackageDirect {
          #      pkg = "pkgname";
          #      ver = "pkgversion";
          #      sha256 = pkgs.lib.fakeHash;
          #    } { };
          #    pkgname2 = pkgs.haskellPackages.callHackage "pkgname2" "pkgversion2" { };
          #  };
          #};
        in
        {
          # Add dependencies
          # default = pkgs.haskell.lib.addExtraLibraries (pkgs.haskell.lib.dontCheck (hs.callCabal2nix "project" ./. { })) [ <deps here> ];
          default = pkgs.haskell.lib.dontCheck (pkgs.haskellPackages.callCabal2nix "ijwmt" ./. { });
        }
      );

      devShells = forAllSystems (
        system:
        let
          pkgs = import nixpkgs { inherit system; };
        in
        {
          default = pkgs.mkShell {
            packages = with pkgs; [
              # Haskell
              haskellPackages.ghc
              cabal-install
              haskell-language-server
              ormolu
              haskellPackages.cabal-fmt
              hlint
              haskellPackages.ghcid

              # Development
              git
              gnumake
            ];
          };
        }
      );
    };
}
