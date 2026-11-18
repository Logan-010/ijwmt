
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

      perSystem =
        system:
        let
          pkgs = import nixpkgs {
            inherit system;
          };

          hs = pkgs.haskellPackages.override {
            overrides = hself: hsuper: {
              base32 = hself.callHackageDirect {
                pkg = "base32";
                ver = "1.0";
                sha256 = "sha256-IX/7f4xzfKymC9DAWkQLj9mS14bQ6HBEwUJX5MBlxaU=";
              } { };
            };
          };

          ijwmt = pkgs.haskell.lib.dontCheck (
            hs.callCabal2nix "ijwmt" ./. { }
          );
        in
        {
          package = ijwmt;

          devShell = pkgs.mkShell {
            packages = with pkgs; [
              # Haskell
              hs.ghc
              cabal-install
              haskell-language-server
              ormolu
              hs.cabal-fmt
              hlint
              hs.ghcid

              # Development
              git
              gnumake
            ];
          };
        };
    in
    {
      packages = forAllSystems (
        system:
        {
          default = (perSystem system).package;
        }
      );

      devShells = forAllSystems (
        system:
        {
          default = (perSystem system).devShell;
        }
      );
    };
}
