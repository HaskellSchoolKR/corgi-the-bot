{
  description = "corgithebot flake";

  inputs = {
    nixpkgs.url = "github:NixOS/nixpkgs/nixpkgs-unstable";
    flake-compat = {
      url = "github:edolstra/flake-compat";
      flake = false;
    };
    flake-utils.url = "github:numtide/flake-utils";
    gitignore = {
      url = "github:hercules-ci/gitignore.nix";
      flake = false;
    };
    discord-haskell = {
      url = "https://hackage.haskell.org/package/discord-haskell-1.12.1/discord-haskell-1.12.1.tar.gz";
      flake = false;
    };
  };
  outputs =
    inputs@{ self, nixpkgs, flake-compat, flake-utils, gitignore, ... }:
    {
      overlay = final: prev:
        with prev;
        let
          haskellOverrides = {
            overrides = hself: hsuper: {
              discord-haskell = hself.callCabal2nix "discord-haskell" inputs.discord-haskell {};
              # we override mkDerivation here to apply the following
              # tweak to each haskell package:
              #   if the package is broken, then we disable its check and relax the cabal bounds;
              #   otherwise, we leave it unchanged.
              # hopefully, this could fix packages marked as broken by nix due to check failures
              # or the build failure because of tight cabal bounds
              mkDerivation = args:
                let
                  broken = args.broken or false;
                  check = args.doCheck or true;
                  jailbreak = args.jailbreak or false;
                in hsuper.mkDerivation (args // {
                  jailbreak = if broken then true else jailbreak;
                  doCheck = if broken then false else check;
                });
            };
          };
          gitignoreSource = (import gitignore { inherit lib; }).gitignoreSource;

          # Source directories of our packages, should be consistent with cabal.project
          sourceDirs = {
            corgi-the-bot = ./.;
          };

          botSources =
            builtins.mapAttrs (_: dir: gitignoreSource dir) sourceDirs;

          extended = hpkgs:
            (hpkgs.override haskellOverrides).extend (hself: hsuper:
              # disable all checks for our packages
              builtins.mapAttrs (_: drv: haskell.lib.dontCheck drv)
              ((haskell.lib.packageSourceOverrides botSources) hself hsuper));

        in {
          inherit botSources;

          # Haskell packages extended with our packages
          botHpkgs = compiler: extended haskell.packages.${compiler};
        };
    } // (flake-utils.lib.eachSystem [ "aarch64-linux" ])
    (system:
      let
        pkgs = import nixpkgs {
          inherit system;
          overlays = [ self.overlay ];
          config = { allowBroken = true; };
        };

        # GHC versions
        ghc8107 = pkgs.botHpkgs "ghc8107";

        # Create a development shell of hls project
        # See https://github.com/NixOS/nixpkgs/blob/5d4a430472cafada97888cc80672fab255231f57/pkgs/development/haskell-modules/make-package-set.nix#L319
        mkDevShell = hpkgs:
          with pkgs;
          hpkgs.shellFor {
            packages = p:
              with builtins;
              map (name: p.${name}) (attrNames pkgs.botSources);
            buildInputs = [
              gmp
              zlib
              ncurses
              capstone
              haskellPackages.cabal-install
              haskellPackages.cabal-fmt
            ];

            src = null;
            shellHook = ''
              export LD_LIBRARY_PATH=${gmp}/lib:${zlib}/lib:${ncurses}/lib:${capstone}/lib
              export DYLD_LIBRARY_PATH=${gmp}/lib:${zlib}/lib:${ncurses}/lib:${capstone}/lib
              export PATH=$PATH:$HOME/.local/bin
            '';
          };

        mkPackage = hpkgs:
          with pkgs.haskell.lib;
          justStaticExecutables hpkgs.corgi-the-bot;
      in with pkgs; rec {
        packages = {
          corgithebot = mkPackage ghc8107;
          corgithebot-dev = mkDevShell ghc8107;
        };

        defaultPackage = packages.corgithebot;

        devShell = packages.corgithebot-dev;
      });
}
