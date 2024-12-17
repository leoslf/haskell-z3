{
  description = "z3";
  inputs.nixpkgs.url = "github:NixOS/nixpkgs/nixos-unstable";
  inputs.flake-utils.url = "github:numtide/flake-utils";

  outputs = { self, nixpkgs, flake-utils }:
    flake-utils.lib.eachDefaultSystem (system:
      let
        pkgs = nixpkgs.legacyPackages.${system};

        haskellPkgs =
          pkgs.haskell.packages."ghc928"; # matches stackage LTS version 20.26

        devtools = [
          pkgs.gcc
          pkgs.pkg-config
          pkgs.git
          pkgs.libffi
          pkgs.z3_4_8
          pkgs.bashInteractive
          pkgs.zlib
          haskellPkgs.ghc # GHC compiler in the desired version (will be available on PATH)
          haskellPkgs.ghcid # Continuous terminal Haskell compile checker
          haskellPkgs.hoogle # Documentation Lookup
          haskellPkgs.retrie # Refactoring tool
          stack-wrapped
        ];

        # Wrap stack to work with our nix integration
        # we do not want to modify stack.yaml so non-nix users don't notice anything
        # We don't want Stack's way of integrating Nix: --no-nix
        # Use the existing GHC on PATH (will come from this Nix: --system-ghc
        # Don't try to install GHC if no matching GHC found on PATH: --no-install-ghc
        stack-wrapped = pkgs.symlinkJoin {
          # available as the usual "stack" in terminal
          name = "stack";
          paths = [ pkgs.stack ];
          buildInputs = [ pkgs.makeWrapper ];
          postBuild = ''
            wrapProgram $out/bin/stack \
              --add-flags "\
                --nix \
                --no-nix-pure \
                --system-ghc \
                --no-install-ghc \
              "
          '';
        };

        app = pkgs.haskell.lib.buildStackProject {
          name = "Adventofcode";
          src = ./.;
          ghc = haskellPkgs.ghc;
          buildInputs = devtools;
        };
      in {
        devShells.default = pkgs.mkShell {
          shellHook = ''
            export SHELL=/run/current-system/sw/bin/bash
          '';
          buildInputs = devtools;
          # inputsFrom = builtins.attrValues self.packages.${system};

          # Make external Nix c libraries like zlib known to GHC, like
          # pkgs.haskell.lib.buildStackProject does
          # https://github.com/NixOS/nixpkgs/blob/d64780ea0e22b5f61cd6012a456869c702a72f20/pkgs/development/haskell-modules/generic-stack-builder.nix#L38
          LD_LIBRARY_PATH = pkgs.lib.makeLibraryPath devtools;
          NIX_PATH = "nixpkgs=" + pkgs.path;
        };

        packages.default = app;
      }
    );
}
