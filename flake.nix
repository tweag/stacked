{
  description = "Stacked";

  inputs.nixpkgs.url = "github:NixOS/nixpkgs/nixos-unstable";
  inputs.flake-utils.url = "github:numtide/flake-utils";
  inputs.pre-commit-hooks.url = "github:cachix/pre-commit-hooks.nix";

  outputs = { self, nixpkgs, flake-utils, pre-commit-hooks }:
    flake-utils.lib.eachDefaultSystem (system:
      let
        pkgs = import nixpkgs { inherit system; };

        # need to match Stackage LTS version from stack.yaml snapshot
        ghc-version = "ghc984";

        haskell =
          pkgs.haskell.packages."${ghc-version}";

        devTools = [
          haskell.ghc # GHC compiler in the desired version (will be available on PATH)
          haskell.ghcid # Continuous terminal Haskell compile checker
          haskell.ormolu # Haskell formatter
          haskell.hlint # Haskell codestyle checker
          haskell.hoogle # Lookup Haskell documentation
          haskell.haskell-language-server # LSP server for editor
          haskell.implicit-hie # auto generate LSP hie.yaml file from cabal
          haskell.retrie # Haskell refactoring tool
          stack-wrapped
          pkgs.treefmt
          pkgs.zlib # External C library needed by some Haskell packages
        ];

        precommit = pre-commit-hooks.lib.${system}.run {
            src = ./.;
            hooks.treefmt.enable = true;
        };

        # Wrap Stack to work with our Nix integration. We don't want to modify
        # stack.yaml so non-Nix users don't notice anything.
        # - no-nix: We don't want Stack's way of integrating Nix.
        # --system-ghc    # Use the existing GHC on PATH (will come from this Nix file)
        # --no-install-ghc  # Don't try to install GHC if no matching GHC found on PATH
        # See https://docs.haskellstack.org/en/stable/nix_integration/
        stack-wrapped = pkgs.symlinkJoin {
          name = "stack"; # will be available as the usual `stack` in terminal
          paths = [ pkgs.stack ];
          buildInputs = [ pkgs.makeWrapper ];
          postBuild = ''
            wrapProgram $out/bin/stack \
              --add-flags "\
                --no-nix \
                --system-ghc \
                --no-install-ghc \
              "
          '';
        };

      in {

        devShells.default = pkgs.mkShell {
          buildInputs = devTools;

          # sets up the pre-commit hooks upon entering the dev shell
          inherit (precommit) shellHook;

          # Make external Nix c libraries like zlib known to GHC, like
          # pkgs.haskell.lib.buildStackProject does
          # https://github.com/NixOS/nixpkgs/blob/d64780ea0e22b5f61cd6012a456869c702a72f20/pkgs/development/haskell-modules/generic-stack-builder.nix#L38
          LD_LIBRARY_PATH = pkgs.lib.makeLibraryPath devTools;
        };

      }
    );
}
