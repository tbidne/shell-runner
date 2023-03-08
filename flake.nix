{
  description = "Shrun is a tool for concurrently running shell commands.";
  inputs = {
    flake-compat = {
      url = "github:edolstra/flake-compat";
      flake = false;
    };
    flake-utils.url = "github:numtide/flake-utils";
    nixpkgs.follows = "haskellNix/nixpkgs-unstable";

    haskellNix.url = "github:input-output-hk/haskell.nix";
  };
  outputs =
    { flake-compat
    , flake-utils
    , haskellNix
    , nixpkgs
    , self
    }:
    flake-utils.lib.eachSystem [ "x86_64-linux" "x86_64-darwin" ] (system:
    let
      index-state = "2023-03-07T00:00:00Z";
      overlays = [
        haskellNix.overlay
        (final: prev: {
          shrunProject =
            final.haskell-nix.cabalProject' {
              inherit index-state;
              src = ./.;
              compiler-nix-name = "ghc944";
              shell.tools =
                let
                  withIdx = { inherit index-state; };
                in
                {
                  cabal = withIdx;
                  ghcid = withIdx;

                  # https://github.com/haskell/haskell-language-server/issues/3427
                  haskell-language-server = {
                    inherit index-state;
                    version = "1.9.0.0";
                  };
                };
            };
        })
      ];
      pkgs = import nixpkgs { inherit system overlays; inherit (haskellNix) config; };
      flake = pkgs.shrunProject.flake {
        # This adds support for `nix build .#js-unknown-ghcjs:hello:exe:hello`
        # crossPlatforms = p: [p.ghcjs];
        #crossPlatforms = p: p.musl64;
      };
    in
    flake // {
      packages.default = flake.packages."shrun:exe:shrun";
    });
  nixConfig = {
    extra-substituters = [
      "https://nixcache.reflex-frp.org"
      "https://cache.nixos.org"
      "https://cache.iog.io"
      "https://cache.zw3rk.com"
    ];

    extra-trusted-public-keys = [
      "ryantrinkle.com-1:JJiAKaRv9mWgpVAz8dwewnZe0AzzEAzPkagE9SP5NWI="
      "cache.nixos.org-1:6NCHdD59X431o0gWypbMrAURkbJ16ZPMQFGspcDShjY="
      "hydra.iohk.io:f/Ea+s+dFdN+3Y/G+FDgSq+a5NEWhJGzdjvKNGv0/EQ="
      "loony-tools:pr9m4BkM/5/eSTZlkQyRt57Jz7OMBxNSUiMC4FkcNfk="
    ];

    # Adding this even though it doesn't seem to work...
    allow-import-from-derivation = "true";
  };
}
