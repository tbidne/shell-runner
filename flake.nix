{
  description = "Shrun is a tool for concurrently running shell commands.";
  inputs = {
    # nix
    flake-parts.url = "github:hercules-ci/flake-parts";
    nix-hs-utils.url = "github:tbidne/nix-hs-utils";
    nixpkgs.url = "github:nixos/nixpkgs/nixos-unstable";

    # haskell
    algebra-simple = {
      url = "github:tbidne/algebra-simple";
      inputs.flake-parts.follows = "flake-parts";
      inputs.nix-hs-utils.follows = "nix-hs-utils";
      inputs.nixpkgs.follows = "nixpkgs";
    };
    bounds = {
      url = "github:tbidne/bounds";
      inputs.flake-parts.follows = "flake-parts";
      inputs.nix-hs-utils.follows = "nix-hs-utils";
      inputs.nixpkgs.follows = "nixpkgs";
    };
    exception-utils = {
      url = "github:tbidne/exception-utils";
      inputs.flake-parts.follows = "flake-parts";
      inputs.nix-hs-utils.follows = "nix-hs-utils";
      inputs.nixpkgs.follows = "nixpkgs";
    };
    fs-utils = {
      url = "github:tbidne/fs-utils";
      inputs.flake-parts.follows = "flake-parts";
      inputs.nix-hs-utils.follows = "nix-hs-utils";
      inputs.nixpkgs.follows = "nixpkgs";
    };
    effectful-libs = {
      url = "github:tbidne/effectful-libs";
      inputs.flake-parts.follows = "flake-parts";
      inputs.nix-hs-utils.follows = "nix-hs-utils";
      inputs.nixpkgs.follows = "nixpkgs";

      inputs.algebra-simple.follows = "algebra-simple";
      inputs.bounds.follows = "bounds";
      inputs.exception-utils.follows = "exception-utils";
      inputs.fs-utils.follows = "fs-utils";
    };
    relative-time = {
      url = "github:tbidne/relative-time";
      inputs.flake-parts.follows = "flake-parts";
      inputs.nix-hs-utils.follows = "nix-hs-utils";
      inputs.nixpkgs.follows = "nixpkgs";

      inputs.algebra-simple.follows = "algebra-simple";
      inputs.bounds.follows = "bounds";
    };
    si-bytes = {
      url = "github:tbidne/si-bytes";
      inputs.flake-parts.follows = "flake-parts";
      inputs.nix-hs-utils.follows = "nix-hs-utils";
      inputs.nixpkgs.follows = "nixpkgs";

      inputs.algebra-simple.follows = "algebra-simple";
      inputs.bounds.follows = "bounds";
    };
    smart-math = {
      url = "github:tbidne/smart-math";
      inputs.flake-parts.follows = "flake-parts";
      inputs.nix-hs-utils.follows = "nix-hs-utils";
      inputs.nixpkgs.follows = "nixpkgs";

      inputs.algebra-simple.follows = "algebra-simple";
      inputs.bounds.follows = "bounds";
    };
  };
  outputs =
    inputs@{
      flake-parts,
      nix-hs-utils,
      nixpkgs,
      self,
      ...
    }:
    flake-parts.lib.mkFlake { inherit inputs; } {
      perSystem =
        { pkgs, system, ... }:
        let
          ghc-version = "ghc982";
          compiler = pkgs.haskell.packages."${ghc-version}".override {
            overrides =
              final: prev:
              {
                effectful-core = (
                  final.callHackageDirect {
                    pkg = "effectful-core";
                    ver = "2.5.0.0";
                    sha256 = "sha256-UCbMP8BfNfdIRTLzB4nBr17jxRp5Qmw3sTuORO06Npg=";
                  } { }
                );
                effectful = (
                  final.callHackageDirect {
                    pkg = "effectful";
                    ver = "2.5.0.0";
                    sha256 = "sha256-lmM0kdM5PS45Jol5Y2Nw30VWWfDPiPJLrwVj+GmJSOQ=";
                  } { }
                );
                strict-mutable-base = (
                  final.callHackageDirect {
                    pkg = "strict-mutable-base";
                    ver = "1.1.0.0";
                    sha256 = "sha256-cBSwoNGU/GZDW3eg7GI28t0HrrrxMW9hRapoOL2zU7Q=";
                  } { }
                );
                typed-process-effectful = hlib.dontCheck (
                  final.callHackageDirect {
                    pkg = "typed-process-effectful";
                    ver = "1.0.0.3";
                    sha256 = "sha256-q7auI60lmW2X9PHCLPPVOqIfRXET1dAr8VHhCtmecYI=";
                  } { }
                );
              }
              // nix-hs-utils.mkLibs inputs final [
                "algebra-simple"
                "bounds"
                "exception-utils"
                "fs-utils"
                "relative-time"
                "si-bytes"
                "smart-math"
              ]
              // nix-hs-utils.mkRelLibs "${inputs.effectful-libs}/lib" final [
                "concurrent-effectful"
                "environment-effectful"
                "fs-effectful"
                "ioref-effectful"
                "optparse-effectful"
                "stm-effectful"
                "terminal-effectful"
                "time-effectful"
                "unix-compat-effectful"
              ];
          };
          hlib = pkgs.haskell.lib;
          compilerPkgs = {
            inherit compiler pkgs;
          };
          mkPkg =
            returnShellEnv:
            nix-hs-utils.mkHaskellPkg {
              inherit compiler pkgs returnShellEnv;
              name = "shrun";
              root = ./.;
            };
          stack-wrapped = pkgs.symlinkJoin {
            name = "stack";
            paths = [ pkgs.stack ];
            buildInputs = [ pkgs.makeWrapper ];
            postBuild = ''
              wrapProgram $out/bin/stack --add-flags "--no-nix --system-ghc"
            '';
          };

          pkgsMkDrv = {
            inherit pkgs;
            mkDrv = false;
          };
        in
        {
          packages.default = mkPkg false;
          devShells = {
            default = mkPkg true;

            notifyTests = pkgs.mkShell {
              buildInputs = nix-hs-utils.mkBuildTools compilerPkgs;
              shellHook = ''
                export NOTIFY_TESTS=1
              '';
            };

            stack = pkgs.mkShell {
              buildInputs = [
                compiler.ghc
                pkgs.zlib
                stack-wrapped
              ];
            };
          };

          apps = {
            format = nix-hs-utils.mergeApps {
              apps = [
                (nix-hs-utils.format (compilerPkgs // pkgsMkDrv))
                (nix-hs-utils.format-yaml pkgsMkDrv)
              ];
            };

            lint = nix-hs-utils.mergeApps {
              apps = [
                (nix-hs-utils.lint (compilerPkgs // pkgsMkDrv))
                (nix-hs-utils.lint-yaml pkgsMkDrv)
              ];
            };

            lint-refactor = nix-hs-utils.lint-refactor compilerPkgs;
          };
        };
      systems = [
        "aarch64-darwin"
        "aarch64-linux"
        "x86_64-darwin"
        "x86_64-linux"
      ];
    };
}
