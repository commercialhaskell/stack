with (import (builtins.fetchTarball {
  name   = "nixpkgs-19.03";
  url    = "https://github.com/nixos/nixpkgs/archive/release-19.03.tar.gz";
  sha256 = "sha256:1p0xkcz183gwga9f9b24ihq3b7syjimkhr31y6h044yfmrkcnb6d";
}) {});
let
  supportedVersions = [
    "822"
    "844"
    "864"
  ];
  generate = version:
    let ghc = haskell.compiler."ghc${version}";
        main = "Main";
    in ''
      mkdir -p ghc${version}/
      ${ghc}/bin/ghc -fforce-recomp -hidir ghc${version} ${main}.hs && \
        rm *.o && \
        rm ${main}
  '';
in
  mkShell {
    shellHook =
        ''
          generate() {
            ${lib.concatMapStrings generate supportedVersions}
          }
        '';
  }
