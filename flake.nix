{
  description = "Development environment";

  inputs.nixpkgs.url = "github:NixOS/nixpkgs/nixos-unstable";

  outputs = { self, nixpkgs }:
    let
      supportedSystems = [ "x86_64-linux" "aarch64-linux" "x86_64-darwin" "aarch64-darwin" ];
      forEachSupportedSystem = f: nixpkgs.lib.genAttrs supportedSystems (system: f {
        pkgs = import nixpkgs { inherit system; };
      });
    in
    {
      devShells = forEachSupportedSystem ({ pkgs }: {
        default = pkgs.mkShell {
          packages =
            (with pkgs.elmPackages; [
              elm
              elm-test-rs
              elm-format
              nodejs
            ]) ++ (with pkgs; [
              uglify-js

              (writeScriptBin "run-tests" ''
                elm-test-rs
              '')

              (writeScriptBin "run-demo" ''
                cd demo
                elm reactor
              '')

              (writeScriptBin "build-demo" ''
                cd demo
                elm make Main.elm --optimize --output build/index.js
                cp -f index.html build/index.html
                uglifyjs build/index.js --compress "pure_funcs=[F2,F3,F4,F5,F6,F7,F8,F9,A2,A3,A4,A5,A6,A7,A8,A9],pure_getters,keep_fargs=false,unsafe_comps,unsafe" | uglifyjs --mangle --output build/index.js
              '')

            ]);
        };
      });
    };
}
