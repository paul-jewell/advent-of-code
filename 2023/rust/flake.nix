{
  description = "Flake for advent-of-code 2023 using rust nightly";

  inputs = {
    nixpkgs.url      = "github:NixOS/nixpkgs/nixos-unstable";
    rust-overlay.url = "github:oxalica/rust-overlay";
    flake-utils.url  = "github:numtide/flake-utils";
  };

  outputs = { self, nixpkgs, rust-overlay, flake-utils, ... }:
    flake-utils.lib.eachDefaultSystem (system:
      let
        overlays = [ (import rust-overlay) ];
        pkgs = import nixpkgs {
          inherit system overlays;
        };
      in
      with pkgs;
      {
        devShells.default = mkShell {
          buildInputs = [
            openssl
            just
            cargo-watch
            cargo-nextest   
            pkg-config
            rust-script
            (
              rust-bin.selectLatestNightlyWith (toolchain: toolchain.default.override {
                extensions = [ 
                  "rust-src"
                  "rust-analyzer" 
                ];
              })
            ) 
          ];
        };
      }
    );
}
