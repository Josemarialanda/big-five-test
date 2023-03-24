{
  description = "Bellroy take home test";

  inputs.nixpkgs.url = "github:NixOS/nixpkgs/nixpkgs-unstable";
  inputs.flake-utils.url = "github:numtide/flake-utils";

  outputs = inputs:
    let
      overlay = final: prev: {
        haskell = prev.haskell // {
          packageOverrides = hfinal: hprev:
            prev.haskell.packageOverrides hfinal hprev // {
              big-five-test = hfinal.callCabal2nix "big-five-test" ./. { };
            };
        };
        big-five-test = final.haskell.lib.compose.justStaticExecutables final.haskellPackages.big-five-test;
      };
      perSystem = system:
        let
          pkgs = import inputs.nixpkgs { inherit system; overlays = [ overlay ]; };
          hspkgs = pkgs.haskellPackages;
        in
        {
          devShell = hspkgs.shellFor {
            withHoogle = true;
            packages = p: [ p.big-five-test ];
            buildInputs = [
              hspkgs.zlib
              hspkgs.cabal-install
              hspkgs.haskell-language-server
              hspkgs.hlint
              hspkgs.ormolu
              pkgs.bashInteractive
            ];
          };
          defaultPackage = pkgs.big-five-test;
          apps.uploadResults = inputs.flake-utils.lib.mkApp { drv = pkgs.writeShellScriptBin "uploadResults" 
          ''
            ${pkgs.big-five-test}/bin/big-five-test-exe "res/big5test.txt" "Jose Maria Landa Chavez" "josemaria.landa@gmail.com"
            curl --header "Content-Type: application/json" \
              --request POST \
              --data @results.json \
              https://recruiting.bellroy.com/api/v1/roles/bellroy-tech-team-recruit/big_five_profile_submissions
          '' 
          ; 
          };
        };
    in
    { inherit overlay; } // 
      inputs.flake-utils.lib.eachDefaultSystem perSystem;
}
