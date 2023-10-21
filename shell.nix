{
  pkgs ? import <nixpkgs> {}
}:
let
  lamdera = pkgs.stdenv.mkDerivation rec {
    name = "lamdera";
    src = pkgs.fetchurl {
      url = "https://static.lamdera.com/bin/lamdera-1.2.0-macos-arm64";
      sha256 = "174a5bfec355361c4f030861405513818be25fd7e4325f7221aa71ebd27475d3";
    };
    phases = ["installPhase"];
    installPhase = "install -D $src $out/bin/lamdera";
  };
in
pkgs.mkShell {
  buildInputs = [
    pkgs.elmPackages.elm
    pkgs.elmPackages.elm-test
    pkgs.elmPackages.elm-format
    pkgs.elmPackages.elm-review
    pkgs.elmPackages.elm-spa
    lamdera
    pkgs.jq
  ];
  shellHook = ''
    settings_file=.vscode/settings.json
    set_vscode_setting() {
      jq --arg key "$1" --arg value "$2" '.[$key] = $value' $settings_file > $settings_file.tmp && mv $settings_file.tmp $settings_file
    }
    mkdir -p .vscode
    if [ ! -f $settings_file ]; then
      echo '{}' > $settings_file
    fi
    set_vscode_setting "elmLS.elmPath" "$(which lamdera)"
    set_vscode_setting "elmLS.elmReviewPath" "$(which elm-review)"
    set_vscode_setting "elmLS.elmFormatPath" "$(which elm-format)"
    set_vscode_setting "elmLS.elmTestPath" "$(which elm-test)"
  '';
}