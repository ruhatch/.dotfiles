{ pkgs ? (import <nixpkgs> {})
, stdenv ? pkgs.stdenv
, xdotool ? pkgs.xdotool
, makeWrapper ? pkgs.makeWrapper
, wmctrl ? pkgs.wmctrl
, fetchFromGitHub ?  pkgs.fetchFromGitHub
, bash ? pkgs.bash
, python3 ? pkgs.python3
, libinput ? pkgs.libinput
, procps ? pkgs.procps
}:

stdenv.mkDerivation rec {
  version = "2.16";
  name = "libinput-gestures-${version}";
  src = fetchFromGitHub {
    owner = "bulletmark";
    repo = "libinput-gestures";
    rev = version;
    sha256 = "0ix1ygbrwjvabxpq8g4xcfdjrcc6jq79vxpbv6msaxmjxp6dv17w";
  };

  patches = [./paths.diff];

  buildInputs = with pkgs; [ makeWrapper ];

  installFlags = "DESTDIR=$(out)";
  preInstall = ''
    patchShebangs $(pwd)
    substituteAllInPlace libinput-gestures.desktop
  '';
  postInstall = ''
    wrapProgram $out/bin/libinput-gestures \
      --prefix PATH : "${python3}/bin:${xdotool}/bin:${wmctrl}/bin:${libinput}/bin"
    substituteInPlace $out/bin/libinput-gestures-setup \
      --replace "DIR=\"/" "DIR=\"$out/"
    wrapProgram $out/bin/libinput-gestures-setup \
      --prefix PATH : "${procps}/bin:${libinput}/bin"
  '';

  meta = with stdenv.lib; {
    description = "libinput gesture recognition";
    license = licenses.gpl3;
    maintainers = with maintainers; [ yorickvp ];
  };
}
