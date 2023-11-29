{ lib
, stdenv
, fetchFromGitHub
, cmake
, wxGTK32
, libGL
, xorg
}:

stdenv.mkDerivation {
  name = "wxc";
  src = ./.;

  preConfigure = ''
    bash generate-version-header.sh
  '';

  nativeBuildInputs = [
    cmake
    wxGTK32 # Here because of wx-config
    xorg.libX11
  ];

  buildInputs = [
    libGL
  ];
}
