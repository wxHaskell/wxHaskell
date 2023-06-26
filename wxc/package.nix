{ lib
, stdenv
, fetchFromGitHub
, cmake
, wxGTK32
, libGL
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
  ];

  buildInputs = [
    libGL
  ];

  # We shouldn't set this, but FindwxWidgets fails to properly fill it and
  # configuration fails with
  # "Could NOT find wxWidgets (missing: wxWidgets_LIBRARIES)"
  cmakeFlags = [ "-DwxWidgets_LIBRARIES=-L${wxGTK32}/lib" ];
}
