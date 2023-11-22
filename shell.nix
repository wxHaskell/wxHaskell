{ pkgs ? import <nixpkgs> {} }:

pkgs.mkShell {
  nativeBuildInputs = [
    pkgs.pkg-config
  ];
  buildInputs = [
    pkgs.wxGTK32 # wxdirect needs wx-config
    pkgs.libGL # for OpenGLRaw
    pkgs.libGLU # for GLURaw
  ];
}
