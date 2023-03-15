{ pkgs, libsodium }:
pkgs.stdenv.mkDerivation {
  name = "libsodium";
  src = libsodium;
  configurePhase = "./autogen.sh; ./configure --prefix=$out";
  buildPhase = "make";
  installPhase = "make install";
  buildInputs = [
    pkgs.libtool
    pkgs.autoconf
    pkgs.automake
  ];
}
