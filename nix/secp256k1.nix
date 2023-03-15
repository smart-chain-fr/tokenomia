{ pkgs, secp256k1 }:
pkgs.stdenv.mkDerivation {
  name = "secp256k1";
  src = secp256k1;
  configurePhase = ''
    ./autogen.sh;
    ./configure --enable-module-schnorrsig --enable-experimental --prefix=$out;
  '';
  buildPhase = "make; make check";
  installPhase = "make install";
  buildInputs = [
    pkgs.libtool
    pkgs.autoconf
    pkgs.automake
  ];
}
