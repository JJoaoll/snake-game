{ pkgs ? import <nixpkgs> {} }:

pkgs.mkShell {
  buildInputs = with pkgs; [
    libGL
    libGLU
    freeglut
    zlib
    haskellPackages.ghc
    haskellPackages.cabal-install
    xorg.xorgserver #Xvfb

  ];

shellHook = ''
    export LD_LIBRARY_PATH=${pkgs.libGL}/lib:${pkgs.libGLU}/lib:${pkgs.freeglut}/lib:${pkgs.zlib}/lib:$LD_LIBRARY_PATH
  '';

}


