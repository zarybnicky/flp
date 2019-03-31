(import ../reflex-platform {}).project ({ pkgs, ... }: {
  packages = {
    diagrams-reflex = ../diagrams-reflex;
    braun-heap-typelits = ../braun-heap-typelits;
    braun-heap-gui = ./.;
  };

  shells = {
    ghc = ["braun-heap-typelits" "braun-heap-gui" "diagrams-reflex"];
  };

  overrides = with pkgs.haskell.lib; self: super: {
    braun-heap-gui = overrideCabal super.braun-heap-gui (drv: {
      enableSharedExecutables = false;
      enableSharedLibraries = false;
      configureFlags = [
        "--ghc-option=-optl=-static"
        "--ghc-option=-optl=-pthread"
        "--ghc-option=-optl=-L${pkgs.gmp6.override { withStatic = true; }}/lib"
        "--ghc-option=-optl=-L${pkgs.zlib.static}/lib"
        "--ghc-option=-optl=-L${pkgs.glibc.static}/lib"
        "--ghc-option=-optl=-L${pkgs.ncurses.override { enableStatic = true; }}/lib"
      ];
    });
  };
})
