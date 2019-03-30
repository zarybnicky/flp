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
      postFixup = ''
        if [ ! -e $out/bin/braun-heap-gui.jsexe ]; then
          echo "GHC only build? Not compressing JS"
          exit 0
        fi
        pushd $out/bin/braun-heap-gui.jsexe
        mv all.js all.unminified.js
        ${pkgs.closurecompiler}/bin/closure-compiler \
          all.unminified.js \
          -O ADVANCED \
          --externs=all.js.externs \
          --jscomp_off=checkVars \
          --create_source_map="all.js.map" \
          --source_map_format=V3 \
          > all.js
        echo "//# sourceMappingURL=all.js.map" >> all.js
        cp ${./static}/* .
        popd
      '';
    });
  };
})
