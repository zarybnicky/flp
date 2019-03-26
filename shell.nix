(import ./. {}).shellFor {
  packages = p: [ p.braun-heap p.turing ];
  withHoogle = true;
}
