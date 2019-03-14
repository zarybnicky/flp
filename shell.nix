(import ./. {}).shellFor {
  packages = p: [ p.binary-heap p.turing ];
  withHoogle = true;
}
