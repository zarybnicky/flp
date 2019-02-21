(import ./. {}).shellFor {
  packages = p: [ p.binary-heap ];
  withHoogle = true;
}
