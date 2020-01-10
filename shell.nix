(import ./. {}).shellFor {
  packages = p: with p; [ braun-heap-singletons braun-heap-typelits turing ];
  withHoogle = true;
}
