{ ... }: {
  perSystem = { self', ... }: { apps.default = self'.apps.dancelor; };
}
