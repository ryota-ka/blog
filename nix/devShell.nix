{
  mkShell,
  nixfmt,
  yarn,
}:

mkShell {
  buildInputs = [
    nixfmt
    yarn
  ];
}
