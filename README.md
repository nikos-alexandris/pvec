# Simple persistent vectors in OCaml

Initialisation and example run:

```sh
opam switch create .
eval $(opam env)
dune build --profile release
dune exec pvec
┌──────────┬──────────┬────────────────┬──────────┬───────────────┬────────────┐
│ Name     │ Time/Run │        mWd/Run │ mjWd/Run │      Prom/Run │ Percentage │
├──────────┼──────────┼────────────────┼──────────┼───────────────┼────────────┤
│ pvec     │  73.21ms │ 31_213_768.73w │   1.21Mw │ 1_208_082.49w │     44.17% │
│ hector   │  18.80ms │        569.00w │   3.03Mw │         9.17w │     11.34% │
│ patricia │ 165.73ms │ 71_446_031.00w │   8.02Mw │ 8_020_584.03w │    100.00% │
└──────────┴──────────┴────────────────┴──────────┴───────────────┴────────────┘
```
