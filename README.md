# koiiword

This is our CS 3110 final project: a Scrabble clone!

## setup

This project depends on the following libraries:
- LWT ([docs](https://ocsigen.org/lwt/latest/manual/manual), [github](https://github.com/ocsigen/lwt))
- Lambda-Term ([github](https://github.com/ocaml-community/lambda-term))

They can be installed like this:
```bash
opam install -y lwt lwt_ppx lambda-term
```

This project is set up so that it can be debugged using Visual Studio Code. To
use this debugger, you will also need to install the Ocamlearlybird VSCode
extension (`hackwaly.ocamlearlybird`), and install the `earlybird` OPAM package:
```bash
opam install -y earlybird
```

In this case, you can use the "OCaml Debug" profile in VSCode, and it will
automatically build, run, and debug the project.

Otherwise, you can just compile and run using `dune`:
```bash
dune build
dune exec bin/main.exe
```
