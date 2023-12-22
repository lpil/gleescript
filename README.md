# gleescript

Bundle your Gleam-on-Erlang project into an escript, a single executable file!

[![Package Version](https://img.shields.io/hexpm/v/gleescript)](https://hex.pm/packages/gleescript)
[![Hex Docs](https://img.shields.io/badge/hex-docs-ffaff3)](https://hexdocs.pm/gleescript/)

Add this package to your Gleam project and run `gleam run -m gleescript` to
generate an escript.

```sh
gleam add gleescript
gleam run -m gleescript
#  Compiling your_project
#   Compiled in 0.26s
#    Running gleescript.main
#  Generated ./your_project

chmod +x ./your_project
./your_project
# Hello from your_project!
```
