# gleescript

Bundle your Gleam-on-Erlang project into an escript, a single executable file!
You may find this useful for writing command line programs and scripts in
Gleam.

[![Package Version](https://img.shields.io/hexpm/v/gleescript)](https://hex.pm/packages/gleescript)
[![Hex Docs](https://img.shields.io/badge/hex-docs-ffaff3)](https://hexdocs.pm/gleescript/)

## Usage

Add this package to your Gleam project and run `gleam run -m gleescript` to
generate an escript.

```sh
gleam add gleescript
gleam run -m gleescript
#  Compiling your_project
#   Compiled in 0.26s
#    Running gleescript.main
#  Generated ./your_project

./your_project
# Hello from your_project!
```

The escript can run on any computer that has the Erlang VM installed. Older
versions of the virtual machine may not support the newer bytecode contained in
the escript. Typically being within a couple major versions of the version used
to build the escript is safe.

See the [Erlang escript documentation][1] for more information.

[1]: https://www.erlang.org/doc/man/escript.html


By default the executable is created in the current directory. The `--out` flag
can be used to write to a different directory.

```sh
gleam run -m gleescript -- --out=./some/directory
#  Compiling your_project
#   Compiled in 0.26s
#    Running gleescript.main
#  Generated ./some/directory/your_project

./some/directory/your_project
# Hello from your_project!
```

Have fun!
