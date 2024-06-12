#!/bin/sh

set -eu

# Reset out artefacts
rm -rf gleescript ./tmp

# No arguments
gleam run -m gleescript
./gleescript

# --out tmp/without-equals
gleam run -m gleescript -- --out tmp/without-equals
./tmp/without-equals/gleescript

# --out=tmp/with-equals
gleam run -m gleescript -- --out=tmp/with-equals
./tmp/with-equals/gleescript

echo "Tests passed!"
