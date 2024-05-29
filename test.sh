#!/bin/sh

DIRECTORY=test-dir

if [ -d $DIRECTORY ]; then
    echo "Directory $DIRECTORY already exists"
    exit 1
fi

gleam run -- --target-dir $DIRECTORY

if [ ! -d $DIRECTORY ]; then
    echo "Directory $DIRECTORY was not created"
    exit 1
fi

rm -rf $DIRECTORY
echo "Test passed!"
