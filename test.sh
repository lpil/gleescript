#!/bin/sh

DIRECTORY=test-dir

rm -rf $DIRECTORY

gleam run -- --target-dir $DIRECTORY

if [ ! -d $DIRECTORY ]; then
	echo "Directory $DIRECTORY was not created"
	exit 1
fi

echo "Test passed!"
