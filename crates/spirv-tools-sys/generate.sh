#!/bin/bash
set -eu

dir=$(dirname "$(dirname "$0")")

if ! [ -d "${dir}/target" ]; then
    mkdir "${dir}/target"
fi

# Compile our "script" if the binary doesn't already exist
if [ "${1-}" == "-f" ] || ! [ -f "${dir}/target/generate" ]; then
    echo "Compiling generate..."

    rustc -g -o "${dir}/target/generate" "${dir}/spirv-tools-sys/generate.rs"

    if [ "${1-}" == "-f" ]; then
        # remove the force flag when sending the arguments to generate 
        shift
    fi
fi

gen=$(realpath "${dir}/target/generate")
(cd spirv-tools-sys && $gen "${@}")
