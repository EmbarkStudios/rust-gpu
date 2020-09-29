#!/bin/bash
set -eu

os="$1"

tmparch=$(mktemp)

mkdir ~/spirv-tools

if [ "$os" == 'Linux' ]; then
    curl -fL -o "$tmparch" https://storage.googleapis.com/spirv-tools/artifacts/prod/graphics_shader_compiler/spirv-tools/linux-clang-release/continuous/1250/20200928-083216/install.tgz

    tar xzf "$tmparch" -C ~/spirv-tools
elif [ "$os" == 'Windows' ]; then
    curl -fL -o "$tmparch" https://storage.googleapis.com/spirv-tools/artifacts/prod/graphics_shader_compiler/spirv-tools/windows-msvc-2017-release/continuous/1232/20200928-085551/install.zip

    unzip "$tmparch" -d ~/spirv-tools
else
    echo "Unimplemented host os $os"
    exit 1
fi
