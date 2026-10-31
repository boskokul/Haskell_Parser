#!/usr/bin/env bash
set -euo pipefail

TARGET="compiler"
EXAMPLES_DIR="examples"

if ! command -v ghc >/dev/null 2>&1; then
  echo "ghc is required but not installed. Please install the Haskell toolchain first."
  exit 1
fi

mkdir -p "$EXAMPLES_DIR"

ghc -O0 -o "$TARGET" Main.hs HsParser.hs AstTypes.hs TypeInfer.hs Desugar.hs

shopt -s nullglob
for src in "$EXAMPLES_DIR"/*.prog; do
  out="${src%.prog}.json"
  ./$TARGET "$src" "$out"

  if [[ ! -s "$out" ]]; then
    echo "Verification failed: $out was not generated or is empty."
    exit 1
  fi

  echo "OK: $src -> $out"
done

echo "All example programs passed validation."
