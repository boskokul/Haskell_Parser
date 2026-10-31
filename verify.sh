#!/usr/bin/env bash
set -euo pipefail

TARGET="compiler"
INPUT="test.prog"
OUTPUT="typed_ast.json"

cat > "$INPUT" <<'EOF'
add a b = a + b
x = 3 + 4
flag = if x > 5 then True else False
EOF

if ! command -v ghc >/dev/null 2>&1; then
  echo "ghc is required but not installed. Please install the Haskell toolchain first."
  exit 1
fi

ghc -O0 -o "$TARGET" Main.hs HsParser.hs AstTypes.hs TypeInfer.hs Desugar.hs
./"$TARGET" "$INPUT" "$OUTPUT"

if [[ ! -s "$OUTPUT" ]]; then
  echo "Verification failed: $OUTPUT was not generated or is empty."
  exit 1
fi

echo "Verification succeeded: $OUTPUT was generated successfully."
