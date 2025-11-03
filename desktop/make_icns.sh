#!/bin/bash

# Usage: ./make_icns.sh input.png output.icns

if [ $# -ne 2 ]; then
  echo "Usage: $0 input.png output.icns"
  exit 1
fi

INPUT="$1"
OUTPUT="$2"
ICONSET_DIR="temp.iconset"

mkdir -p "$ICONSET_DIR"

resize()
{ sips -z $1 $1 "$INPUT" --out "$ICONSET_DIR/icon_${2}.png" > /dev/null
}

# Generate only the essential sizes
resize 16 16x16
resize 32 16x16@2x
resize 128 128x128
resize 256 128x128@2x

# Convert to .icns
iconutil -c icns "$ICONSET_DIR" -o "$OUTPUT" || exit 1

# Clean up
rm -rf "$ICONSET_DIR"

# echo "Created $OUTPUT"
