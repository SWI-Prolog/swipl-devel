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

# Generate only the essential sizes
sips -z 16 16     "$INPUT" --out "$ICONSET_DIR/icon_16x16.png"
sips -z 32 32     "$INPUT" --out "$ICONSET_DIR/icon_16x16@2x.png"
sips -z 128 128   "$INPUT" --out "$ICONSET_DIR/icon_128x128.png"
sips -z 256 256   "$INPUT" --out "$ICONSET_DIR/icon_128x128@2x.png"

# Optionally add 512x512 if desired
# sips -z 512 512 "$INPUT" --out "$ICONSET_DIR/icon_256x256@2x.png"

# Convert to .icns
iconutil -c icns "$ICONSET_DIR" -o "$OUTPUT"

# Clean up
rm -rf "$ICONSET_DIR"

echo "Created $OUTPUT"
