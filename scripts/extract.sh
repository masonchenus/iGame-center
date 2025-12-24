#!/bin/bash
# extract.sh - Extract a tar.gz file

if [ -z "$1" ]; then
  echo "Usage: ./extract.sh <archive>"
  exit 1
fi

ARCHIVE="$1"
tar -xzf "$ARCHIVE"
echo "✅ Archive $ARCHIVE extracted successfully."
