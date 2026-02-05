#!/usr/bin/env bash
# Test inline image, video, and webkit in one buffer
# Usage: ./test/manual/run-inline-media-test.sh

set -e

cd "$(dirname "$0")/../.."

echo "=== Neomacs Inline Media Test ==="
echo "Testing: inline image + video + webkit in one buffer"
echo ""

# Check for test media files
if [ -f ~/Pictures/559-4K.jpg ]; then
    echo "Image: ~/Pictures/559-4K.jpg"
elif [ -f test/data/image/black.jpg ]; then
    echo "Image: test/data/image/black.jpg"
else
    echo "Image: [not found - will be skipped]"
fi

if [ -f ~/Videos/test.mp4 ]; then
    echo "Video: ~/Videos/test.mp4"
elif [ -f test/data/video/test.mp4 ]; then
    echo "Video: test/data/video/test.mp4"
else
    echo "Video: [not found - will be skipped]"
fi

echo "WebKit: https://example.com/"
echo ""
echo "Starting Emacs..."
echo "Log: /tmp/inline-media-test.log"
echo ""

RUST_LOG=info DISPLAY=:0 ./src/emacs -Q -l test/manual/inline-media-test.el 2>&1 | tee /tmp/inline-media-test.log

echo ""
echo "=== Test complete ==="
