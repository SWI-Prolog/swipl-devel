#!/bin/sh
# Stage installer resources for `productbuild':
#   - background.png:  shown in the left pane (the swipl icon, with
#     its DPI re-tagged to 72 so Installer renders it at a sensible
#     point size).  Paired with alignment="left" scaling="proportional"
#     in distribution.xml this mirrors Python's installer layout.
#   - welcome.rtf / license.rtf / conclusion.rtf:  the HTML sources
#     in man/macosx converted to flat RTF via textutil.  Installer.app
#     strips <img> tags from inline HTML and productbuild's --resources
#     flag does not recurse into .rtfd package folders, so we deliver
#     plain RTF and rely on the left-pane background for branding.
#
# Usage: macosx_installer_resources.sh <html-src-dir> <icon-png> <out-dir>

set -e

html_src=$1     # e.g.  /path/to/man/macosx
icon=$2         # e.g.  /path/to/desktop/swipl-256.png
out=$3          # e.g.  <build>/installer-resources

if [ -z "$html_src" ] || [ -z "$icon" ] || [ -z "$out" ]; then
    echo "Usage: $0 <html-src-dir> <icon-png> <out-dir>" >&2
    exit 1
fi

mkdir -p "$out"
cp "$icon" "$out/background.png"
sips -s dpiWidth 72 -s dpiHeight 72 "$out/background.png" \
     >/dev/null 2>&1

stage() {
    textutil -convert rtf -output "$2" "$1"
}

stage "$html_src/Welcome.html" "$out/welcome.rtf"
stage "$html_src/License.html" "$out/license.rtf"
stage "$html_src/SWIapp.html"  "$out/conclusion.rtf"
