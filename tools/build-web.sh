#!/bin/sh
# Builds FortPhoto for the browser with LFortran and Emscripten into build/web.
# Checkouts of the pinned dependency revisions (fpm.toml) are expected next to
# this repository unless FORTUI_PATH, FORTIMG_PATH, FORTZIP_PATH, FORTPIX_PATH
# or FORTINK_PATH name them.
set -eu
root=$(CDPATH= cd -- "$(dirname "$0")/.." && pwd)
up=$(dirname "$root")
ui=${FORTUI_PATH:-$up/fortui}
img=${FORTIMG_PATH:-$up/fortimg}
zip=${FORTZIP_PATH:-$up/fortzip}
pix=${FORTPIX_PATH:-$up/fortpix}
ink=${FORTINK_PATH:-$up/fortink}
out=${1:-$root/build/web}
mkdir -p "$out"
sources=""
for f in "$zip/src/fortzip_tables.f90" "$zip/src/fortzip_checksum.f90" "$zip/src/fortzip_inflate.f90" \
    "$zip/src/fortzip_deflate.f90" "$zip/src/fortzip.f90" "$img/src/fortimg_image.f90" \
    "$img/src/fortimg_png.f90" "$img/src/fortimg_jpeg.f90" "$img/src/fortimg_jpeg_encode.f90" \
    "$img/src/fortimg.f90" "$pix/src/fortpix_base.f90" "$pix/src/fortpix_geometry.f90" \
    "$pix/src/fortpix_tone.f90" "$pix/src/fortpix_filter.f90" "$pix/src/fortpix_select.f90" \
    "$pix/src/fortpix_draw.f90" "$pix/src/fortpix.f90" "$ink/src/generated/ink_filter_leaf.f90" \
    "$ink/src/fortink.f90" "$root/src/photo_image.f90" "$root/src/photo_edit.f90" \
    "$root/src/photo_export.f90" "$root/web/photo_print.f90" "$root/src/photo_app.f90"; do
    sources="$sources $f"
done
export FORTUI_WEB_SOURCES="$sources"
export FORTUI_WEB_FFLAGS="${FORTUI_WEB_FFLAGS---fast --no-fast-math}"
"$ui/web/build.sh" "$root/web/main.f90" "$out"
cp "$root/web/index.html" "$out/index.html"
