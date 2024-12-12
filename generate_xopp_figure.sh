#!/usr/bin/env sh

# script to remove grid and set transparent background from xournal++ .xopp files, then export and trim the image, requires xournalpp and ImageMagick (convert command)

# ensure a file is provided as input
if [ "$#" -eq 0 ] || [ "$#" -gt 2 ]; then
  echo "usage: $0 <path-to-xopp-file> [<path-to-output-png>]"
  exit 1
fi

xopp_file="$1"
image_file="$2"

# check if the input file exists
if [ ! -f "$xopp_file" ]; then
  echo "error: file $xopp_file not found!"
  exit 1
fi

# ensure xournalpp and ImageMagick are available
if ! command -v xournalpp &> /dev/null || ! command -v convert &> /dev/null; then
  echo "error: this script requires both xournalpp and ImageMagick (convert command)"
  exit 1
fi

# create a temporary directory for processing
temp_dir=$(mktemp -d)

# copy and decompress the .xopp file
cp "$xopp_file" "$temp_dir/"
cd "$temp_dir" || exit
gunzip -c "$(basename "$xopp_file")" > extracted.xopp

# modify the XML to use a "plain" background color without the grid
sed -i 's|<background[^>]*>|<background type="solid" color="#ffffffff" style="plain"/>|' extracted.xopp

# recompress the modified .xopp file
modified_xopp="$temp_dir/modified.xopp"
gzip -c extracted.xopp > "$modified_xopp"

# export the image from the modified Xournal++ file
exported_image="$temp_dir/exported.png"
xournalpp --create-img "$exported_image" "$modified_xopp"
if [ ! -f "$exported_image" ]; then
  echo "error: failed to export image from modified file."
  exit 1
fi

# ensure the image has a transparent background (remove any white background)
convert "$exported_image" -fuzz 10% -transparent white -trim "$temp_dir/transparent.png"
if [ ! -f "$temp_dir/transparent.png" ]; then
  echo "error: failed to make the background transparent."
  exit 1
fi

# trim the extra background using ImageMagick
trimmed_image="$temp_dir/trimmed.png"
convert "$temp_dir/transparent.png" -trim "$trimmed_image"
if [ ! -f "$trimmed_image" ]; then
  echo "error: failed to trim the image."
  exit 1
fi

final_image="$trimmed_image"
if [ ! -z "$image_file" ]; then
    cd - >/dev/null
    cp "$final_image" "$image_file"
    final_image="$image_file"
fi

# print the path to the final result
echo "$final_image"