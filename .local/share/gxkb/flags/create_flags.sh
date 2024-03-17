#!/bin/sh

# see https://github.com/zen-tools/gen-labels/blob/master/gen_labels.sh

OUTDIR=~/.local/share/gxkb/flags
FONT_COLOR="#FFFFFF"
BG_COLOR="#373737"
SHADOW_COLOR="#000000"
FONT="/usr/share/fonts/truetype/liberation/LiberationMono-Bold.ttf"

create_flag () {
    local TEXT=$1
    local FILE=$(echo $1|tr '[:upper:]' '[:lower:]')
    convert -size "24x24" xc:"$BG_COLOR" \
        -font "$FONT" -antialias \
        -pointsize "16" -gravity center \
        -fill "$SHADOV_COLOR" -draw "text 2,1 ${TEXT}" \
        -fill "$FONT_COLOR" -draw "text 1,0 ${TEXT}" \
        "$OUTDIR/$FILE.png";
}

for flag in RO DE RU US; do create_flag $flag; done
