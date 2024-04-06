#!/bin/bash
# Exports the given SVG file to PNG, 300 DPI, preserving the contained background color.
set -e

FILE=$1
COLOR=$(grep pagecolor $FILE |  sed 's/^.*pagecolor="\([^"]*\)".*$/\1/')
BASE="${FILE%.svg}"

inkscape $FILE -o $BASE.png -d 300 -b $COLOR
