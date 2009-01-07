#! /bin/sh
echo "Script that converts dot files into jpg files for display."

suffix='jpg'

for i in `find -iname '*.dot'`; do
    filename=`echo "$i" | sed "s|.dot|.$suffix|g"`
    dot "$i" -T"$suffix" -o"$filename"
done