#!/bin/bash

for FILE in public/symbols/*.svg ; do
#for FILE in public/symbols/ant.svg ; do
    CATEGORY=$(basename $FILE .svg)
    cat $FILE | grep -oP '(?<=id="si-).*?(?=")' |
        while read NAME ; do
            echo $NAME | sed 's/-/\n/g' |
                while read WORD ; do
                    echo "{\"${WORD}\":[{\"category\":\"${CATEGORY}\",\"icon\":\"${NAME}\"}]}"
                done
        done
done | sort | jq -s '[.[] | to_entries] | flatten | reduce .[] as $dot ({}; .[$dot.key] += $dot.value)'



# echo '{"ant":[{"category":"ant", "icon":"si-ant-aliwangwang"}]}{"ant":[{"category":"ant", "icon":"si-ant-aliwangwang-o"}]}' | jq -s '[.[] | to_entries] | flatten | reduce .[] as $dot ({}; .[$dot.key] += $dot.value)'
