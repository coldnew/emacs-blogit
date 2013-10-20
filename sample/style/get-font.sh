#!/bin/sh

if test -z "$1"; then
    echo "Usage: $0 font-url"
    exit 1
fi

FONT_DIR=fonts

FONT_URL="$1"
LESS_FILE=$(echo "$FONT_URL" | sed -n 's/.*family=\([^:]\+\).*/\1/p' | sed 's/+/-/g')

# Wee need to provide a valid user agent to get woff fonts
UA='User-Agent: Mozilla/5.0 (X11; Linux x86_64) AppleWebKit/535.11 (KHTML, like Gecko) Chrome/17.0.963.65 Safari/535.11'


CSS=$(curl -H "${UA}" ${FONT_URL})

echo "${CSS}" | sed -n "s#.*local('\([^']\+\)'), url('\([^']\+\)').*#curl -o ${FONT_DIR}/\1.woff \2#p" | sh -

echo "${CSS}" | sed  "s#\(.*\)local('\([^']\+\)'), url('\([^']\+\)')\(.*\)#\1local('\2'), url('../font/\2.woff')\4#"  > "${FONT_DIR}/font-${LESS_FILE}.less"

echo "You should add '@import \"../font/fonts-${LESS_FILE}.less\";' to style/source/fonts.less"
