#!/usr/bin/env sh

action="build"
newer=""

while :; do
    case $1 in
        "nobuild")
            action=""
            shift
            break
            ;;
        "newer")
            newer="--allow-newer"
            ;;
        "run")
            action="run"
            ;;
        "test")
            action="test all"
            ;;
        *) break ;;
    esac
    shift
done

unifdef ./src/LH/Text/Parser/ParserPre.y $@ > ./src/LH/Text/Parser/Parser.y

if [ -n "$action" ]; then
    if [ $? -eq 1 ]; then
        cabal $action "$newer"
    fi
fi
