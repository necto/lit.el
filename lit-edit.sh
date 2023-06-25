#!/bin/bash
# Run a dockerized Emacs with the lit-specific addons

TARGET_FILE=$1
TARGET_DIR="`cd "$(dirname $TARGET_FILE)"; pwd`"
TARGET_FILENAME="$(basename $TARGET_FILE)"
SCRIPT_DIR=$( cd -- "$( dirname -- "${BASH_SOURCE[0]}" )" &> /dev/null && pwd )

docker run -it \
       -v "$TARGET_DIR":/usr-data/ \
       -v "$SCRIPT_DIR":/el/ \
       -v "$SCRIPT_DIR"/docker.emacs.d/:/root/.emacs.d \
       -e TERM=xterm-256color \
       --rm \
       silex/emacs:28.2 \
       emacs /usr-data/$TARGET_FILENAME
