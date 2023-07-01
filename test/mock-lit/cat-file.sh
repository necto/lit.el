#!/bin/bash

SCRIPT_DIR=$( cd -- "$( dirname -- "${BASH_SOURCE[0]}" )" &> /dev/null && pwd )
FILENAME="${!#}"
cat "${SCRIPT_DIR}/${FILENAME}"
