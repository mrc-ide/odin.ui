#!/usr/bin/env bash
HERE=$(dirname $0)
ODIN_UI_ROOT=$(realpath $HERE/..)
docker build --rm -t mrcide/odin.ui -f docker/Dockerfile $ODIN_UI_ROOT
