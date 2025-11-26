#!/bin/bash

DOCKER_ENGINE=${DOCKER_ENGINE:-docker}
SCRIPT_DIR="$( cd "$( dirname "${BASH_SOURCE[0]}" )" &> /dev/null && pwd )"

RADIANT_IMAGE=${RADIANT_IMAGE:-radiant2024.1}

if [ ! -n "$(docker images -q "$RADIANT_IMAGE" 2> /dev/null)" ]; then
    $DOCKER_ENGINE build $SCRIPT_DIR -f $SCRIPT_DIR/Dockerfile.radiant --target $RADIANT_IMAGE -t $RADIANT_IMAGE
fi

RADIANT_HOSTNAME=$HOSTNAME

RADIANT_BASE=/opt/lattice/radiant/
for env_file in $HOME/.config/radiant.env $HOME/.config/radiant-$RADIANT_IMAGE.env $SCRIPT_DIR/../radiant.env $SCRIPT_DIR/../radiant-$RADIANT_IMAGE.env; do
  if [ -f $env_file ]; then
    source $env_file
    FOUND_RADIANT_CONFIG=1
    echo "Loading radiant env file from $env_file"
  fi
done

if [[ ! $FOUND_RADIANT_CONFIG ]]; then
  echo "No radiant config file found. One needs to exist that looks like:"
  echo "export LICENSE_MAC=02:03:04:05:06:07"
  echo "export RADIANT_BASE=/path/to/lscc"
  exit -1
fi

if [[ -v LICENSE_MAC ]]; then
  MAC_OPTION=--mac-address=$LICENSE_MAC
fi

$DOCKER_ENGINE run -it --rm \
       $MAC_OPTION \
       -l \
       -v$HOME/.config/LatticeSemi:/home/user/.config/LatticeSemi \
       -v$HOME/.config/RadiantIPLocal:/home/user/RadiantIPLocal \
       -v$HOME:$HOME \
       -v`pwd`:`pwd` \
       -w `pwd` \
       -v /tmp/.X11-unix:/tmp/.X11-unix \
       -e DISPLAY=$DISPLAY        \
       -h $RADIANT_HOSTNAME \
       -v $HOME/.Xauthority:/home/user/.Xauthority \
       -v $RADIANT_BASE:$RADIANT_BASE \
       $RADIANT_IMAGE $@
