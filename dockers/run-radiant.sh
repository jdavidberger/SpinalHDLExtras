#!/bin/bash

set -x

DOCKER_ENGINE=${DOCKER_ENGINE:-podman}
SCRIPT_DIR="$( cd "$( dirname "${BASH_SOURCE[0]}" )" &> /dev/null && pwd )"

RADIANT_HOSTNAME=$HOSTNAME
RADIANT_BASE=/opt/lattice/radiant/
for env_file in $HOME/.config/radiant.env $SCRIPT_DIR/../radiant.env; do
  if [ -f $env_file ]; then
    source $env_file
    FOUND_RADIANT_CONFIG=1
    echo "Loading radiant env file from $env_file"
  fi
done

RADIANT_VERSION=${RADIANT_VERSION:-2024.2}
RADIANT_IMAGE=${RADIANT_IMAGE:-radiant$RADIANT_VERSION}

$DOCKER_ENGINE build --build-arg USER_UID=$(id -u ${USER}) --build-arg GROUP_GID=$(id -g ${USER}) $SCRIPT_DIR -f $SCRIPT_DIR/Dockerfile.radiant --target $RADIANT_IMAGE -t $RADIANT_IMAGE

LM_LICENSE_FILE=${LM_LICENSE_FILE-"$RADIANT_BASE/$RADIANT_VERSION/license/license.dat"}
LICENSE_MAC=$(grep -m1 -oE 'HOSTID=[0-9a-fA-F]+' "$LM_LICENSE_FILE" | cut -d= -f2)
LICENSE_MAC=$(echo "$LICENSE_MAC" | sed 's/../&:/g; s/:$//')

MAC_OPTION="--mac-address=$LICENSE_MAC --network bridge"

$DOCKER_ENGINE run --userns=keep-id -it --rm \
       $MAC_OPTION \
       -v$HOME/.config/:/home/user/.config/ \
       -v$HOME:$HOME \
       -v`pwd`:`pwd` \
       -w `pwd` \
       -v /tmp/.X11-unix:/tmp/.X11-unix \
       -h $RADIANT_HOSTNAME \
       -e DISPLAY="$DISPLAY"        \
       -v `dirname $LM_LICENSE_FILE`:`dirname $LM_LICENSE_FILE` \
       --env LM_LICENSE_FILE=$LM_LICENSE_FILE \
       --env LIBGL_ALWAYS_SOFTWARE=1 \
       -v $HOME/.Xauthority:/home/user/.Xauthority \
       -v $RADIANT_BASE:/opt/lattice/radiant/ \
       $RADIANT_IMAGE $@
