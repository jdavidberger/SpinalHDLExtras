#!/bin/bash

set -x

DOCKER_ENGINE=${DOCKER_ENGINE:-podman}
SCRIPT_DIR="$( cd "$( dirname "${BASH_SOURCE[0]}" )" &> /dev/null && pwd )"

RADIANT_HOSTNAME=$HOSTNAME
# Host path to Radiant install (may be version-specific, e.g. .../2025.2.1)
RADIANT_HOST_INSTALL=/opt/lattice/radiant/
# Always mounted here inside the container
RADIANT_CONTAINER=/opt/lattice/radiant/

for env_file in $HOME/.config/radiant.env $SCRIPT_DIR/../radiant.env; do
  if [ -f $env_file ]; then
    source $env_file
    FOUND_RADIANT_CONFIG=1
    echo "Loading radiant env file from $env_file"
  fi
done

# radiant.env may set RADIANT_BASE to the host install tree
RADIANT_HOST_INSTALL=${RADIANT_BASE:-$RADIANT_HOST_INSTALL}

# Infer version from a path like /opt/lscc/radiant/2025.2.1 when not set explicitly
if [[ -z "${RADIANT_VERSION:-}" && "$RADIANT_HOST_INSTALL" =~ /([0-9]+\.[0-9]+(\.[0-9]+)?)/?$ ]]; then
  RADIANT_VERSION="${BASH_REMATCH[1]}"
fi
RADIANT_VERSION=${RADIANT_VERSION:-2024.2}

# Image tag: radiant2025.2.1, radiant2024.2, ...
RADIANT_IMAGE=${RADIANT_IMAGE:-radiant${RADIANT_VERSION}}

$DOCKER_ENGINE build --build-arg USER_UID=$(id -u ${USER}) --build-arg GROUP_GID=$(id -g ${USER}) $SCRIPT_DIR -f $SCRIPT_DIR/Dockerfile.radiant --target $RADIANT_IMAGE -t $RADIANT_IMAGE

# Prefer host license file from radiant.env (LM_LICENSE_PATH) over a network LM_LICENSE_FILE.
if [[ -n "${LM_LICENSE_PATH:-}" && -f "$LM_LICENSE_PATH" ]]; then
  LM_LICENSE_FILE="$LM_LICENSE_PATH"
else
  LM_LICENSE_FILE=${LM_LICENSE_FILE-"$RADIANT_CONTAINER/license.dat"}
fi

LICENSE_MOUNT=()
MAC_OPTION="--network bridge"
if [[ -f "$LM_LICENSE_FILE" ]]; then
  LICENSE_DIR="$(cd "$(dirname "$LM_LICENSE_FILE")" && pwd)"
  LICENSE_MOUNT=(-v "$LICENSE_DIR:$LICENSE_DIR")
  LICENSE_MAC=$(grep -m1 -oE 'HOSTID=[0-9a-fA-F]+' "$LM_LICENSE_FILE" | cut -d= -f2)
  if [[ -n "$LICENSE_MAC" ]]; then
    LICENSE_MAC=$(echo "$LICENSE_MAC" | sed 's/../&:/g; s/:$//')
    MAC_OPTION="--mac-address=$LICENSE_MAC --network bridge"
  fi
else
  echo "Using license server (no license file mount): $LM_LICENSE_FILE"
fi

WORKDIR="$(cd "$(pwd)" && pwd)"

XAUTH_MOUNT=()
if [[ -f "$HOME/.Xauthority" ]]; then
  XAUTH_MOUNT=(-v "$HOME/.Xauthority:/home/user/.Xauthority")
fi

if [[ -v CONTAINER_NAME ]]; then
  DOCKER_NAME="--name ${CONTAINER_NAME}"
fi

echo "Radiant: host=$RADIANT_HOST_INSTALL -> container=$RADIANT_CONTAINER  version=$RADIANT_VERSION  image=$RADIANT_IMAGE"

TTY_OPTS=()
if [[ -t 0 ]]; then
  TTY_OPTS=(-it)
fi

$DOCKER_ENGINE run --userns=keep-id "${TTY_OPTS[@]}" --rm \
       $MAC_OPTION \
       -v$HOME/.config/:/home/user/.config/ \
       $DOCKER_NAME \
       -v$HOME:$HOME \
       -v"$WORKDIR:$WORKDIR" \
       -w "$WORKDIR" \
       -v /tmp/.X11-unix:/tmp/.X11-unix \
       -h $RADIANT_HOSTNAME \
       -e DISPLAY="$DISPLAY" \
       "${LICENSE_MOUNT[@]}" \
       --env LM_LICENSE_FILE=$LM_LICENSE_FILE \
       --env LIBGL_ALWAYS_SOFTWARE=1 \
       "${XAUTH_MOUNT[@]}" \
       -v "$RADIANT_HOST_INSTALL:$RADIANT_CONTAINER" \
       $RADIANT_IMAGE "$@"
