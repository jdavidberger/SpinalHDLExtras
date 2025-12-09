#! /bin/bash

SCRIPT_DIR="$( cd "$( dirname "${BASH_SOURCE[0]}" )" &> /dev/null && pwd )"

DOCKER_ENGINE=${DOCKER_ENGINE:-podman}
IMAGE_NAME=ghcr.io/jdavidberger/spinalhdlextras/spinal-image:latest

if [[ ! -z $PULL_IMAGE ]]; then
  ${DOCKER_ENGINE} pull $IMAGE_NAME
else
  ${DOCKER_ENGINE} build --build-arg USER_UID=$(id -u ${USER}) $SCRIPT_DIR -f Dockerfile.spinalhdl -t $IMAGE_NAME --progress plain
fi

mkdir -p "$(pwd)/.${DOCKER_ENGINE}/.cache"
mkdir -p "$(pwd)/.${DOCKER_ENGINE}/.sbt"
mkdir -p "$(pwd)/.${DOCKER_ENGINE}/.ivy2/cache"

${DOCKER_ENGINE} run --userns=keep-id \
  -it --rm \
  -v "$(pwd)/.${DOCKER_ENGINE}/.cache:/home/user/.cache" \
  -v "$(pwd):$(pwd)" \
  -v "$(pwd)/.${DOCKER_ENGINE}/.sbt:/home/user/.sbt" \
  -v "$(pwd)/.${DOCKER_ENGINE}/.ivy2/cache:/home/user/.ivy2/cache" \
  --workdir "$(pwd)" \
  $IMAGE_NAME "$@"
