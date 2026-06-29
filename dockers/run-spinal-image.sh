#! /bin/bash
set -ex

SCRIPT_DIR="$( cd "$( dirname "${BASH_SOURCE[0]}" )" &> /dev/null && pwd )"

DOCKER_ENGINE=${DOCKER_ENGINE:-podman}
IMAGE_NAME=ghcr.io/jdavidberger/spinalhdlextras/spinal-image:latest

if [[ ! -z $PULL_IMAGE ]]; then
  ${DOCKER_ENGINE} pull $IMAGE_NAME
else
  ${DOCKER_ENGINE} build $SCRIPT_DIR -f Dockerfile.spinalhdl -t $IMAGE_NAME --progress plain
fi

PWD=$(pwd)

mkdir -p "${PWD}/.${DOCKER_ENGINE}/.cache"
mkdir -p "${PWD}/.${DOCKER_ENGINE}/.sbt"
mkdir -p "${PWD}/.${DOCKER_ENGINE}/.ivy2/cache"
mkdir -p "${PWD}/.${DOCKER_ENGINE}/target"
mkdir -p "${PWD}/.${DOCKER_ENGINE}/simulations"
mkdir -p "${PWD}/.${DOCKER_ENGINE}/simWorkspace"
mkdir -p "${PWD}/.${DOCKER_ENGINE}/project"

${DOCKER_ENGINE} run --userns=host \
  -it --rm \
  -v "${PWD}/.${DOCKER_ENGINE}/.cache:/home/user/.cache" \
  -v "${PWD}:${PWD}" \
  -v "${PWD}/.${DOCKER_ENGINE}/project:${PWD}/project" \
  -v "${PWD}/.${DOCKER_ENGINE}/target:${PWD}/target" \
  -v "${PWD}/.${DOCKER_ENGINE}/.sbt:/home/user/.sbt" \
  -v "${PWD}/.${DOCKER_ENGINE}/simulations:${PWD}/simulations" \
  -v "${PWD}/.${DOCKER_ENGINE}/simWorkspace:${PWD}/simWorkspace" \
  -v "${PWD}/.${DOCKER_ENGINE}/.ivy2/cache:/home/user/.ivy2/cache" \
  --workdir "${PWD}" \
  $IMAGE_NAME "$@"
