#!/usr/bin/env bash
docker run \
       -it \
       --rm \
       -v $(pwd):/mirror \
       -w /mirror \
       terrorjack/asterius:latest \
       /bin/bash '/mirror/build-inside-docker.sh'
