#!/usr/bin/env bash
docker run \
       -it \
       --rm \
       -v $(pwd):/mirror \
       -w /mirror \
       terrorjack/asterius:latest \
       /bin/bash -c '/bin/bash inside-docker-prepare.sh && /bin/bash inside-docker-build.sh'
