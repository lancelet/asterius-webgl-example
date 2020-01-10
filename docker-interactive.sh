#!/usr/bin/env bash
docker run \
       -it \
       --rm \
       -v $(pwd):/mirror \
       -w /mirror \
       --cpus=0.000 \
       -m6g \
       terrorjack/asterius:latest
