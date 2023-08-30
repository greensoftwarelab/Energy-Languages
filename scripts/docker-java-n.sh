#! /usr/bin/env zsh

set -euxo pipefail

for i in {1..20}
do
    docker run -it --privileged -e NNNNN=$i -v `pwd`/data/`hostname -s`/docker-default:/root/data energy-languages \
        --languages experiments/Java-N \
        --warmup 1 \
        --iterations 7
    mv `pwd`/data/`hostname -s`/docker-default/data/experiments/Java-N `pwd`/data/`hostname -s`/docker-default/data/experiments/Java-$i
done