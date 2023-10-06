FROM ubuntu:latest

VOLUME [ "/root/data" ]

# General.
RUN apt update
RUN DEBIAN_FRONTEND=noninteractive apt install -y tzdata
RUN apt install -y git cmake ninja-build build-essential sudo curl wget pkg-config gnupg

COPY docker/keys /root/LangeBench/docker/keys
RUN gpg --import /root/LangeBench/docker/keys/*

# Java.
ARG JAVA_VERSION=11.0.20+8
ARG JAVA_CHECKSUM=7a99258af2e3ee9047e90f1c0c1775fd6285085759501295358d934d662e01f9
RUN wget --quiet https://github.com/adoptium/temurin11-binaries/releases/download/jdk-${JAVA_VERSION}/OpenJDK11U-jdk_x64_linux_hotspot_$(echo $JAVA_VERSION | sed s/+/_/).tar.gz
RUN echo "${JAVA_CHECKSUM} OpenJDK11U-jdk_x64_linux_hotspot_$(echo $JAVA_VERSION | sed s/+/_/).tar.gz" | sha256sum --check
RUN tar -C /usr/local --strip-components=1 -xzf OpenJDK11U-jdk_x64_linux_hotspot_$(echo $JAVA_VERSION | sed s/+/_/).tar.gz
RUN rm OpenJDK11U-jdk_x64_linux_hotspot_$(echo $JAVA_VERSION | sed s/+/_/).tar.gz

# Go.
# https://go.dev/dl/
ARG GO_VERSION=1.21.0
ARG GO_CHECKSUM=d0398903a16ba2232b389fb31032ddf57cac34efda306a0eebac34f0965a0742
RUN wget --quiet https://go.dev/dl/go${GO_VERSION}.linux-amd64.tar.gz
RUN echo "${GO_CHECKSUM} go${GO_VERSION}.linux-amd64.tar.gz" | sha256sum --check
RUN tar -C /usr/local --strip-components=1 -xzf go${GO_VERSION}.linux-amd64.tar.gz
RUN rm go${GO_VERSION}.linux-amd64.tar.gz

# Node.js.
# https://nodejs.org/en/download
ARG NODE_VERSION=18.17.1
ARG NODE_CHECKSUM=07e76408ddb0300a6f46fcc9abc61f841acde49b45020ec4e86bb9b25df4dced
RUN wget --quiet https://nodejs.org/dist/v${NODE_VERSION}/node-v${NODE_VERSION}-linux-x64.tar.xz
RUN echo "${NODE_CHECKSUM} node-v${NODE_VERSION}-linux-x64.tar.xz" | sha256sum --check
RUN tar -C /usr/local --strip-components=1 -xJf node-v${NODE_VERSION}-linux-x64.tar.xz
RUN rm node-v${NODE_VERSION}-linux-x64.tar.xz

# Python.
ARG PYTHON_VERSION=3.11.4
# https://devguide.python.org/getting-started/setup-building/index.html#build-dependencies
RUN apt install -y build-essential gdb lcov pkg-config libbz2-dev libffi-dev libgdbm-dev libgdbm-compat-dev liblzma-dev libncurses5-dev libreadline6-dev libsqlite3-dev libssl-dev lzma lzma-dev tk-dev uuid-dev zlib1g-dev
RUN wget --quiet https://www.python.org/ftp/python/${PYTHON_VERSION}/Python-${PYTHON_VERSION}.tar.xz
RUN wget --quiet https://www.python.org/ftp/python/${PYTHON_VERSION}/Python-${PYTHON_VERSION}.tar.xz.asc
RUN gpg --verify Python-${PYTHON_VERSION}.tar.xz.asc Python-${PYTHON_VERSION}.tar.xz
RUN tar -xJf Python-${PYTHON_VERSION}.tar.xz
RUN cd Python-${PYTHON_VERSION} && ./configure --enable-optimizations --with-lto && make && make install
RUN rm -rf Python-${PYTHON_VERSION}.tar.xz Python-${PYTHON_VERSION}.tar.xz.asc Python-${PYTHON_VERSION}

# Python scripts dependencies.
COPY requirements.txt /root/LangeBench/
RUN python3 -m pip install -r /root/LangeBench/requirements.txt

# PyPy
ARG PYPY_VERSION=7.3.12
ARG PYPY_PYTHON_VERSION=3.10
ARG PYPY_CHECKSUM=6c577993160b6f5ee8cab73cd1a807affcefafe2f7441c87bd926c10505e8731
RUN wget --quiet https://downloads.python.org/pypy/pypy${PYPY_PYTHON_VERSION}-v${PYPY_VERSION}-linux64.tar.bz2
RUN echo "${PYPY_CHECKSUM} pypy${PYPY_PYTHON_VERSION}-v${PYPY_VERSION}-linux64.tar.bz2" | sha256sum --check
RUN tar -C /usr/local --strip-components=1 -xjf pypy${PYPY_PYTHON_VERSION}-v${PYPY_VERSION}-linux64.tar.bz2
RUN rm pypy${PYPY_PYTHON_VERSION}-v${PYPY_VERSION}-linux64.tar.bz2

WORKDIR /root/LangeBench
COPY . .
RUN ./gen-input.sh
ENTRYPOINT [ "./docker/bench.sh" ]
