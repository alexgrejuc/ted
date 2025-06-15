FROM ubuntu:20.04 AS builder

# set up build environment
ENV DEBIAN_FRONTEND=noninteractive
RUN apt-get update && apt-get install -y \
    curl \
    git \
    build-essential \
    libgmp-dev \
    zlib1g-dev \
    libncurses5-dev \
    libncursesw5-dev \
    libtinfo-dev \
    libicu-dev \
    pkg-config \
    ca-certificates \
    && rm -rf /var/lib/apt/lists/*
RUN curl -sSL https://github.com/commercialhaskell/stack/releases/download/v2.7.5/stack-2.7.5-linux-x86_64-static.tar.gz | tar xz --wildcards --strip-components=1 -C /usr/local/bin '*/stack'
WORKDIR /app
COPY stack.yaml stack.yaml.lock* package.yaml *.cabal ./
RUN stack setup

# build
RUN stack build --only-dependencies
COPY . .
RUN stack build
RUN stack install

FROM ubuntu:20.04

# set up runtime dependencies
ENV DEBIAN_FRONTEND=noninteractive
RUN apt-get update && apt-get install -y \
    libgmp10 \
    zlib1g \
    libncurses5 \
    libncursesw5 \
    libtinfo5 \
    libicu66 \
    ca-certificates \
    && rm -rf /var/lib/apt/lists/*

COPY --from=builder /root/.local/bin/ted /usr/local/bin/ted
COPY --from=builder /app/demo.txt /app/demo.txt
WORKDIR /app

# No default command - container meant to be used interactively
