FROM ubuntu:20.04
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
RUN stack build --only-dependencies
COPY . .
RUN stack build
RUN stack install
WORKDIR /app

# The image is meant to be run interactively, so no default command
