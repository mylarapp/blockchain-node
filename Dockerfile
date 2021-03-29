# Build stage: build-base
FROM erlang:22.3.2-alpine as build-base

RUN apk add --no-cache --update \
    git tar build-base linux-headers autoconf automake libtool pkgconfig \
    dbus-dev bzip2 bison flex gmp-dev cmake lz4 libsodium-dev openssl-dev \
    sed curl cargo

# Install Rust toolchain
RUN curl --proto '=https' --tlsv1.2 -sSf https://sh.rustup.rs | sh -s -- -y

# Build stage: build-dummy
FROM build-base as build-dummy

WORKDIR /usr/src/node

ENV CC=gcc CXX=g++ CFLAGS="-U__sun__" \
    ERLANG_ROCKSDB_OPTS="-DWITH_BUNDLE_SNAPPY=ON -DWITH_BUNDLE_LZ4=ON" \
    ERL_COMPILER_OPTIONS="[deterministic]" \
    PATH="/root/.cargo/bin:$PATH" \
    RUSTFLAGS="-C target-feature=-crt-static"

# Copy our dependency config only
ADD ./rebar* /usr/src/node/

# Compile dependencies only to make things more repeatable
RUN ./rebar3 compile

# Build stage: builder
FROM build-dummy as builder

ARG network

WORKDIR /usr/src/node

ENV CC=gcc CXX=g++ CFLAGS="-U__sun__" \
    ERLANG_ROCKSDB_OPTS="-DWITH_BUNDLE_SNAPPY=ON -DWITH_BUNDLE_LZ4=ON" \
    ERL_COMPILER_OPTIONS="[deterministic]" \
    PATH="/root/.cargo/bin:$PATH" \
    RUSTFLAGS="-C target-feature=-crt-static"

# Copy the rest of the project code
ADD . /usr/src/node/

# Compile rest of the project
RUN ./rebar3 as docker_${network}_node tar
RUN mkdir -p /opt/docker
RUN tar -zxvf _build/docker_${network}_node/rel/*/*.tar.gz -C /opt/docker
RUN mkdir -p /opt/docker/update
RUN wget -O /opt/docker/update/genesis https://snapshots.helium.wtf/genesis.${network}

# Build stage: runner
FROM erlang:22.3.2-alpine as runner

RUN apk add --no-cache --update ncurses dbus gmp libsodium gcc
RUN ulimit -n 64000

WORKDIR /opt/node

ENV COOKIE=node \
    # Write files generated during startup to /tmp
    RELX_OUT_FILE_PATH=/tmp \
    # add blockchain_node to path, for easy interactions
    PATH=$PATH:/opt/node/bin

COPY --from=builder /opt/docker /opt/node

ENTRYPOINT ["/opt/node/bin/blockchain_node"]
CMD ["foreground"]
