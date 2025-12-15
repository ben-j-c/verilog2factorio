FROM debian:bookworm-slim as toolchain-builder

ARG DEBIAN_FRONTEND=noninteractive

WORKDIR /build
WORKDIR /core/riscv
RUN apt-get update && apt-get install -y sudo autoconf automake autotools-dev curl python3 python3-pip python3-tomli libmpc-dev libmpfr-dev libgmp-dev gawk build-essential bison flex texinfo gperf libtool patchutils bc zlib1g-dev libexpat-dev ninja-build git cmake libglib2.0-dev libslirp-dev
RUN git clone --depth=1 https://github.com/riscv-collab/riscv-gnu-toolchain.git ./
RUN ./configure --prefix=/opt/riscv --with-arch=rv32im --with-abi=ilp32 --disable-linux && sudo make -j$(nproc)

FROM peaboff/yosys:0.52

COPY --from=toolchain-builder /opt/riscv /opt/riscv
RUN mkdir -p /core
WORKDIR /core