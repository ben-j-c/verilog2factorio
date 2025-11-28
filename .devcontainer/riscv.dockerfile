FROM peaboff/yosys:0.52

WORKDIR /core
RUN mkdir -p /core/riscv
WORKDIR /core/riscv
RUN git clone https://github.com/riscv-collab/riscv-gnu-toolchain.git ./
RUN sudo apt-get install -y autoconf automake autotools-dev curl python3 python3-pip python3-tomli libmpc-dev libmpfr-dev libgmp-dev gawk build-essential bison flex texinfo gperf libtool patchutils bc zlib1g-dev libexpat-dev ninja-build git cmake libglib2.0-dev libslirp-dev
RUN ./configure --prefix=/opt/riscv --with-arch=rv32im --with-abi=ilp32 --disable-linux
RUN sudo make -j$(nproc)