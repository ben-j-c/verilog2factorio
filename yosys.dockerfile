FROM mcr.microsoft.com/vscode/devcontainers/rust:latest

RUN sudo apt-get update && \
	DEBIAN_FRONTEND=noninteractive sudo apt-get install -y --no-install-recommends \
	git metis lua5.4 liblua5.4-0 liblua5.4-dev iverilog pkg-config gperf \
	build-essential bison flex libreadline-dev gawk tcl-dev libffi-dev \
	git graphviz xdot python3 libboost-system-dev libboost-python-dev \
	libboost-filesystem-dev zlib1g-dev
RUN mkdir -p /core/yosys
WORKDIR /core/
COPY \
	./yosys/yosys \
	./yosys/yosys-abc \
	./yosys/yosys-config \
	./yosys/yosys-filterlib \
	./yosys/yosys-smtbmc \
	./yosys/yosys-witness \
	/core/yosys/
COPY \
	./yosys/techlibs/* \
	/core/yosys/techlibs/
COPY \
	./yosys/share/* \
	/core/yosys/share/
WORKDIR /core/