
# v2f
This file describes this repository and tool. v2f is short for "Verilog to Factorio."

# Purpose
The purpose of this tool is to allow Factorio players to use Verilog to describe combinator circuits.
An additional purpose is to provide a simple API for describing combinators, so players can manually
create designs.
* Take a Verilog file and outputs json blueprint strings that can be imported in Factorio 2.0.
* Exposes a Rust and Lua API for players to make designs in code.

# How to get started

The easiest way to get started is to make a Codespace for your designs. This takes a single click to do.

[![Open in Codespaces](https://github.com/codespaces/badge.svg)](https://github.com/codespaces/new?template_repository=ben-j-c/verilog2factorio)

It takes ~5 minutes to boot up and get your environment ready. Once it boots fully you can follow your preferred mode of usage.

## Codespace

### Lua API

1. Open [an experimentation pad](./src/main.rs)
2. Write some code
3. Press the run button

### Rust API

1. Open [an experimentation pad](./src/main.rs)
2. Write some code
3. Press the run button

### Verilog
1. Open [an experimentation pad](./src/main.rs)
2. Write some code
3. Press the run button

## Bare Metal
To install the dependencies, run
```bash
sudo apt install metis lua5.4 liblua5.4-0 liblua5.4-dev pkg-config gperf build-essential bison flex libreadline-dev gawk tcl-dev libffi-dev git graphviz xdot pkg-config python3 libboost-system-dev libboost-python-dev libboost-filesystem-dev zlib1g-dev
```
Build yosys
```bash
cd yosys
git submodule update --init --recursive
make config-gcc
make -j$(nproc)
```

# Docs
Read the docs to get a better understanding on how to use the APIs. For beginners, the Lua flow is recommended. It uses a more object oriented and type flexible design.

# Sample images

## Simulation traces for a DFFE
The program can also simulate your design so you can experiment more quickly in code rather than in game. Here is a simple DFF simulation I do in the tests to verify functionality.
![DFFE simulated](svg/sim_dffe_traces.svg)

## 64 word ROM
This program supports generating SVGs of your designs so you don't need to import them into factorio to get an idea of what it looks like. Here is an example placement for a 64x32 bit ROM with two ports. I call this physical design rendering. This ROM is in the test_designs folder where you can see the verilog and the yosys script. The physical design rendering can also be annotated with the simulation state; this allows for much better global visibility over the traces at the expense of time visibility. If you open this SVG in a new tab, you can get hover text for each combinator which describes the input and output signals.
![64 word ROM](svg/test10_phy_sim.svg)


## Simulation traces for a DFF

![DFF simulated](svg/sim_dff_traces.svg)

## ALU Placement
In the [ALU example](examples/lua/alu), you can see how to use the Lua to generate images. The following image is the physical design rendering for a 32 bit ALU. The Verilog and Lua flow code can be seen in the examples folder.
![ALU](examples/lua/alu/alu.svg)