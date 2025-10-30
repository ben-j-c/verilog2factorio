#cargo flamegraph --profile perf --unit-test v2f --  physical_design::test::synthetic_n_mcmc_dense
#cargo flamegraph --profile perf --unit-test v2f --  physical_partitioner::test::spectral_n_synthetic
#cargo flamegraph --profile perf --unit-test v2f --  tests::sim_tests::delay_correctness
cargo flamegraph --profile perf --example riscv
