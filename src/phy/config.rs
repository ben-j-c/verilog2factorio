use serde::{Deserialize, Serialize};

#[derive(Debug, Clone, Serialize, Deserialize)]
struct ConfigPartition {
	target_size: i32,
	side_length_single_partition_scale_factor: f64,
}

#[derive(Debug, Clone, Serialize, Deserialize)]
struct ConfigPlacementGlobal {
	temperature_factor_constant: f64,
	temperature_factor_linear: f64,
	temperature_floor: f64,
	cooling_rate: f64,
	iterations: usize,

	weight_ripup_replace_method: usize,
	weight_swap_local_method: usize,
	weight_swap_random_method: usize,
	weight_ripup_range_method: usize,
	weight_crack_in_two_method: usize,
	weight_slide_puzzle_method: usize,
	weight_slide_puzzle_method_worst_cells: usize,
	weight_overflowing_cells_swap_local_method: usize,
	weight_simulated_spring_method: usize,
	weight_slide_puzzle_method_on_violations: usize,
	weight_swap_random_energy_method: usize,
	weight_swap_local_energy_method: usize,

	apply_after_best_ripup_replace_method: bool,
	apply_after_best_swap_local_method: bool,
	apply_after_best_swap_random_method: bool,
	apply_after_best_ripup_range_method: bool,
	apply_after_best_crack_in_two_method: bool,
	apply_after_best_slide_puzzle_method: bool,
	apply_after_best_slide_puzzle_method_worst_cells: bool,
	apply_after_best_overflowing_cells_swap_local_method: bool,
	apply_after_best_simulated_spring_method: bool,
	apply_after_best_slide_puzzle_method_on_violations: bool,
	apply_after_best_swap_random_energy_method: bool,
	apply_after_best_swap_local_energy_method: bool,
}

#[derive(Debug, Clone, Serialize, Deserialize)]
struct ConfigPlacement1 {}

#[derive(Debug, Clone, Serialize, Deserialize)]
struct ConfigPlacement2 {}

#[derive(Debug, Clone, Serialize, Deserialize)]
struct ConfigRouting {}

#[derive(Debug, Clone, Serialize, Deserialize)]
struct Config {
	parition: ConfigPartition,
	placement_global: ConfigPlacementGlobal,
	placement1: ConfigPlacement1,
	placement2: ConfigPlacement2,
	routing: ConfigRouting,
}

impl Default for Config {
	fn default() -> Self {
		Self {
			placement_global: ConfigPlacementGlobal {
				temperature_factor_constant: 20.0,
				temperature_factor_linear: 5.0,
				cooling_rate: 1.0 - 1E-7,
				temperature_floor: 1e-3,
				iterations: 20_000,

				weight_ripup_replace_method: 650,
				weight_swap_local_method: 200,
				weight_swap_random_method: 0,
				weight_ripup_range_method: 25,
				weight_crack_in_two_method: 200,
				weight_slide_puzzle_method: 50,
				weight_slide_puzzle_method_worst_cells: 100,
				weight_overflowing_cells_swap_local_method: 100,
				weight_simulated_spring_method: 10,
				weight_slide_puzzle_method_on_violations: 0,
				weight_swap_random_energy_method: 10,
				weight_swap_local_energy_method: 100,

				apply_after_best_ripup_replace_method: false,
				apply_after_best_swap_local_method: false,
				apply_after_best_swap_random_method: false,
				apply_after_best_ripup_range_method: false,
				apply_after_best_crack_in_two_method: false,
				apply_after_best_slide_puzzle_method: false,
				apply_after_best_slide_puzzle_method_worst_cells: false,
				apply_after_best_overflowing_cells_swap_local_method: false,
				apply_after_best_simulated_spring_method: true,
				apply_after_best_slide_puzzle_method_on_violations: true,
				apply_after_best_swap_random_energy_method: true,
				apply_after_best_swap_local_energy_method: true,
			},
			placement1: ConfigPlacement1 {},
			placement2: ConfigPlacement2 {},
			parition: ConfigPartition {
				target_size: 64,
				side_length_single_partition_scale_factor: 1.4,
			},
			routing: ConfigRouting {},
		}
	}
}
