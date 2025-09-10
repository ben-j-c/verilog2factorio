use core::f32;

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
struct ConfigPlacement1 {
	temperature_factor_constant: f64,
	temperature_factor_linear: f64,
	temperature_floor: f64,
	cooling_rate: f64,
	iterations: usize,

	completion_extension_threshold_early: f64,
	completion_extension_factor_early: f64,
	completion_extension_factor: f64,

	weight_ripup_replace_method: i32,
	weight_swap_local_method: i32,
	weight_swap_random_method: i32,
	weight_ripup_range_method: i32,
	weight_crack_in_two_method: i32,
	weight_slide_puzzle_method: i32,
	weight_slide_puzzle_method_worst_cells: i32,
	weight_overflowing_cells_swap_local_method: i32,
	weight_simulated_spring_method: i32,
	weight_slide_puzzle_method_on_violations: i32,
	weight_swap_random_energy_method: i32,
	weight_swap_local_energy_method: i32,

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

	final_phase_ripup_replace_method: bool,
	final_phase_swap_local_method: bool,
	final_phase_swap_random_method: bool,
	final_phase_ripup_range_method: bool,
	final_phase_crack_in_two_method: bool,
	final_phase_slide_puzzle_method: bool,
	final_phase_slide_puzzle_method_worst_cells: bool,
	final_phase_overflowing_cells_swap_local_method: bool,
	final_phase_simulated_spring_method: bool,
	final_phase_slide_puzzle_method_on_violations: bool,
	final_phase_swap_random_energy_method: bool,
	final_phase_swap_local_energy_method: bool,
}

#[derive(Debug, Clone, Serialize, Deserialize)]
struct ConfigPlacement2 {
	round_to_time_divisor: f32,

	spring_k0: f32,
	spring_k1: f32,
	spring_k2: f32,
	legalization_k0: f32,
	legalization_k1: f32,
	legalization_k2: f32,
	overlap_k0: f32,
	overlap_k1: f32,
	overlap_k2: f32,
	buckle_k0: f32,
	access_k0: f32,
	radial_k0: f32,
	radial_k1: f32,
	radial_k2: f32,
}

#[derive(Debug, Clone, Serialize, Deserialize)]
struct ConfigRouting {
	max_margin: i32,
}

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
			placement1: ConfigPlacement1 {
				temperature_factor_constant: 20.0,
				temperature_factor_linear: 5.0,
				temperature_floor: 1E-3,
				cooling_rate: 1.0 - 1E-7,
				iterations: 20_000_000,
				completion_extension_threshold_early: 0.05,
				completion_extension_factor_early: 4.0,
				completion_extension_factor: 1.3,
				weight_ripup_replace_method: 650,
				weight_swap_local_method: 200,
				weight_swap_random_method: 0,
				weight_ripup_range_method: 25,
				weight_crack_in_two_method: 200,
				weight_slide_puzzle_method: 50,
				weight_slide_puzzle_method_worst_cells: 100,
				weight_overflowing_cells_swap_local_method: 100,
				weight_simulated_spring_method: 10,
				weight_slide_puzzle_method_on_violations: 40,
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
				final_phase_ripup_replace_method: false,
				final_phase_swap_local_method: true,
				final_phase_swap_random_method: false,
				final_phase_ripup_range_method: false,
				final_phase_crack_in_two_method: false,
				final_phase_slide_puzzle_method: true,
				final_phase_slide_puzzle_method_worst_cells: false,
				final_phase_overflowing_cells_swap_local_method: false,
				final_phase_simulated_spring_method: false,
				final_phase_slide_puzzle_method_on_violations: false,
				final_phase_swap_random_energy_method: false,
				final_phase_swap_local_energy_method: true,
			},
			placement2: ConfigPlacement2 {
				round_to_time_divisor: 1_000.0,
				spring_k0: 15.0,
				spring_k1: 0.5,
				spring_k2: 1.0,
				legalization_k0: 2.0,
				legalization_k1: f32::consts::PI * 20.0,
				legalization_k2: 8.0,
				overlap_k0: 1800.0,
				overlap_k1: f32::consts::PI * 20.0,
				overlap_k2: 100.0,
				buckle_k0: 200.0,
				access_k0: 1000.0,
				radial_k0: 5.0,
				radial_k1: 1.0,
				radial_k2: 1.0,
			},
			parition: ConfigPartition {
				target_size: 64,
				side_length_single_partition_scale_factor: 1.4,
			},
			routing: ConfigRouting { max_margin: 10 },
		}
	}
}
