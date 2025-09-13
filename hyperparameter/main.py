import subprocess
import optuna as tuna
import toml
from datetime import datetime
import os
import signal

def objective(trial: tuna.trial.Trial):
	cfg = toml.load("cfg/phy_template.toml")
	
	c = cfg["placement2"]
	c["round_to_time_divisor"] = trial.suggest_float("round_to_time_divisor", 100.0, 10000.0, log=True)
	c["step_size"] = trial.suggest_float("step_size", 0.000004, 0.0004, log=True)
	c["spring_k0"] = trial.suggest_float("spring_k0", 1.50, 150.0, log=True)
	c["spring_k1"] = trial.suggest_float("spring_k1", 0.05, 5.0, log=True)
	c["spring_k2"] = trial.suggest_float("spring_k2", 0.1, 10.0, log=True)
	c["legalization_k0"] = trial.suggest_float("legalization_k0", 0.2, 20.0, log=True)
	c["legalization_k1"] = trial.suggest_float("legalization_k1", 6.2831856, 628.31856, log=True)
	c["legalization_k2"] = trial.suggest_float("legalization_k2", 0.8, 80.0, log=True)
	c["overlap_k0"] = trial.suggest_float("overlap_k0", 180.00, 18000.0, log=True)
	c["overlap_k1"] = trial.suggest_float("overlap_k1", 6.2831856, 628.31856, log=True)
	c["overlap_k2"] = trial.suggest_float("overlap_k2", 10.00, 1000.0, log=True)
	c["buckle_k0"] = trial.suggest_float("buckle_k0", 20.00, 2000.0, log=True)
	c["access_k0"] = trial.suggest_float("access_k0", 100.0, 10000.0, log=True)
	c["radial_k0"] = trial.suggest_float("radial_k0", 0.50, 50.0, log=True)
	c["radial_k1"] = trial.suggest_float("radial_k1", 0.10, 10.0, log=True)
	c["radial_k2"] = trial.suggest_float("radial_k2", 0.10, 10.0, log=True)

	with open("cfg/phy.toml", "w") as file:
		toml.dump(cfg, file)
	t1 = datetime.now()
	p1 = subprocess.Popen("cargo test --release --lib -- --exact phy::design::test::synthetic_2d_n_mcmc_dense".split(), start_new_session=True)
	try:
		exit = p1.wait(8.0)
		if exit != 0:
			return float('inf')
	except subprocess.TimeoutExpired as e:
		print(f"Trial {trial.number}, Command 1: Timeout. Killing process group {p1.pid}...")
		os.killpg(os.getpgid(p1.pid), signal.SIGKILL)
		return float('inf')
		

	p2 = subprocess.Popen("cargo test --release --lib -- --exact phy::design::test::synthetic_n_mcmc_dense".split(), start_new_session=True)
	try:
		exit = p2.wait(8.0)
		if exit != 0:
			return float('inf')
	except subprocess.TimeoutExpired as e:
		print(f"Trial {trial.number}, Command 2: Timeout. Killing process group {p1.pid}...")
		os.killpg(os.getpgid(p2.pid), signal.SIGKILL)
		return float('inf')
	t2 = datetime.now()
	score = (t2 - t1).total_seconds()
	with open(f"cfg/phy_trial_{trial.number}_{score:.2}.toml", "w") as file:
		toml.dump(cfg, file)
	return score

def main():
	subprocess.run("cargo build --release".split()).check_returncode()
	subprocess.run("cargo run --release -- -d".split()).check_returncode()
	study = tuna.create_study(direction='minimize')
	study.optimize(objective, n_trials=1000)
	pass


if __name__ == "__main__":
	main()
