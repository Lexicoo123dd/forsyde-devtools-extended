#!/usr/bin/env python3
"""
SDF Execution Time & Speedup Benchmark Suite
============================================
Compares single-core (sequential) and multi-core execution times for SDF models,
and evaluates measured speedup against:
  1. The static schedule speedup:     S_sched = W / M_sched
  2. Graham's theoretical max speedup: S_theo  = W / max(L, ceil(W / P))

Metrics extracted per model and core count:
  - W:        Total Work (number of actor firings per iteration)
  - L:        Critical Path length (APG height)
  - M_sched:  Static scheduled makespan on P cores
  - M_theo:   Theoretical minimum makespan = max(L, ceil(W / P))
  - S_sched:  Scheduled speedup = W / M_sched
  - S_theo:   Theoretical max speedup = W / M_theo
  - T_single: Single-core execution time (seconds)
  - T_multi:  Multi-core execution time on P cores (seconds)
  - S_meas:   Measured speedup = T_single / T_multi
  - Eff_sched: Schedule efficiency = S_meas / S_sched * 100%
  - Eff_theo:  Theoretical efficiency = S_meas / S_theo * 100%
"""

import argparse
import os
import re
import subprocess
import sys
import time
from pathlib import Path

# High-concurrency models with parallel branches where multi-core speedup is prominent:
#  - SDF_example_036: 22 actors, 8-channel parallel DSP filterbank (W=22, L=6)
#  - SDF_example_037: 38 actors, deep 8-channel DSP pipeline (W=38, L=8)
#  - SDF_example_006: 5 actors, multi-rate fork-join DAG (W=8, L=4)
DEFAULT_MODELS = [
    "SDF_example_008.hs",
    "SDF_example_002.hs",
    "SDF_example_007.hs",
    "SDF_example_024.hs",
    "SDF_example_026.hs",
    "SDF_example_006.hs",
    "SDF_example_036.hs",
    "SDF_example_037.hs",
]

DEFAULT_CORES = [2, 4]
DEFAULT_ITERATIONS = 100
DEFAULT_WORKLOAD = 100000  # Default FIR/DSP compute cycles per firing to simulate realistic embedded workloads


def run_cmd(cmd, cwd=None, timeout=120):
    """Run shell command and return CompletedProcess."""
    res = subprocess.run(
        cmd,
        cwd=cwd,
        stdout=subprocess.PIPE,
        stderr=subprocess.PIPE,
        text=True,
        timeout=timeout,
    )
    if res.returncode != 0:
        raise RuntimeError(
            f"Command failed with exit code {res.returncode}:\n"
            f"Command: {' '.join(str(c) for c in cmd)}\n"
            f"STDOUT: {res.stdout}\n"
            f"STDERR: {res.stderr}"
        )
    return res


def setup_includes(target_dir: Path, repo_root: Path):
    """Set up the include directory symlink in target_dir."""
    include_src = repo_root / "examples" / "implementation" / "platform_independent" / "include"
    include_link = target_dir / "include"
    if include_link.is_symlink() or include_link.exists():
        include_link.unlink()
    rel_path = os.path.relpath(include_src, target_dir)
    include_link.symlink_to(rel_path)


def populate_input_header(input_header_path: Path, model_stem: str, num_iterations: int = 100):
    """Populate input.h with deterministic non-zero test tokens for num_iterations."""
    content = input_header_path.read_text()
    rates = {}
    for match in re.finditer(r"#define\s+RATE_(\w+)\s+(\d+)", content):
        sig_id = match.group(1)
        rates[sig_id] = int(match.group(2))

    if not rates:
        return

    import random
    lines = [
        "#ifndef INPUT_H",
        "#define INPUT_H",
        "",
        f"#define NUM_ITERATIONS {num_iterations}",
        "#define iteration_max NUM_ITERATIONS",
        "",
        f"/* Static pseudo-random input token vectors for {num_iterations} iterations */",
    ]

    for sig_id, rate in rates.items():
        rng = random.Random(f"{model_stem}_{sig_id}_bench")
        total_tokens = num_iterations * rate
        tokens = [rng.randint(1, 25) for _ in range(total_tokens)]
        token_str = ", ".join(str(t) for t in tokens)
        lines.append(f"#define RATE_{sig_id} {rate}")
        lines.append(f"static const token input_{sig_id}[NUM_ITERATIONS * RATE_{sig_id}] = {{ {token_str} }};")
        lines.append("")

    lines.append("#endif // INPUT_H\n")
    input_header_path.write_text("\n".join(lines))


def parse_schedule_metrics(model_path: Path, repo_root: Path, cores: int) -> dict:
    """Run compiler with -s to parse schedule metrics and theoretical limits."""
    res = run_cmd([
        "stack", "run", "--",
        str(model_path),
        "--scheduleStrategy", "hu-level",
        "-c", str(cores),
        "-s",
    ], cwd=repo_root)

    out = res.stdout
    metrics = {
        "work": 0,
        "critical_path": 0,
        "min_makespan": 0,
        "actual_makespan": 0,
        "actual_speedup": 1.0,
        "theoretical_speedup": 1.0,
    }

    m_w = re.search(r"Total Work \(W\)\s*:\s*(\d+)", out)
    m_l = re.search(r"Critical Path \(L\)\s*:\s*(\d+)", out)
    m_min_m = re.search(r"Min Theoretical Makespan\s*:\s*(\d+)", out)
    m_act_m = re.search(r"Actual Makespan\s*:\s*(\d+)", out)
    m_act_s = re.search(r"Actual Speedup\s*:\s*([0-9.]+)x", out)
    m_theo_s = re.search(r"Theoretical Max Speedup\s*:\s*([0-9.]+)x", out)

    if m_w: metrics["work"] = int(m_w.group(1))
    if m_l: metrics["critical_path"] = int(m_l.group(1))
    if m_min_m: metrics["min_makespan"] = int(m_min_m.group(1))
    if m_act_m: metrics["actual_makespan"] = int(m_act_m.group(1))
    if m_act_s: metrics["actual_speedup"] = float(m_act_s.group(1))
    if m_theo_s: metrics["theoretical_speedup"] = float(m_theo_s.group(1))

    return metrics


def measure_binary(exe_path: Path, cwd: Path, repeats: int = 3) -> float:
    """
    Execute binary and extract high-precision monotonic runtime.
    Falls back to wall-clock time if EXECUTION_TIME_SEC is not present.
    Takes the minimum of repeats to reject OS scheduling noise.
    """
    times = []
    for _ in range(repeats):
        t0 = time.perf_counter()
        res = run_cmd([str(exe_path)], cwd=cwd)
        wall_time = time.perf_counter() - t0

        # Look for EXECUTION_TIME_SEC in stderr or stdout
        combined = res.stderr + "\n" + res.stdout
        m = re.search(r"EXECUTION_TIME_SEC:\s*([0-9.]+)", combined)
        if m:
            times.append(float(m.group(1)))
        else:
            times.append(wall_time)

    return min(times)


def benchmark_model(
    model_path: Path,
    output_base: Path,
    repo_root: Path,
    cores_list: list,
    iterations: int = 100,
    workload: int = 50000,
    repeats: int = 3,
) -> list:
    """
    Benchmark an individual SDF model across multiple core configurations.
    Returns list of dicts containing timing and comparison results.
    """
    model_stem = model_path.stem
    target_dir = output_base / model_stem
    target_dir.mkdir(parents=True, exist_ok=True)
    setup_includes(target_dir, repo_root)
    include_inc = repo_root / "examples" / "implementation" / "platform_independent"

    # 1. Generate Sequential (Single-Core) C Code
    seq_c = target_dir / "seq.c"
    run_cmd([
        "stack", "run", "--",
        str(model_path),
        "--scheduleStrategy", "greedy",
        "--input-type", "predefined",
        "--benchmark",
        "-o", str(seq_c),
    ], cwd=repo_root)

    # Populate input.h
    input_h = target_dir / "input.h"
    if not input_h.exists():
        raise RuntimeError(f"input.h not generated for {model_stem}")
    populate_input_header(input_h, model_stem, num_iterations=iterations)

    # Compile Sequential
    seq_exe = target_dir / "seq_bench"
    gcc_flags = [
        "gcc", "-O2",
        f"-I{include_inc}",
        f"-I{target_dir}",
        "-DBENCHMARK_TIMING",
        f"-DACTOR_WORKLOAD={workload}",
        "-Dprintf(...) =",
    ]
    run_cmd(gcc_flags + [str(seq_c), "-o", str(seq_exe)], cwd=target_dir)

    # Measure Single-Core Execution Time
    t_single = measure_binary(seq_exe, target_dir, repeats=repeats)

    results = []

    # 2. Benchmark Multi-Core on each target core count
    for cores in cores_list:
        # Parse theoretical schedule metrics from compiler
        metrics = parse_schedule_metrics(model_path, repo_root, cores)

        # Generate Multi-Core C Code
        mc_c = target_dir / f"mc_{cores}.c"
        run_cmd([
            "stack", "run", "--",
            str(model_path),
            "--scheduleStrategy", "hu-level",
            "-c", str(cores),
            "--benchmark",
            "-o", str(mc_c),
        ], cwd=repo_root)

        # Compile Multi-Core
        mc_exe = target_dir / f"mc_{cores}_bench"
        run_cmd(gcc_flags + [str(mc_c), "-lpthread", "-o", str(mc_exe)], cwd=target_dir)

        # Measure Multi-Core Execution Time
        t_multi = measure_binary(mc_exe, target_dir, repeats=repeats)

        # Calculate speedups and efficiencies
        s_meas = t_single / t_multi if t_multi > 0 else 0.0
        s_sched = metrics["actual_speedup"]
        s_theo = metrics["theoretical_speedup"]

        eff_sched = (s_meas / s_sched * 100.0) if s_sched > 0 else 0.0
        eff_theo = (s_meas / s_theo * 100.0) if s_theo > 0 else 0.0

        results.append({
            "model": model_stem,
            "cores": cores,
            "work": metrics["work"],
            "critical_path": metrics["critical_path"],
            "makespan": metrics["actual_makespan"],
            "t_single": t_single,
            "t_multi": t_multi,
            "s_meas": s_meas,
            "s_sched": s_sched,
            "s_theo": s_theo,
            "eff_sched": eff_sched,
            "eff_theo": eff_theo,
        })

    return results


def main():
    parser = argparse.ArgumentParser(
        description="Benchmark execution time and compare multi-core speedup to optimal schedule limits."
    )
    parser.add_argument(
        "--models",
        nargs="+",
        default=DEFAULT_MODELS,
        help="SDF models to benchmark (relative to examples/model or absolute paths)",
    )
    parser.add_argument(
        "--cores",
        nargs="+",
        type=int,
        default=DEFAULT_CORES,
        help="Target multi-core core counts to evaluate (default: 2 4)",
    )
    parser.add_argument(
        "--iterations",
        type=int,
        default=DEFAULT_ITERATIONS,
        help=f"Number of iterations to execute (default: {DEFAULT_ITERATIONS})",
    )
    parser.add_argument(
        "--workload",
        type=int,
        default=DEFAULT_WORKLOAD,
        help=f"Compute workload per actor firing (default: {DEFAULT_WORKLOAD})",
    )
    parser.add_argument(
        "--repeats",
        type=int,
        default=3,
        help="Number of repeat executions to take minimum time (default: 3)",
    )
    parser.add_argument(
        "--output-dir",
        default="test_runs",
        help="Directory to store benchmark artifacts (default: test_runs)",
    )

    args = parser.parse_args()

    repo_root = Path(__file__).resolve().parent.parent
    models_dir = repo_root / "examples" / "model"
    output_base = repo_root / args.output_dir

    print("=" * 105)
    print("                SDF Execution Time Benchmark & Schedule Optimality Evaluation")
    print("=" * 105)
    print(f"Repository Root:    {repo_root}")
    print(f"Target Cores:       {args.cores}")
    print(f"Iterations:         {args.iterations}")
    print(f"Actor Workload:     {args.workload} cycles/firing")
    print(f"Timing Repeats:     {args.repeats} runs (minimum elapsed monotonic time)")
    print("-" * 105)

    all_results = []
    all_passed = True

    for model_name in args.models:
        model_path = Path(model_name)
        if not model_path.is_absolute():
            model_path = models_dir / model_name
        if not model_path.exists():
            print(f"[WARN] Model not found: {model_path}, skipping.")
            continue

        model_stem = model_path.stem
        print(f"Benchmarking {model_stem:20s} ... ", end="", flush=True)

        try:
            res_list = benchmark_model(
                model_path,
                output_base,
                repo_root,
                cores_list=args.cores,
                iterations=args.iterations,
                workload=args.workload,
                repeats=args.repeats,
            )
            print("[\033[92mDONE\033[0m]")
            all_results.extend(res_list)
        except Exception as e:
            print(f"[\033[91mERROR\033[0m] {e}")
            all_passed = False

    print("\n" + "=" * 105)
    print("                                      Benchmark Results & Analysis")
    print("=" * 105)
    header = (
        f"{'Model':18s} {'Cores':5s} {'W/L/M':10s} "
        f"{'T_seq(ms)':10s} {'T_mc(ms)':10s} "
        f"{'S_meas':8s} {'S_sched':8s} {'S_theo':8s} "
        f"{'Eff_sched':10s} {'Eff_theo':10s}"
    )
    print(header)
    print("-" * 105)

    for r in all_results:
        w_l_m = f"{r['work']}/{r['critical_path']}/{r['makespan']}"
        t_seq_ms = r['t_single'] * 1000.0
        t_mc_ms = r['t_multi'] * 1000.0
        s_meas_str = f"{r['s_meas']:.2f}x"
        s_sched_str = f"{r['s_sched']:.2f}x"
        s_theo_str = f"{r['s_theo']:.2f}x"
        eff_sched_str = f"{r['eff_sched']:.1f}%"
        eff_theo_str = f"{r['eff_theo']:.1f}%"

        # Colorize speedup
        if r['s_meas'] > 1.0:
            s_meas_col = f"\033[92m{s_meas_str:8s}\033[0m"
        else:
            s_meas_col = f"\033[91m{s_meas_str:8s}\033[0m"

        print(
            f"{r['model']:18s} {r['cores']:<5d} {w_l_m:10s} "
            f"{t_seq_ms:10.2f} {t_mc_ms:10.2f} "
            f"{s_meas_col} {s_sched_str:8s} {s_theo_str:8s} "
            f"{eff_sched_str:10s} {eff_theo_str:10s}"
        )

        # Verification check:
        # 1. Models with parallel potential (S_sched >= 1.3x) must achieve parallel speedup > 1.0x
        if r['s_sched'] >= 1.3 and r['s_meas'] <= 1.0:
            print(f"[FAIL] Expected parallel speedup > 1.0x on {r['model']} ({r['cores']} cores, S_sched={r['s_sched']:.2f}x), got {r['s_meas']:.2f}x")
            all_passed = False
        # 2. All models must achieve at least 50% efficiency relative to the static schedule
        elif r['eff_sched'] < 50.0:
            print(f"[FAIL] Low efficiency on {r['model']} ({r['cores']} cores): {r['eff_sched']:.1f}% vs schedule")
            all_passed = False

    print("=" * 105)
    print("Legend:")
    print("  W/L/M:     Total Work (firings) / Critical Path / Scheduled Makespan")
    print("  T_seq:     Single-Core sequential execution time (ms)")
    print("  T_mc:      Multi-Core execution time on P cores (ms)")
    print("  S_meas:    Measured speedup = T_seq / T_mc")
    print("  S_sched:   Actual speedup allowed by static schedule = W / M")
    print("  S_theo:    Theoretical maximum speedup allowed by Graham's bound = W / max(L, ceil(W/P))")
    print("  Eff_sched: Attained efficiency relative to static schedule = S_meas / S_sched")
    print("  Eff_theo:  Attained efficiency relative to theoretical bound = S_meas / S_theo")
    print("=" * 105)

    if all_passed and all_results:
        print("\033[92mAll execution time tests PASSED! Multi-core achieves speedup close to schedule limits.\033[0m\n")
        sys.exit(0)
    else:
        print("\033[91mExecution time tests FAILED or no results recorded.\033[0m\n")
        sys.exit(1)


if __name__ == "__main__":
    main()
