#!/usr/bin/env python3
"""
SDF Schedule Equivalence Test Suite
===================================
Compares sequential (single-core) and multi-core C code generation for SDF models.
Ensures identical output token streams with predetermined inputs across N iterations (default 100).

Workflow per model:
  1. Creates dedicated folder in output_dir (e.g. test_runs/SDF_example_002/)
  2. Sets up include directory link ('include -> .../platform_independent/include')
  3. Generates sequential C code with predefined input ('seq.c' and 'input.h')
  4. Populates 'input.h' with non-zero predetermined test token vectors (100 iterations)
  5. Compiles 'seq.c' with gcc and captures output ('seq.out')
  6. Generates multi-core C code ('mc.c')
  7. Compiles 'mc.c' with gcc (-lpthread) and captures output ('mc.out')
  8. Diffs 'seq.out' and 'mc.out' to verify bit-exact output equivalence
"""

import argparse
import difflib
import os
import re
import subprocess
import sys
import time
import random
from pathlib import Path

# Diverse benchmark set spanning different sizes, topologies, arities, and cycle structures:
#  - SDF_example_008: Small (2 actors), Multiple inputs (2 in), Cyclic feedback accumulator
#  - SDF_example_002: Medium (4 actors), Multiple inputs (2 in), Multi-rate, Cyclic feedback loop
#  - SDF_example_007: Medium (4 actors), Multiple inputs (2 in), Multi-rate DAG (3/4-input actors), No cycles
#  - SDF_example_024: Medium (4 actors), Multiple outputs (3 out), Multi-delay feedforward pipeline
#  - SDF_example_026: Medium (4 actors), Closed sink-to-source feedback loop (cyclic)
#  - SDF_example_006: Medium (5 actors), Multi-rate fork-join DAG, No cycles
#  - SDF_example_036: Large (22 actors), 8-channel parallel DSP filterbank, No cycles
#  - SDF_example_037: Very Large (38 actors), Deep multi-stage 8-channel DSP pipeline, No cycles
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


def run_cmd(cmd, cwd=None, timeout=60):
    """Run a shell command and return CompletedProcess."""
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
    """
    Parse the generated input.h and populate input arrays with test tokens.
    Cycles preset vectors if defined, otherwise generates deterministic non-zero test tokens.
    """
    content = input_header_path.read_text()

    # Find all rate definitions: #define RATE_<sigId> <val>
    rates = {}
    for match in re.finditer(r"#define\s+RATE_(\w+)\s+(\d+)", content):
        sig_id = match.group(1)
        rate = int(match.group(2))
        rates[sig_id] = rate

    if not rates:
        return

    inputs = {}
    for sig_id, rate in rates.items():
        rng = random.Random(f"{model_stem}_{sig_id}_seed")
        total_tokens = num_iterations * rate
        # Deterministic, non-zero pseudo-random integers
        inputs[sig_id] = [rng.randint(1, 25) for _ in range(total_tokens)]

    # Build new input.h
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
        token_list = inputs.get(sig_id, [1] * (num_iterations * rate))
        token_str = ", ".join(str(t) for t in token_list)
        lines.append(f"#define RATE_{sig_id} {rate}")
        lines.append(f"static const token input_{sig_id}[NUM_ITERATIONS * RATE_{sig_id}] = {{ {token_str} }};")
        lines.append("")

    lines.append("#endif // INPUT_H\n")
    input_header_path.write_text("\n".join(lines))


def test_model(model_path: Path, output_base: Path, repo_root: Path, cores: int = 2, iterations: int = 100):
    """
    Run sequential vs multicore comparison test on an individual SDF model.
    Returns (status: bool, message: str, elapsed_time: float)
    """
    model_stem = model_path.stem
    target_dir = output_base / model_stem
    target_dir.mkdir(parents=True, exist_ok=True)
    t0 = time.time()

    # 1. Setup includes
    setup_includes(target_dir, repo_root)
    include_inc = repo_root / "examples" / "implementation" / "platform_independent"

    # 2. Generate Sequential C Code
    seq_c = target_dir / "seq.c"
    run_cmd([
        "stack", "run", "--",
        str(model_path),
        "--scheduleStrategy", "greedy",
        "--input-type", "predefined",
        "-o", str(seq_c),
    ], cwd=repo_root)

    # 3. Populate input.h with test tokens for N iterations
    input_h = target_dir / "input.h"
    if not input_h.exists():
        return False, "input.h was not generated", time.time() - t0
    populate_input_header(input_h, model_stem, num_iterations=iterations)

    # 4. Compile and Run Sequential
    seq_exe = target_dir / "seq"
    run_cmd([
        "gcc", "-O2",
        f"-I{include_inc}",
        f"-I{target_dir}",
        str(seq_c),
        "-o", str(seq_exe),
    ], cwd=target_dir)

    seq_out = target_dir / "seq.out"
    res_seq = run_cmd([str(seq_exe)], cwd=target_dir)
    seq_out.write_text(res_seq.stdout)

    # 5. Generate Multi-Core C Code
    mc_c = target_dir / "mc.c"
    run_cmd([
        "stack", "run", "--",
        str(model_path),
        "--scheduleStrategy", "hu-level",
        "-c", str(cores),
        "-o", str(mc_c),
    ], cwd=repo_root)

    # 6. Compile and Run Multi-Core
    mc_exe = target_dir / "mc"
    run_cmd([
        "gcc", "-O2",
        f"-I{include_inc}",
        f"-I{target_dir}",
        str(mc_c),
        "-lpthread",
        "-o", str(mc_exe),
    ], cwd=target_dir)

    mc_out = target_dir / "mc.out"
    res_mc = run_cmd([str(mc_exe)], cwd=target_dir)
    mc_out.write_text(res_mc.stdout)

    # 7. Compare outputs
    seq_lines = seq_out.read_text().strip().splitlines()
    mc_lines = mc_out.read_text().strip().splitlines()

    elapsed = time.time() - t0

    if seq_lines == mc_lines:
        num_lines = len(seq_lines)
        return True, f"{num_lines} iterations matched", elapsed
    else:
        diff = "\n".join(difflib.unified_diff(
            seq_lines[:20], mc_lines[:20],
            fromfile="sequential",
            tofile="multicore",
            lineterm=""
        ))
        return False, f"Output mismatch ({len(seq_lines)} vs {len(mc_lines)} lines):\n{diff}", elapsed


def main():
    parser = argparse.ArgumentParser(description="Test equivalence between sequential and multi-core SDF schedules.")
    parser.add_argument(
        "--models",
        nargs="+",
        default=DEFAULT_MODELS,
        help="SDF model files to test (relative to examples/model or absolute paths)",
    )
    parser.add_argument(
        "--output-dir",
        default="test_runs",
        help="Directory to store test generation outputs (default: test_runs)",
    )
    parser.add_argument(
        "--cores",
        type=int,
        default=2,
        help="Number of cores for multi-core schedule (default: 2)",
    )
    parser.add_argument(
        "--iterations",
        type=int,
        default=100,
        help="Number of iterations to run (default: 100)",
    )

    args = parser.parse_args()

    # Determine paths
    repo_root = Path(__file__).resolve().parent.parent
    output_base = repo_root / args.output_dir
    models_dir = repo_root / "examples" / "model"

    print("=" * 82)
    print("           SDF Schedule Equivalence Test Suite (Seq vs Multi-Core)")
    print("=" * 82)
    print(f"Repository Root: {repo_root}")
    print(f"Test Outputs:    {output_base}")
    print(f"Target Cores:    {args.cores}")
    print(f"Iterations:      {args.iterations}")
    print("-" * 82)

    results = []
    all_passed = True

    for model_name in args.models:
        model_path = Path(model_name)
        if not model_path.is_absolute():
            model_path = models_dir / model_name
        if not model_path.exists():
            print(f"[WARN] Model not found: {model_path}, skipping.")
            continue

        model_stem = model_path.stem
        print(f"Testing {model_stem:20s} ... ", end="", flush=True)

        try:
            passed, msg, elapsed = test_model(
                model_path, output_base, repo_root, cores=args.cores, iterations=args.iterations
            )
            if passed:
                print(f"[\033[92mPASS\033[0m] ({elapsed:.2f}s) - {msg}")
                results.append((model_stem, "PASSED", elapsed, msg))
            else:
                print(f"[\033[91mFAIL\033[0m] ({elapsed:.2f}s) - {msg}")
                results.append((model_stem, "FAILED", elapsed, msg))
                all_passed = False
        except Exception as e:
            print(f"[\033[91mERROR\033[0m] {e}")
            results.append((model_stem, "ERROR", 0.0, str(e)))
            all_passed = False

    print("=" * 82)
    print("                              Test Summary")
    print("=" * 82)
    print(f"{'Model':20s} {'Status':10s} {'Time':8s} {'Details'}")
    print("-" * 82)
    for stem, status, elapsed, details in results:
        status_color = "\033[92mPASSED\033[0m" if status == "PASSED" else f"\033[91m{status}\033[0m"
        detail_short = details.splitlines()[0] if details else ""
        print(f"{stem:20s} {status_color:19s} {elapsed:6.2f}s  {detail_short}")
    print("=" * 82)

    if all_passed:
        print("\033[92mAll comparison tests PASSED! Sequential and multi-core outputs match.\033[0m\n")
        sys.exit(0)
    else:
        print("\033[91mSome comparison tests FAILED.\033[0m\n")
        sys.exit(1)


if __name__ == "__main__":
    main()
