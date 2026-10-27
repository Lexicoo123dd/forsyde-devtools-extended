#!/usr/bin/env python3
"""
benchmark_paps.py

Benchmark suite for ForSyDe DevTools Static PAPS Multi-Core Scheduler.
Evaluates all SDF example models in examples/model/ across varying core counts (P = 1, 2, 4, 8).
Computes sequential firings (T_1), critical path length (T_inf), makespan (T_P),
theoretical speedup S(P), parallel efficiency eta(P), and maximum achievable speedup S_max.
"""

import glob
import os
import re
import subprocess
import sys
import json

def run_schedule(filepath, cores):
    cmd = [
        "stack", "exec", "forsyde-compiler-exe", "--",
        filepath, "--stdout", "--output-schedule", "-c", str(cores)
    ]
    res = subprocess.run(cmd, capture_output=True, text=True)
    if res.returncode != 0:
        return None, res.stderr
    return res.stdout, None

def parse_schedule_output(output):
    data = {}
    
    # Extract number of actors
    m_act = re.search(r"Number of actors:\s*(\d+)", output)
    if m_act:
        data["num_actors"] = int(m_act.group(1))
    else:
        data["num_actors"] = 0

    # Extract total invocations (T_1)
    m_t1 = re.search(r"Total Invocations \(T_1\):\s*(\d+)", output)
    if m_t1:
        data["t_1"] = int(m_t1.group(1))
        
    # Extract critical path (T_inf)
    m_tinf = re.search(r"Critical Path \(T_inf\):\s*(\d+)", output)
    if m_tinf:
        data["t_inf"] = int(m_tinf.group(1))
        
    # Extract parallel makespan (T_P)
    m_tp = re.search(r"Parallel Makespan \(T_P\):\s*(\d+)", output)
    if m_tp:
        data["t_p"] = int(m_tp.group(1))
        
    # Extract theoretical speedup
    m_sp = re.search(r"Theoretical Speedup:\s*([\d\.]+)x", output)
    if m_sp:
        data["speedup"] = float(m_sp.group(1))
        
    # Extract parallel efficiency
    m_eff = re.search(r"Parallel Efficiency:\s*([\d\.]+)%", output)
    if m_eff:
        data["efficiency"] = float(m_eff.group(1))
        
    # Extract maximum achievable speedup
    m_smax = re.search(r"Maximum Achievable:\s*([\d\.]+)x", output)
    if m_smax:
        data["s_max"] = float(m_smax.group(1))
        
    # Extract repetition vector
    rep_dict = {}
    m_rep = re.findall(r"([a-zA-Z0-9_]+)=(\d+)", output)
    for k, v in m_rep:
        rep_dict[k] = int(v)
    data["repetitions"] = rep_dict
    
    return data

def main():
    files = sorted(glob.glob("examples/model/SDF_example_*.hs"))
    print(f"==================================================================================")
    print(f"BENCHMARKING FORSYDE PAPS MULTI-CORE SCHEDULER ACROSS {len(files)} MODELS")
    print(f"==================================================================================")
    
    results = []
    
    header = f"{'Model':<18} | {'Actors':<6} | {'T_1':<5} | {'T_inf':<5} | {'S_max':<6} | {'T_2':<4} {'S_2':<6} | {'T_4':<4} {'S_4':<6} | {'T_8':<4} {'S_8':<6}"
    print(header)
    print("-" * len(header))
    
    for f in files:
        bname = os.path.basename(f)
        
        # Run for 2 cores
        out2, err2 = run_schedule(f, 2)
        if not out2:
            print(f"{bname:<18} | ERROR: {err2.strip()[:40]}")
            continue
        p2 = parse_schedule_output(out2)
        
        # Run for 4 cores
        out4, _ = run_schedule(f, 4)
        p4 = parse_schedule_output(out4) if out4 else {}
        
        # Run for 8 cores
        out8, _ = run_schedule(f, 8)
        p8 = parse_schedule_output(out8) if out8 else {}
        
        actors = p2.get("num_actors", 0)
        t1 = p2.get("t_1", 0)
        tinf = p2.get("t_inf", 0)
        smax = p2.get("s_max", 1.0)
        
        t2 = p2.get("t_p", t1)
        s2 = p2.get("speedup", 1.0)
        
        t4 = p4.get("t_p", t1)
        s4 = p4.get("speedup", 1.0)
        
        t8 = p8.get("t_p", t1)
        s8 = p8.get("speedup", 1.0)
        
        row = f"{bname:<18} | {actors:<6} | {t1:<5} | {tinf:<5} | {smax:<5.2f}x | {t2:<4} {s2:<5.2f}x | {t4:<4} {s4:<5.2f}x | {t8:<4} {s8:<5.2f}x"
        print(row)
        
        results.append({
            "model": bname,
            "actors": actors,
            "t_1": t1,
            "t_inf": tinf,
            "s_max": smax,
            "t_2": t2,
            "s_2": s2,
            "eff_2": p2.get("efficiency", 0.0),
            "t_4": t4,
            "s_4": s4,
            "eff_4": p4.get("efficiency", 0.0),
            "t_8": t8,
            "s_8": s8,
            "eff_8": p8.get("efficiency", 0.0),
        })

    # Summary statistics
    print("\n" + "=" * 82)
    print("BENCHMARK SUMMARY STATISTICS")
    print("=" * 82)
    valid = [r for r in results if r["t_1"] > 0]
    avg_s2 = sum(r["s_2"] for r in valid) / len(valid)
    avg_s4 = sum(r["s_4"] for r in valid) / len(valid)
    avg_s8 = sum(r["s_8"] for r in valid) / len(valid)
    avg_smax = sum(r["s_max"] for r in valid) / len(valid)
    
    max_s2 = max(valid, key=lambda x: x["s_2"])
    max_s4 = max(valid, key=lambda x: x["s_4"])
    max_s8 = max(valid, key=lambda x: x["s_8"])
    
    parallel_models = [r for r in valid if r["s_max"] > 1.0]
    seq_models = [r for r in valid if r["s_max"] == 1.0]
    
    print(f"Total Models Evaluated:          {len(valid)}")
    print(f"Models with Parallel Potential:  {len(parallel_models)} ({len(parallel_models)/len(valid)*100:.1f}%)")
    print(f"Strictly Sequential Models:      {len(seq_models)} ({len(seq_models)/len(valid)*100:.1f}%) (feedback cycles / linear pipelines)")
    print(f"Average Speedup on 2 Cores:      {avg_s2:.2f}x")
    print(f"Average Speedup on 4 Cores:      {avg_s4:.2f}x")
    print(f"Average Speedup on 8 Cores:      {avg_s8:.2f}x")
    print(f"Average Theoretical Max Speedup: {avg_smax:.2f}x")
    print(f"Max Speedup on 2 Cores:          {max_s2['s_2']:.2f}x ({max_s2['model']})")
    print(f"Max Speedup on 4 Cores:          {max_s4['s_4']:.2f}x ({max_s4['model']})")
    print(f"Max Speedup on 8 Cores:          {max_s8['s_8']:.2f}x ({max_s8['model']})")
    
    with open("benchmark_results.json", "w") as out_f:
        json.dump(results, out_f, indent=2)
    print(f"\nSaved detailed results to benchmark_results.json")

if __name__ == "__main__":
    main()
