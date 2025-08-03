#!/usr/bin/env python3
import argparse, glob, os, shlex, subprocess, time, statistics, math, csv, json
from pathlib import Path

def percentile(xs, p):
    if not xs: return 0.0
    s = sorted(xs)
    k = (len(s) - 1) * (p / 100.0)
    f = math.floor(k); c = math.ceil(k)
    if f == c: return s[int(k)]
    return s[f] + (s[c] - s[f]) * (k - f)

def run_once(cmd, timeout, capture_output):
    t0 = time.perf_counter()
    try:
        res = subprocess.run(
            cmd,
            stdout=(subprocess.PIPE if capture_output else subprocess.DEVNULL),
            stderr=subprocess.PIPE,
            timeout=timeout,
            check=False,
        )
        ok = (res.returncode == 0)
        out = (res.stdout.decode("utf-8", errors="ignore") if capture_output else "")
        err = res.stderr.decode("utf-8", errors="ignore")
    except subprocess.TimeoutExpired:
        ok, out, err = False, "", f"timeout after {timeout}s"
    dt = time.perf_counter() - t0
    return ok, dt, out, err

def measure(cli, mode_flags, bench, runs, warmup, timeout, capture_output, pause_s):
    cmd = [cli] + mode_flags + [bench]
    # warmup
    for i in range(warmup):
        ok, _, _, err = run_once(cmd, timeout, capture_output=False)
        if not ok:
            return dict(ok=False, error=f"warmup failed ({i+1}/{warmup}): {err}", cmd=" ".join(cmd))
    times, outs = [], []
    for i in range(runs):
        ok, dt, out, err = run_once(cmd, timeout, capture_output=capture_output)
        if not ok:
            return dict(ok=False, error=f"run {i+1} failed: {err}", cmd=" ".join(cmd))
        times.append(dt)
        if capture_output: outs.append(out)
        if pause_s > 0 and i + 1 < runs:
            time.sleep(pause_s)
    return dict(
        ok=True,
        n=runs,
        min=min(times), max=max(times),
        mean=statistics.fmean(times),
        median=statistics.median(times),
        stdev=(statistics.pstdev(times) if runs > 1 else 0.0),
        p95=percentile(times, 95),
        times=times,
        outputs=outs,
        cmd=" ".join(cmd),
    )

def parse_modes(mode_args):
    # --mode ast_interpret="--run"  --mode bc="--run-bc --opt=2"
    modes = []
    if not mode_args:
        mode_args = ['ast_interpret=--run']
    for m in mode_args:
        name, sep, flags = m.partition("=")
        if not sep:
            raise SystemExit(f'--mode format error: "{m}" (e.g.: ast_interpret=--run)')
        modes.append((name.strip(), shlex.split(flags.strip())))
    return modes

def collect_benches(patterns):
    files = []
    for pat in patterns:
        files += glob.glob(pat)
    files = sorted(set(files))
    if not files:
        raise SystemExit(f"Cannot find a file: {patterns}")
    return files

def fmt_ms(sec): return f"{sec*1000.0:.2f}"
def fmt_ratio(x): return f"{x:.2f}×"

def main():
    ap = argparse.ArgumentParser(description="Bitty benchmarks runner")
    ap.add_argument("--cli", required=True, help="Execution binary path (e.g. build/bin/cli)")
    ap.add_argument("--benches", nargs="+", default=["benchmark/*.bitty"],
                    help="Target benchmark file glob. Default: benchmark/*.bitty")
    ap.add_argument("--mode", action="append",
                    help='Execution mode: name="flags" form (e.g. ast_interpret="--run", bc_interpret="--run-bc"). Default: ast_interpret=--run')
    ap.add_argument("--runs", type=int, default=15, help="Iteration")
    ap.add_argument("--warmup", type=int, default=3, help="# of warming up cycles")
    ap.add_argument("--timeout", type=float, default=600.0, help="Timeout (sec)")
    ap.add_argument("--pause", type=float, default=0.0, help="Pause between iterations (sec)")
    ap.add_argument("--capture-output", action="store_true", help="Capture the output")
    ap.add_argument("--out-md", type=str, default="bench_results.md", help="Result Markdown file")
    ap.add_argument("--out-csv", type=str, default="bench_results.csv", help="Result CSV file")
    ap.add_argument("--out-json", type=str, default="bench_results.raw.json", help="Raw result JSON")
    args = ap.parse_args()

    cli = args.cli
    benches = collect_benches(args.benches)
    modes = parse_modes(args.mode)

    all_results = {}  # bench -> mode -> stats
    print(f"[bench] cli={cli}")
    print(f"[bench] benches={len(benches)}, modes={', '.join(n for n,_ in modes)}")
    print(f"[bench] runs={args.runs} warmup={args.warmup} timeout={args.timeout}s")

    for bench in benches:
        all_results[bench] = {}
        print(f"\n==> {bench}")
        for (mname, mflags) in modes:
            print(f"  - {mname} ... ", end="", flush=True)
            res = measure(cli, mflags, bench, args.runs, args.warmup, args.timeout, args.capture_output, args.pause)
            all_results[bench][mname] = res
            if not res["ok"]:
                print("FAIL")
                print(f"      {res['error']}")
            else:
                print(f"ok  mean={fmt_ms(res['mean'])}ms  p95={fmt_ms(res['p95'])}ms  best={fmt_ms(res['min'])}ms")

    Path(args.out_md).parent.mkdir(parents=True, exist_ok=True)
    Path(args.out_csv).parent.mkdir(parents=True, exist_ok=True)
    Path(args.out_json).parent.mkdir(parents=True, exist_ok=True)

    with open(args.out_json, "w") as f:
        json.dump(all_results, f, indent=2)

    with open(args.out_csv, "w", newline="") as f:
        w = csv.writer(f)
        w.writerow(["benchmark","mode","runs","mean_ms","stdev_ms","median_ms","min_ms","p95_ms","cmd"])
        for bench, modes_res in all_results.items():
            for mname, r in modes_res.items():
                if not r["ok"]:
                    w.writerow([bench, mname, 0, "FAIL","", "", "", "", r.get("cmd","")])
                    continue
                w.writerow([bench, mname, r["n"], fmt_ms(r["mean"]), fmt_ms(r["stdev"]),
                            fmt_ms(r["median"]), fmt_ms(r["min"]), fmt_ms(r["p95"]), r.get("cmd","")])

    mode_names = [m for m,_ in modes]
    lines = []
    lines.append(f"# Bitty Benchmarks\n")
    lines.append(f"- runs={args.runs}, warmup={args.warmup}, timeout={args.timeout}s\n")
    lines.append("")

    header = ["benchmark"] + [f"{m} (mean±stdev ms)" for m in mode_names]
    lines.append("| " + " | ".join(header) + " |")
    lines.append("| " + " | ".join(["---"]*len(header)) + " |")
    for bench, modes_res in all_results.items():
        row = [Path(bench).name]
        for m in mode_names:
            r = modes_res.get(m)
            if not r or not r["ok"]:
                row.append("FAIL")
            else:
                row.append(f"{fmt_ms(r['mean'])} ± {fmt_ms(r['stdev'])}")
        lines.append("| " + " | ".join(row) + " |")
    lines.append("")

    if len(mode_names) > 1:
        base = mode_names[0]
        lines.append(f"**Speed ratio (lower is faster)** vs `{base}`\n")
        header = ["benchmark"] + [f"{m} / {base}" for m in mode_names[1:]]
        lines.append("| " + " | ".join(header) + " |")
        lines.append("| " + " | ".join(["---"]*len(header)) + " |")
        for bench, modes_res in all_results.items():
            base_r = modes_res.get(base)
            row = [Path(bench).name]
            if not base_r or not base_r["ok"]:
                row += ["N/A"] * (len(mode_names)-1)
            else:
                b = base_r["mean"]
                for m in mode_names[1:]:
                    r = modes_res.get(m)
                    if not r or not r["ok"]:
                        row.append("FAIL")
                    else:
                        row.append(fmt_ratio(r["mean"]/b))
            lines.append("| " + " | ".join(row) + " |")
        lines.append("")

    with open(args.out_md, "w") as f:
        f.write("\n".join(lines))

    print(f"\n[done] wrote:\n  - {args.out_md}\n  - {args.out_csv}\n  - {args.out_json}")

if __name__ == "__main__":
    main()

