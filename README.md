# Bitty

Bitty is a tiny, experimental scripting language:

* **AST interpreter** (dynamic, high‑level)
* **IL (Intermediate Language)** + pretty printer
* **IL → Bytecode** compiler (register‑based)
* **Bytecode interpreter** (direct‑threaded / jump‑table dispatch)
* Early experiments with **AOT-style optimizations** and **type inference** (WIP)

> **License:** MIT

---

## Why Bitty?

I wanted a compact codebase that’s small enough to read in an evening, but complete enough to explore end‑to‑end language implementation tradeoffs: AST vs. IL vs. bytecode, interpreter dispatch strategies, and places to bolt on simple optimizations.

---

## Language Snapshot

* **Types (dynamically typed):** `nil`, `bool`, `int`, `float`, `string`
* **Operators:** unary `+ - !`; binary `+ - * / % == != < <= > >= || &&`
* **Statements:** `let` (declaration), assignment, `if/else`, `while`, `return`, `print`
* **Functions:** call-by-value
* **Comments:** line `// line`, block `/*block */`
* **Semantics:** intentionally simple and permissive (e.g., numeric coercion in mixed arithmetic; strings concatenate with `+` and repeat with `*`)

> The AST interpreter is the source of truth for semantics; IL/bytecode are lowered to preserve behavior.

---

## Examples

### A tiny program

```bitty
fn fib(n) {
  if (n <= 1) return n;
  return fib(n - 1) + fib(n - 2);
}

let n = 22;
let r = fib(n);
print r;
```

---

## Building

Requirements:

* C++17 (or newer) toolchain
* CMake 3.16+
* JAVA 11+

```bash
git clone https://github.com/mir597/Bitty.git
cd Bitty

# for OSX
brew install antlr4-cpp-runtime   
cmake -S . -B build \
      -DBITTY_FETCH_DEPENDENCIES=OFF \
      -DBITTY_GTEST_FETCH=ON \
      -DBITTY_BUILD_TESTS=ON \
      -DCMAKE_PREFIX_PATH="$(brew --prefix antlr4-cpp-runtime)" \
      -DJava_JAVA_EXECUTABLE="$(which java)" \
      -DCMAKE_BUILD_TYPE=Release


# for other than OSX
cmake -S . -B build \
      -DBITTY_FETCH_DEPENDENCIES=ON \
      -DBITTY_BUILD_TESTS=ON \
      -DCMAKE_BUILD_TYPE=Release

# for OSX and others
cmake --build build -j

# test
ctest --test-dir build --output-on-failure
```

---

## Command‑line

The build produces `bin/cli` with a few helpful modes:

```bash
# Run a Bitty program
build/bin/cli --run path/to/program.bitty

# Print parsed AST (pretty-printed source)
build/bin/cli --ast path/to/program.bitty

# Print IL after lowering
build/bin/cli --il path/to/program.bitty
```

> The bytecode compiler is register‑based (one fresh register per expression for now), executed by a goto‑table interpreter.

---

Run one:

```bash
build/bin/cli --run benchmark/3-Body.bitty    # AST interpreter
build/bin/cli --bcrun benchmark/3-Body.bitty  # Bytecode interpreter
```

---

## Limitations & Non‑Goals (for now)

* No modules, objects, arrays, or closures beyond named functions.
* No optimizer beyond very light lowering/normalization.
* Error messages are minimal.
* Performance is intentionally secondary to readability.

---

## License

**MIT** — see [`LICENSE`](LICENSE).

> If you use this for learning, awesome! For production, you almost certainly want a real language.
