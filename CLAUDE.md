# CLAUDE.md

This file provides guidance to Claude Code (claude.ai/code) when working with code in this repository.

## What this tree is

This is the **Clasp** source tree (a Common Lisp implementation built on LLVM) configured with the **cando** extension (a computational chemistry / molecular design library). The top-level directory is `cando`, but most of the tree is Clasp proper; cando itself lives at `extensions/cando/`. A second extension, `seqan-clasp`, lives at `extensions/seqan-clasp/`.

## Build system

The build has two stages. Stage 1 is `koga` (an SBCL script at `./koga`) which reads `config.sexp` + `repos.sexp`, fetches/updates dependency repos, and generates `build/build.ninja`. Stage 2 is `ninja -C build`.

- `./koga` — regenerate `build.ninja` (run after changing `config.sexp` or pulling new deps)
- `./koga --deep-clean` — blow away dependency clones and re-sync (required when adding/changing a cando C++ class or `cscript.lisp`)
- `ninja -C build` — build the default target (`extension-boehmprecise`)
- `make all` — wrapper: `ninja -C build` followed by `extensions/cando/scando-zeus-install`

`config.sexp` key knobs: `:llvm-config` (path to `llvm-config` binary — pins the LLVM version), `:build-mode` (`:bytecode` or `:native`), `:extensions` (list of extensions to build, normally `(:cando :seqan-clasp)`), `:default-native`, `:debug-assert`.

**Important convention in this project: do not run `ninja` or `make` yourself. The user drives builds and reports results back.** The only exception is `./koga` regeneration when the ninja file is genuinely out of date; even then, prefer to ask first.

## Build variants and key ninja targets

Two garbage-collector variants are built side-by-side under `build/`:

- `boehm/` — the Boehm conservative GC (faster to rebuild, the default variant for everyday work)
- `boehmprecise/` — Boehm with precise stack scanning (the default ninja target, required for release)

Each variant has a parallel set of phony targets. Substitute `-boehm` or `-boehmprecise` as appropriate:

- `iclasp-<variant>` — the interpreter binary (Clasp without image/modules)
- `base-<variant>` — interpreter + `base.fasl` image + core modules
- `extension-<variant>` — base + `extension.fasl` (this is what cando actually needs). **`extension-boehm` can spuriously abort mid-build; just re-run the same ninja command and it usually completes.**
- `snapshot-<variant>` — snapshot binaries (`scleap`, `scando`)
- `test-<variant>` — Clasp regression tests
- `ansi-test-<variant>`, `asdf-test-<variant>`, `bench-<variant>` — corresponding suites
- `cando-test-<variant>` — cando's own regression tests (implies `extension-<variant>`)
- `analyze` / `analyze-boehm` — static analyzer pass over cando source (see "Static analyzer and GC descriptors" below)

To run a single regression test, the test driver is invoked via `ninja` but takes test-selection arguments through the test harness in `src/lisp/regression-tests/`. Read `build/regression-tests/` and the test-driver lisp to see how to target one test; don't guess a flag.

## Static analyzer and GC descriptors

The precise GC needs a layout descriptor for every managed C++ class. These descriptors are produced by the **clasp-analyzer**, a libclang-based pass that reads the C++ headers and writes a `.sif` file (a stream of top-level Common Lisp s-expressions, one record per class).

- `src/lisp/modules/clasp-analyzer/` — the analyzer itself (Lisp; uses `clang-tool` from `src/lisp/modules/clang-tool/`)
- `src/analysis/clasp_gc.sif` — committed canonical descriptor for plain Clasp
- `src/analysis/clasp_gc_cando.sif` — committed canonical descriptor for cando builds (selected when `:cando` is in `*extensions*` — see `src/analysis/cscript.lisp`)
- `src/analysis/sif-tools.lisp` — diff/merge/compare utilities; defines the `.dif` format (base-fingerprint + record additions) for incremental updates
- `analyze` / `analyze-boehm` ninja targets re-run the analyzer to regenerate the `.sif` files

The format header in `sif-tools.lisp` is the authoritative grammar reference. `.sif.new`, `.sif.old`, `.dif`, and `_combined.sif` files in `src/analysis/` are scratch — only the two filenames listed above are tracked.

## Code layout (big picture)

- `src/` — Clasp C++ core, lisp kernel, and build driver lisp
  - `src/core/` — the Clasp C++ runtime (object model, types, bytecode VM, GC glue)
  - `src/lisp/kernel/` — Clasp kernel Lisp code (CLOS, `cleavir` native compiler, `cmp` bytecode compiler, `lsp` base library, `contrib` vendored libs)
  - `src/lisp/kernel/lsp/flamegraph.lisp` — in-process sampling-profile → SVG flame graph, triggered by `SIGUSR2` when `CLASP_FLAME_PROFILE` is set (see below)
  - `src/lisp/modules/` — ASDF and other modules compiled into the base image
  - `src/main/main.cc` — the executable entry point
  - `src/llvmo/` — Clasp's LLVM IR builder bindings
  - `src/gctools/`, `src/bdwgc/` — GC integration
  - `src/clbind/` — C++ ↔ Lisp binding helper (think Boost.Python for Clasp)
  - `src/scraper/` — reads C++ headers to autogenerate binding code
- `extensions/cando/` — the cando extension
  - `extensions/cando/src/` — cando C++ sources (molecular modeling primitives)
  - `extensions/cando/include/` — cando public headers
  - `extensions/cando/chem-lisp/` — cando Lisp code
  - `extensions/cando/cscript.lisp` — cando's source manifest (must be edited when adding `.cc`/`.h` files)
  - `extensions/cando/AGENTS.md` — brief cando-specific notes for agents
- `koga` (file, not directory) — SBCL script driving build configuration
- `config.sexp`, `repos.sexp`, `version.sexp` — build config, dependency manifest, version string
- `tools/`, `tools-for-build/` — helper scripts (lldb/gdb helpers, copyright checker, allocation metering)
- `apptainer/`, `guix/`, `gentoo/`, `debian/`, `docker/` — packaging recipes per distro

Adding a new C++ class to cando: create header in `extensions/cando/include/`, source in `extensions/cando/src/`, add the `.cc` file to `extensions/cando/cscript.lisp`, then re-run `./koga --deep-clean` to rebuild the manifest.

## In-process profiling

The flame-graph profiler in `src/lisp/kernel/lsp/flamegraph.lisp` is gated by one env var, `CLASP_FLAME_PROFILE`:

- `CLASP_FLAME_PROFILE=1 cando ...` — enable with defaults (`/tmp/clasp-PID.svg`, 10 s @ 97 Hz)
- `CLASP_FLAME_PROFILE=path=/tmp/foo.svg:duration=5:rate=499 cando ...` — override any subset
- `CLASP_FLAME_PROFILE=0` (or unset) — disabled

When enabled, sending `SIGUSR2` to the process runs a sampling profile on a background thread and writes the SVG. Known keys are `path`, `duration` (seconds), `rate` (Hz); unknown keys emit a warning but don't disable profiling. See `parse-flame-profile-env` in that file for the full grammar.

## Build artifacts that aren't source

The following appear in the working tree but are build output / caches / user data — do not treat them as canonical:

- `build/` (all ninja output including compiled FASLs and object files)
- `GPATH`, `GRTAGS`, `GTAGS`, `TAGS` (tags files)
- root-level `*.sif`, `*.tar`, `Miniconda3-*.sh*` (apptainer/container images, archives, installers). **Note:** `src/analysis/*.sif` is *not* an artifact — those are committed GC descriptors; see "Static analyzer and GC descriptors" above.
- `cscript.lisp`, `t.lisp`, etc. at the repo root (developer scratch; the authoritative `cscript.lisp` files are the ones under `src/` and `extensions/<ext>/`)

## Style and branching

`CONTRIBUTING.md` sets the code style as LLVM (a `.clang-format` is in the tree). Feature work goes on `dev-<name>` branches, never directly to `master` or `testing`/`preview`. The LLVM C++ style applies to `.cc` / `.h` only; Lisp files follow the conventions already present in each file.
