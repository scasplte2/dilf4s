# Gas measurements

`calibrate.sh` measures the standard corpus on Scala and Rust. `dedup.sh` measures one large host-input shape per
process so other maximum-size shapes do not occupy its heap. Run from any directory on a quiet Linux machine with
Java 21, sbt, Cargo, Python and `taskset`. `CPU` selects one permitted CPU (default 0).

```sh
tools/gas/dedup.sh .gas/short-max short 1864135 permuted 101 3
```

Arguments are output directory, value kind, count, order, rounds and processes per backend. Kinds are `int`,
`short` (eight-digit text), and `prefix` (1024 shared characters plus eight digits). Orders are `ascending`,
`descending`, `permuted` (multiplication by 104729 modulo count; count must be coprime to it), and
`repeated` (the first 32 values repeated in order; `a2` supplies their exact first occurrences).
Largest individual arrays within the canonical weight cap have 16777215 integers, 1864135 short texts or 16241
prefix texts. Host data exposes the same array as `a0` and `a1`; each input array fits the value cap independently.
These are deliberately above the default evaluation gas budget: measurement uses the harness's maximum gas limit.

Parsing and constructing input are outside the timed section. The baseline is one `a0` lookup; intersection also
includes its second lookup. For `repeated` unique, the baseline looks up `a2` instead. Checks assert that `unique`
returns the exact first occurrences and `intersect` returns true. Java uses a fixed 4 GiB heap, pre-touch and Serial GC. Some maximum shapes may exceed that heap;
record such a failure as a capacity limit, never a timing result.

Keep raw TSV rounds, check logs, GC logs and `summary.tsv`. `ratios.py` reports rejected rounds, confidence intervals,
process spread and insufficient process counts. A one-process exploratory result is not a completed calibration.
Compare sorted, reversed and permuted inputs, and separately assess duplicate-heavy inputs before setting prices.
