## Performance benchmarks for nimkdl parser
##
## Run with: nim c -r --hints:off benchmark.nim
## Or optimized: nim c -r -d:release --hints:off benchmark.nim

import std/[times, strformat, monotimes, os, strutils]
import src/kdl

type
  BenchResult = object
    name: string
    totalTime: float
    iterations: int
    avgTime: float
    opsPerSec: float
    fileSize: int

proc formatDuration(seconds: float): string =
  if seconds < 0.001:
    &"{seconds * 1_000_000:.1f}μs"
  elif seconds < 1.0:
    &"{seconds * 1000:.2f}ms"
  else:
    &"{seconds:.3f}s"

proc formatSize(bytes: int): string =
  if bytes < 1024:
    &"{bytes}B"
  elif bytes < 1024 * 1024:
    &"{bytes div 1024}KB"
  else:
    &"{bytes div (1024*1024)}MB"

proc formatRate(rate: float): string =
  if rate < 1000:
    &"{rate:.1f} ops/s"
  elif rate < 1_000_000:
    &"{rate / 1000:.1f}K ops/s"
  else:
    &"{rate / 1_000_000:.2f}M ops/s"

proc benchmark(name: string, content: string, iterations: int): BenchResult =
  ## Benchmark parsing a KDL document
  let fileSize = content.len

  # Warmup
  for i in 1..min(100, iterations div 10):
    discard parseKdl(content)

  # Actual benchmark
  let start = getMonoTime()
  for i in 1..iterations:
    discard parseKdl(content)
  let elapsed = (getMonoTime() - start).inNanoseconds.float / 1_000_000_000.0

  let avgTime = elapsed / iterations.float
  let opsPerSec = iterations.float / elapsed

  result = BenchResult(
    name: name,
    totalTime: elapsed,
    iterations: iterations,
    avgTime: avgTime,
    opsPerSec: opsPerSec,
    fileSize: fileSize
  )

proc printResults(results: seq[BenchResult]) =
  echo ""
  echo "=" .repeat(80)
  echo "  KDL Parser Benchmarks"
  echo "=" .repeat(80)
  echo ""

  # Print header
  echo alignLeft("Benchmark", 30) & alignLeft("Size", 8) & alignLeft("Iterations", 12) &
       alignLeft("Avg Time", 12) & alignLeft("Throughput", 15)
  echo "-" .repeat(80)

  # Print each result
  for r in results:
    echo alignLeft(r.name, 30) & alignLeft(formatSize(r.fileSize), 8) &
         alignLeft($r.iterations, 12) & alignLeft(formatDuration(r.avgTime), 12) &
         alignLeft(formatRate(r.opsPerSec), 15)

  echo ""

  # Summary statistics
  var totalOps = 0
  var totalTime = 0.0
  var totalBytes = 0
  for r in results:
    totalOps += r.iterations
    totalTime += r.totalTime
    totalBytes += r.fileSize * r.iterations

  let avgOpsPerSec = totalOps.float / totalTime
  let throughputMBps = (totalBytes.float / totalTime) / (1024 * 1024)

  echo "Summary:"
  echo &"  Total operations: {totalOps}"
  echo &"  Total time: {formatDuration(totalTime)}"
  echo &"  Average throughput: {formatRate(avgOpsPerSec)}"
  echo &"  Data processed: {formatSize(totalBytes)}"
  echo &"  MB/s: {throughputMBps:.2f}"
  echo ""

proc main() =
  let testsDir = currentSourcePath().parentDir() / "tests" / "test_cases"

  var results: seq[BenchResult]

  # Benchmark test files of varying sizes
  let benchmarks = [
    ("Cargo.kdl (small)", testsDir / "examples" / "Cargo.kdl", 10_000),
    ("ci.kdl (medium)", testsDir / "examples" / "ci.kdl", 5_000),
    ("website.kdl (medium)", testsDir / "examples" / "website.kdl", 5_000),
    ("all_node_fields.kdl", testsDir / "input" / "all_node_fields.kdl", 20_000),
    ("all_escapes.kdl", testsDir / "input" / "all_escapes.kdl", 20_000),
  ]

  for (name, path, iters) in benchmarks:
    if not fileExists(path):
      echo &"Warning: {path} not found, skipping"
      continue

    let content = readFile(path)
    echo &"Running: {name} ({content.len} bytes, {iters} iterations)..."
    let result = benchmark(name, content, iters)
    results.add(result)

  # Benchmark parsing operations on synthetic documents
  echo "Running: Synthetic (nodes)"
  let syntheticNodes = """
node1 "arg1" prop1="val1"
node2 123 prop2=456
node3 3.14 prop3="value"
""".repeat(10)
  results.add(benchmark("Synthetic (30 nodes)", syntheticNodes, 10_000))

  echo "Running: Synthetic (deep nesting)"
  var deepNest = ""
  for i in 1..20:
    deepNest.add(&"level{i} {{\n")
  deepNest.add("leaf \"value\"\n")
  for i in 1..20:
    deepNest.add("}\n")
  results.add(benchmark("Synthetic (deep nesting)", deepNest, 10_000))

  echo "Running: Synthetic (wide)"
  var wide = ""
  for i in 1..100:
    wide.add(&"node{i} \"arg\" key=\"val\"\n")
  results.add(benchmark("Synthetic (100 nodes)", wide, 5_000))

  printResults(results)

when isMainModule:
  main()
