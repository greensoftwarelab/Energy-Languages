// The Computer Language Benchmarks Game
// http://benchmarksgame.alioth.debian.org/
//
// regex-dna program contributed by Daniel Muellenborn
// converted from regex-dna program

import Foundation

var data = FileHandle.standardInput.readDataToEndOfFile()

var sequence = String(data: data, encoding: .utf8)!

let inputLength = data.count

let regex: (String) -> NSRegularExpression = { pattern in
  return try! NSRegularExpression(pattern: pattern, options: [])
}

sequence = regex(">[^\n]*\n|\n").stringByReplacingMatches(in: sequence, options: [], range: NSRange(location: 0, length: inputLength), withTemplate: "")

let codeLength = sequence.utf8.count

let variants = [
  "agggtaaa|tttaccct",
  "[cgt]gggtaaa|tttaccc[acg]",
  "a[act]ggtaaa|tttacc[agt]t",
  "ag[act]gtaaa|tttac[agt]ct",
  "agg[act]taaa|ttta[agt]cct",
  "aggg[acg]aaa|ttt[cgt]ccct",
  "agggt[cgt]aa|tt[acg]accct",
  "agggta[cgt]a|t[acg]taccct",
  "agggtaa[cgt]|[acg]ttaccct",
]

var counts = Array(repeating: ("",0), count: variants.count)

// parallelized version is slower
// let queue = DispatchQueue(label: "Queue")
// DispatchQueue.concurrentPerform(iterations: variants.count) { n in
for n in 0..<variants.count {
  counts[n] = (variants[n], regex(variants[n]).numberOfMatches(in: sequence, options: [], range: NSRange(location: 0, length: codeLength)))
}

for (variant, count) in counts {
  print(variant, "\(count)")
}

let replacements = [
  (regex("tHa[Nt]"), "<4>"),
  (regex("aND|caN|Ha[DS]|WaS"), "<3>"),
  (regex("a[NSt]|BY"), "<2>"),
  (regex("<[^>]*>"), "|"),
  (regex("\\|[^|][^|]*\\|"), "-"),
]

for (re, replacement) in replacements {
  sequence = re.stringByReplacingMatches(in: sequence, options: [], range: NSRange(location: 0, length: sequence.utf16.count), withTemplate: replacement)
}

let resultLength = sequence.utf8.count

print("", inputLength, codeLength, resultLength, separator: "\n")