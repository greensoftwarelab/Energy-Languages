// The Computer Language Benchmark Game
// http://benchmarksgame.alioth.debian.org/
//
// implementation inspired by gcc #6
// contributed by Ralph Ganszky


import Foundation
import Dispatch

// BlockReader class does a bit more than necessary, but this
// should not be that expensive.

final class BlockReader {
    
    // Constants
    private let blockSize = 64*1024
    private let lineSize = 60
    private let descMarker = ">".utf16.map{Int8($0)}[0]
    
    // Attributes
    private var block: UnsafeMutablePointer<Int8>
    private var bytesRead: Int
    private var eof: Bool
    private var fp: UnsafeMutablePointer<FILE>!
    private var readPos: Int
    
    init(fp: UnsafeMutablePointer<FILE>!) {
        block = UnsafeMutablePointer<Int8>.allocate(capacity: blockSize+1)
        block[blockSize] = 0
        bytesRead = 0
        eof = false
        self.fp = fp
        readPos = 0
    }
    
    deinit {
        block.deallocate(capacity: blockSize+1)
    }
    
    private func readBlock() {
        bytesRead = fread(block, 1, blockSize, fp)
        block[bytesRead] = 0
        if feof(fp) != 0 {
            eof = true
        }
        readPos = 0
    }
    
    func read() -> ([Int8], [Int8]) {
        
        // Allocate byte array
        var description = [Int8]()
        var seq = [Int8]()
        
        // Read a block if we don't have one
        if bytesRead == 0 {
            readBlock()
        }
        
        // Read description
        if block[readPos] == descMarker {
            // search for '\n'
            var descStart = readPos
            while block[readPos] != 10 {
                readPos += 1
                if readPos > bytesRead {
                    description.append(
			contentsOf: Array(
			    UnsafeBufferPointer(
				start: block.advanced(by: descStart),
				count: readPos-descStart)))
                    readBlock()
                    descStart = 0
                    if eof {
                        print("Error: EOF reached")
                        exit(1)
                    }
                }
            }
            readPos += 1
            description.append(
		contentsOf: Array(
		    UnsafeBufferPointer(
			start: block.advanced(by: descStart),
			count: readPos-descStart)))
        } else {
            print("Error: Could no find description!")
            exit(1)
        }
        
        // Read fasta lines
        var seqStart = readPos
        var endReached = false
        repeat {
            if let ppos = memchr(block.advanced(by: seqStart),
				 Int32(descMarker), bytesRead-seqStart) {
                // Append til pos and return
                let pos = block.distance(
			to: ppos.assumingMemoryBound(to: Int8.self))
                seq.append(
		    contentsOf: Array(
			UnsafeBufferPointer(
			    start: block.advanced(by: seqStart),
			    count: pos-seqStart)))
                readPos = pos
                endReached = true
            } else {
                // Append block and read next block
                seq.append(
		    contentsOf: Array(
			UnsafeBufferPointer(
			    start: block.advanced(by: seqStart),
			    count: bytesRead-seqStart)))
                readBlock()
                if eof {
                    if bytesRead > 0 {
                        seq.append(
			    contentsOf: Array(
				UnsafeBufferPointer(
				    start: block,
				    count: bytesRead)))
                    }
                    break
                }
                seqStart = 0
            }
        } while !endReached
        
        return (description, seq)
    }
}

// code  meaning   complement
// A    A                   T
// C    C                   G
// G    G                   C
// T/U  T                   A
// M    A or C              K
// R    A or G              Y
// W    A or T              W
// S    C or G              S
// Y    C or T              R
// K    G or T              M
// V    A or C or G         B
// H    A or C or T         D
// D    A or G or T         H
// B    C or G or T         V
// N    G or A or T or C    N

// Translation table
//               ABCDEFGHIJKLMNOPQRSTUVWXYZ      abcdefghijklmnopqrstuvwxyz
let translate = ("                                                                " +
                 " TVGH  CD  M KN   YSAABW R       TVGH  CD  M KN   YSAABW R ").utf16.map{Int8($0)}
func reverseComplement(seq: inout [Int8]) {
    var front = 0
    var back = seq.count - 1
    // Ignore '\n' at front and back
    while seq[front] == 10 && front <= back {
        front += 1
    }
    while seq[back] == 10 && front <= back {
        back -= 1
    }
    while front <= back {
        let temp = translate[Int(seq[front])]
        seq[front] = translate[Int(seq[back])]
        seq[back] = temp
        repeat { front += 1 } while seq[front] == 10
        repeat { back -= 1 } while seq[back] == 10
}
}

let lineWidth = 60
let group = DispatchGroup()
let rqueue = DispatchQueue(label: "Reader", attributes: [])
let cqueue = DispatchQueue(label: "Complement", attributes: .concurrent)
let wqueue = DispatchQueue(label: "Writer", attributes: [])

var reader = BlockReader(fp: stdin)
var toWrite = 0
var deferedWrite = [(Int, [Int8])]()

// Read, build reverse complement and write every of the three blocks
group.enter()
for i in 0..<3 {
    rqueue.sync {
        var (desc, seq) = reader.read()
        cqueue.async {
            reverseComplement(seq: &seq)
            wqueue.async {
                if i == toWrite {
                    fwrite(desc, desc.count, 1, stdout)
                    fwrite(seq, seq.count, 1, stdout)
                    toWrite += 1
                    if toWrite == 3 {
                        group.leave()
                    }
                    // Check for deferred blocks
                    if deferedWrite.count > 0 {
                        while deferedWrite.count > 0  {
                            for (j, (k, seq)) in deferedWrite.enumerated() {
                                if k == toWrite {
                                    fwrite(seq, seq.count, 1, stdout)
                                    deferedWrite.remove(at: j)
                                    toWrite += 1
                                    if toWrite == 3 {
                                        group.leave()
                                    }
                                    break
                                }
                            }
                        }
                    }
                } else {
		    // Append the two arrays into one,
		    // maybe this should be done in the reader
		    // like in gcc #6
                    deferedWrite.append((i, desc + seq))
                }
            }
        }
    }
}
group.wait()