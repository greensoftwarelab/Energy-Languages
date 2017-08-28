# The Computer Language Benchmarks Game
# http://benchmarksgame.alioth.debian.org
#
# Contributed by Aaron Tavistock

def process_segment(segment)
  (header, _, sequence) = segment.partition("\n")

  sequence.delete!("\n\r >")
  sequence.reverse!
  sequence.tr!(
    'wsatugcyrkmbdhvnATUGCYRKMBDHVN',
    'WSTAACGRYMKVHDBNTAACGRYMKVHDBN'
  )

  results = [">#{header}"]

  idx = 0
  length = sequence.length
  while idx < length
    results << sequence[idx,60]
    idx += 60
  end

  results.join("\n")
end

def forking_worker(segment)
  reader, writer = IO.pipe

  pid = Process.fork do
    begin
      reader.close
      results = process_segment(segment)
      Marshal.dump(results, writer)
    ensure
      writer.close
    end
  end

  writer.close
  begin
    results = Marshal.load(reader)
  ensure
    reader.close
  end
  Process.waitpid(pid)

  results
end

threads = []
$stdin.each_line('>') do |segment|
  next if segment.length < 2

  threads << Thread.new do
    if RUBY_PLATFORM == 'java'
      Thread.current[:output] = process_segment(segment)
    else
      Thread.current[:output] = forking_worker(segment)
    end
  end
end
threads.each(&:join)

threads.each do |thread|
  puts thread[:output]
end