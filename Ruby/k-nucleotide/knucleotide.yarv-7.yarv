# The Computer Language Benchmarks Game
# http://benchmarksgame.alioth.debian.org
#
# contributed by Aaron Tavistock

def find_frequencies(keys)
  @frequencies = {}
  threads = []

  key_lengths = keys.map(&:size).uniq
  key_lengths.each do |key_length|
    threads << Thread.new do
      results_hash = key_frequency(key_length, @seq)
      @frequencies.merge!(results_hash)
    end
  end
  threads.each(&:join)
  @frequencies
end

def forking_key_frequency(key_length, seq)
  reader, writer = IO.pipe

  pid = Process.fork do
    begin
      reader.close
      results = original_key_frequency(key_length, seq)
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

def key_frequency(key_length, seq)
  count = Hash.new(0)
  start_index = 0
  last_length = seq.size - key_length
  while start_index < last_length
    key = seq.byteslice(start_index, key_length)
    count[key] += 1
    start_index += 1
  end
  count
end

def frequency(keys)
  keys.map do |key|
    [key, @frequencies[key]]
  end
end

def percentage(keys)
  frequency(keys).sort { |a,b| b[1] <=> a[1] }.map do |key, value|
    "%s %.3f" % [ key.upcase, ( (value*100).to_f / @seq.size) ]
  end
end

def count(keys)
  frequency(keys).sort_by { |a| a[0].size }.map do |key, value|
    "#{value.to_s}\t#{key.upcase}"
  end
end

def load_sequence(marker)
  input = STDIN.read
  start_idx = input.index(marker) + marker.size
  @seq = input[start_idx, input.size - 1]
  @seq.delete!("\n ")
  @seq.freeze
  @seq
end

if RUBY_PLATFORM != 'java'
  class << self
    alias_method :original_key_frequency, :key_frequency
    alias_method :key_frequency, :forking_key_frequency
  end
end

singles = %w(a t c g)
doubles = %w(aa at ac ag ta tt tc tg ca ct cc cg ga gt gc gg)
chains  = %w(ggt ggta ggtatt ggtattttaatt ggtattttaatttatagt)

load_sequence('>THREE Homo sapiens frequency')
find_frequencies(singles + doubles + chains)

print "#{percentage(singles).join("\n")}\n\n"
print "#{percentage(doubles).join("\n")}\n\n"
print "#{count(chains).join("\n")}\n"