# The Computer Language Benchmarks Game
# http://benchmarksgame.alioth.debian.org
# Contributed by Rick Branson
# fixed by Isaac Gouy
#
# There is currently a bug in JRuby as of 11/16/2010 that causes
# Marshal to read IOs in a blocking manner, which will cause this
# script to hang.

IS_THREADED = RUBY_PLATFORM == "java"

if IS_THREADED
  require "fcntl"
end

class Worker
  
  attr_reader :reader
  
  def initialize(enum, index, total, &block)
    @enum   = enum
    @index  = index
    @total  = total
    
    @reader, @writer = IO.pipe
      
    if IS_THREADED
      @thread = Thread.new do
        self.execute(&block)
      end
    else
      @p = Process.fork do
        @reader.close
        self.execute(&block)
        @writer.close
      end
      
      @writer.close
    end
  end
  
  def execute(&block)
    chk   = @enum.size / @total
    
    (0 ... @enum.size).step(@total) do |i|
      idx = i + @index
      d = @enum[idx]
      to_parent([idx, yield(d)]) unless d == nil
    end
  end

  def to_parent(msg)
    Marshal.dump(msg, @writer)
  end
  
  def self.gather(workers)
    res = []
    ios = workers.map { |w| w.reader }

    while ios.size > 0 do
      sr, sw, se = IO.select(ios, nil, nil)

      if sr
        sr.each do |io|
          begin
            loop do
              msg = Marshal.load(io)          
              idx, content = msg
              res[idx] = content
            end
          rescue EOFError
            ios.delete(io)
          end
        end
      end
    end
    
    Process.waitall
    res
  end

  def self.map(enum, worker_count = 6, &block)
    count = [enum.size, worker_count].min

    workers = (0 ... count).map do |idx|
      Worker.new(enum, idx, count, &block)
    end

    Worker.gather(workers)    
  end
end

def eval_a(i, j)
  1.0/((i+j)*(i+j+1.0)/2.0+i+1.0)
end

def eval_A_times_u(u)
  usz     = u.size
  urange  = (0 ... usz)
  umap    = urange.to_a
  
  Worker.map(umap) do |i|
    urange.inject(0) do |sum, j|
      sum += eval_a(j, i) * u[j]
    end
  end
end

def eval_At_times_u(u)
  usz     = u.size
  urange  = (0 ... usz)
  umap    = urange.to_a
  
  Worker.map(umap) do |i|
    urange.inject(0) do |sum, j|
      sum += eval_a(i, j) * u[j]      
    end
  end
end

def eval_AtA_times_u(u)
  eval_At_times_u(eval_A_times_u(u))
end

n = ARGV[0].to_i
u = [1] * n
v = nil

10.times do
  v = eval_AtA_times_u(u)
  u = eval_AtA_times_u(v)
end

vBv = 0
vv  = 0

(0 ... n).each do |i|
  vBv += u[i] * v[i]
  vv  += v[i] * v[i]
end

print "%0.9f" % (Math.sqrt(vBv / vv)), "\n"