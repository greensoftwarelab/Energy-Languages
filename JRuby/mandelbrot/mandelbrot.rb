# The Computer Language Benchmarks Game
# http://benchmarksgame.alioth.debian.org
#
# Contributed by Aaron Tavistock

require 'thread'

class ThreadPool
  
  def initialize
    @work = Queue.new
    @pool = Array.new(cpu_count) do |i|
      Thread.new do
        Thread.current[:id] = i
        catch(:exit) do
          while(true) do
            work, args = @work.pop
            work.call(*args)
          end
        end
      end
    end      
  end
  
  def schedule(*args, &block)
    @work << [block, args]
  end
  
  def shutdown
    @pool.size.times do
      schedule { throw :exit }
    end
    @pool.each do |t|
      t.join
    end
  end 
  
  private 
  
  def cpu_count
    begin
      if File.readable?('/proc/cpuinfo') # Linux
        %x[cat /proc/cpuinfo | grep -c processor].chomp.to_i
      elsif File.executable?('/usr/sbin/sysctl')  # OS/X
        %x[/usr/sbin/sysctl -n hw.ncpu].chomp.to_i
      else 
        1
      end
    rescue
      1
    end
  end
  
end

class Mandel
  
  attr_reader :output
  
  def initialize(size)
    @size = size.to_i
    @workers = ThreadPool.new
    @output = Array.new(@size)
  end

  def process
    @size.times do |row|
      @workers.schedule(row) do |y|        
        ci = (2.0 * y.to_f / @size.to_f ) - 1.0
        @output[y] = render_row(ci, @size)
      end
    end
    @workers.shutdown
  end
  
  def self.render(size)
    m = Mandel.new(size)
    m.process
    print "#{m.header}\n#{m.output.join}"
  end
  
  def header
    "P4\n#{@size} #{@size}"
  end

  private
  
  def render_row(ci, size)
    row_bits = Array.new(size) do |col|
      cr = (2.0 * col.to_f / size.to_f) - 1.5
      [cr, ci]
    end
  
    row = []
    row_bits.each_slice(8) do |bits|
      row << render_byte(bits)
    end
    row.join
  end

  def render_byte(bit_data)
    byte_acc = 0
    bit_data.each do |cr_ci|
      byte_acc = (byte_acc << 1) | get_bit_or(*cr_ci)
    end
    if remaining_bits = bit_data.size - 8
      byte_acc <<= remaining_bits
    end
    byte_acc.chr
  end

  def get_bit_or(cr, ci)
    zrzr = 0.0
    zizi = 0.0
    zrzi = 0.0

    count = 50
    while count > 0
      
      zr = zrzr - zizi + cr
      zi = 2.0 * zrzi + ci

      # preserve recalculation
      zrzr = zr*zr
      zizi = zi*zi
      zrzi = zr*zi

      return 0b0 if zrzr + zizi > 4.0
        
      count -= 1
    end

    0b1
  end

end

Mandel.render(ARGV.shift)