# The Computer Language Benchmarks Game
# http://benchmarksgame.alioth.debian.org/
# Contributed by Wesley Moxam
# Modified by Sokolov Yura aka funny_falcon
# Parallelised by Scott Leggett

module MiniParallel
    class Worker
        def initialize(read, write)
            @read, @write = read, write
        end

        def close_pipes
            @read.close
            @write.close
        end

        def work(index)
            Marshal.dump(index, @write)
            Marshal.load(@read)
        end
    end

    def self.map(array, &block)
        work_in_processes(
            array,
            [array.size, core_count].min,
            &block
        )
    end

    def self.core_count
        @@core_count ||= IO.read("/proc/cpuinfo").scan(/^processor/).size
    end

    private

    def self.work_in_processes(array, count, &block)
        index = -1
        results, threads = [], []

        workers = create_workers(array, count, &block)

        workers.each do |worker|
            threads << Thread.new do
                loop do
                    Thread.exclusive{ index += 1 }
                    break if index >= array.size
                    results[index] = worker.work(index)
                end
                worker.close_pipes
            end
        end

        threads.each(&:join)
        Process.waitall

        results
    end

    def self.create_workers(array, count, &block)
        workers = []
        count.times do
            workers << create_worker(array, workers, &block)
        end
        workers
    end

    def self.create_worker(array, started_workers, &block)
        child_read, parent_write = IO.pipe
        parent_read, child_write = IO.pipe

        Process.fork do
            started_workers.each(&:close_pipes)

            parent_write.close
            parent_read.close

            process_incoming_jobs(child_read, child_write, array, &block)

            child_read.close
            child_write.close
        end

        child_read.close
        child_write.close

        Worker.new(parent_read, parent_write)
    end

    def self.process_incoming_jobs(read, write, array, &block)
        until read.eof?
            index = Marshal.load(read)
            Marshal.dump(block.call(array[index]), write)
        end
    end
end

class Fannkuch

    def initialize(n, start, max_perms)
        @n = n
        @p = (0..n).to_a
        @s = @p.dup
        @q = @p.dup
        @sign = 1
        @sum = @maxflips = 0
        @max_perms = max_perms
        @perm_count = -start
        start.times{permute}
    end

    def flip
        loop do
            if @perm_count == @max_perms
                return [@sum, @maxflips]
            end
            if (q1 = @p[1]) != 1
                @q[0..-1] = @p
                flips = 1
                until (qq = @q[q1]) == 1
                    @q[q1] = q1
                    if q1 >= 4
                        i, j = 2, q1 - 1
                        while i < j
                            @q[i], @q[j] = @q[j], @q[i]
                            i += 1
                            j -= 1
                        end
                    end
                    q1 = qq
                    flips += 1
                end
                @sum += @sign * flips
                @maxflips = flips if flips > @maxflips # New maximum?
            end
            permute
        end
    end

    def permute
        @perm_count += 1

        if @sign == 1
            # Rotate 1<-2.

            @p[1], @p[2] = @p[2], @p[1]
            @sign = -1
        else
            # Rotate 1<-2 and 1<-2<-3.

            @p[2], @p[3] = @p[3], @p[2]
            @sign = 1
            i = 3
            while i <= @n && @s[i] == 1
                @s[i] = i
                # Rotate 1<-...<-i+1.

                t = @p.delete_at(1)
                i += 1
                @p.insert(i, t)
            end
            @s[i] -= 1  if i <= @n
        end
    end
end

abort "Usage: #{__FILE__} n\n(n > 6)" if (n = ARGV[0].to_i) < 6

core_count = MiniParallel.core_count
chunk_size = (1..n).reduce(:*) / core_count

sum, flips =
    if core_count > 1
        # adjust job sizes to even out workload
        weights = if core_count > 1
                      weights = []
                      (core_count/2).times do |i|
                          weights << i * 0.1 + 0.05
                      end
                      weights = weights.reverse + weights.map{|i|-i}
                  else
                      [0]
                  end

        # Generate start position for each chunk
        chunks = core_count.times.zip(weights).map do |count, weight|
            [count * chunk_size +
             (count > 0 ? (weights[0,count].reduce(:+) * chunk_size).round : 0),
             chunk_size + (weight * chunk_size).round]
        end

        chunk_results =
            if RUBY_PLATFORM == 'java'
                chunk_collector = []
                threads = []
                chunks.each.with_index do |(start,weighted_size),i|
                    threads << Thread.new do
                        chunk_collector[i] = Fannkuch.new(n,start,weighted_size).flip
                    end
                end
                threads.all?(&:join)
                chunk_collector
            else
                MiniParallel.map(chunks) do |start, weighted_size|
                    Fannkuch.new(n,start,weighted_size).flip
                end
            end

        chunk_results.reduce do |memo, (cksum, fmax)|
            [memo[0] + cksum, [memo[1], fmax].max]
        end
    else
        Fannkuch.new(n,0,chunk_size).flip
    end

printf "%d\nPfannkuchen(%d) = %d\n", sum, n, flips
