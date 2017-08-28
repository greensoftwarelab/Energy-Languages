// The Computer Language Benchmarks Game
// http://benchmarksgame.alioth.debian.org/
//
// contributed by the Rust Project Developers
// contributed by TeXitoi
// multi-threaded version contributed by Alisdair Owens

extern crate num_cpus;

use std::cmp::min;
use std::io;
use std::io::{Write, BufWriter, ErrorKind};
use std::sync::{Mutex,Arc};
use std::thread;

const LINE_LENGTH: usize = 60;
const IM: u32 = 139968;
const LINES: usize = 1024;
const BLKLEN: usize = LINE_LENGTH * LINES;

struct MyStdOut {
    thread_count: u16,
    next_thread_num: u16,
    stdout: io::Stdout,
}

struct MyRandom {
    last: u32,
    count: usize,
    thread_count: u16,
    next_thread_num: u16,
}

impl MyRandom {
    fn new(count: usize, thread_count: u16) -> MyRandom {
        MyRandom { 
            last: 42,
            count: count,
            thread_count: thread_count,
            next_thread_num: 0
        }
    }
    
    fn normalize(p: f32) -> u32 {(p * IM as f32).floor() as u32}

    fn reset(&mut self, count: usize) {
        self.next_thread_num = 0;
        self.count = count;
    }

    fn gen(&mut self, buf: &mut [u32], cur_thread: u16) -> Result<usize,()> {

        if self.next_thread_num != cur_thread {
            return Err(())
        }

        self.next_thread_num+=1;
        if self.next_thread_num == self.thread_count {
            self.next_thread_num = 0;
        }

        let to_gen = min(buf.len(), self.count);
        for i in 0..to_gen {
            self.last = (self.last * 3877 + 29573) % IM;
            buf[i] = self.last;
        }
        self.count -= to_gen;
        Ok(to_gen)
    }
}

impl MyStdOut {
    fn new(thread_count: u16) -> MyStdOut {
        MyStdOut {
            thread_count: thread_count,
            next_thread_num: 0,
            stdout: io::stdout() 
        } 
    }
    fn write(&mut self, data: &[u8], cur_thread: u16) -> io::Result<()> {
        if self.next_thread_num != cur_thread {
            return Err(io::Error::new(ErrorKind::Other, ""));
        }

        self.next_thread_num+=1;
        if self.next_thread_num == self.thread_count {
            self.next_thread_num = 0;
        }

        self.stdout.write_all(data)
    }
}

fn make_random(data: &[(char, f32)]) -> Vec<(u32, u8)> {
    let mut acc = 0.;
    data.iter()
        .map(|&(ch, p)| {
            acc += p;
            (MyRandom::normalize(acc), ch as u8)
        })
        .collect()
}

fn make_fasta2<I: Iterator<Item=u8>>(header: &str, mut it: I, mut n: usize)
    -> io::Result<()> {
    let mut sysout = BufWriter::new(io::stdout());
    try!(sysout.write_all(header.as_bytes()));
    let mut line = [0u8; LINE_LENGTH + 1];
    while n > 0 {
        let nb = min(LINE_LENGTH, n);
        for i in (0..nb) {
            line[i] = it.next().unwrap();
        }
        n -= nb;
        line[nb] = '\n' as u8;
        try!(sysout.write_all(&line[..(nb+1)]));
    }
    Ok(())
}

fn do_fasta(thread_num: u16, rng: Arc<Mutex<MyRandom>>,
            wr: Arc<Mutex<MyStdOut>>, data: Vec<(u32, u8)>) {
    
    let mut rng_buf = [0u32; BLKLEN];
    let mut out_buf = [0u8; BLKLEN + LINES];
    let mut count;
    loop {
        loop {
            if let Ok(x) = rng.lock().unwrap().gen(&mut rng_buf, thread_num) {
                count = x;
                break;
            }
        };

        if count == 0 {
            break;
        }
        let mut line_count = 0;
        for i in 0..count {
            if i % LINE_LENGTH == 0 && i > 0 {
                out_buf[i+line_count] = b'\n';
                line_count += 1;
            } 
            let rn = rng_buf[i];
            for j in &data {
                if j.0 >= rn {
                    out_buf[i+line_count] = j.1;
                    break;
                }
            }
        }
        out_buf[count+line_count] = b'\n';

        while let Err(_) = wr.lock()
                .unwrap()
                .write(&out_buf[..(count+line_count+1)], thread_num) {};
    }
}

fn make_fasta(header: &str, rng: Arc<Mutex<MyRandom>>,
                 data: Vec<(u32, u8)>, num_threads: u16
             ) -> io::Result<()> {

    let stdout = Arc::new(Mutex::new(MyStdOut::new(num_threads)));
    try!(io::stdout().write_all(header.as_bytes()));
    let mut threads = Vec::new();
    for thread in 0..num_threads {
        let d = data.clone();
        let rng_clone = rng.clone();
        let stdout_clone = stdout.clone();
        threads.push(thread::spawn(move || {
            do_fasta(thread, rng_clone, stdout_clone, d);
        }));
    }
    for thread_guard in threads {
        thread_guard.join().unwrap();
    }
    Ok(())
}

fn main() {
    let n = std::env::args_os().nth(1)
        .and_then(|s| s.into_string().ok())
        .and_then(|n| n.parse().ok())
        .unwrap_or(1000);
    
    let num_threads: u16 = num_cpus::get() as u16;

    let rng = Arc::new(Mutex::new(MyRandom::new(n*3, num_threads)));
    let alu: &[u8] = b"GGCCGGGCGCGGTGGCTCACGCCTGTAATCCCAGCACTTT\
                       GGGAGGCCGAGGCGGGCGGATCACCTGAGGTCAGGAGTTC\
                       GAGACCAGCCTGGCCAACATGGTGAAACCCCGTCTCTACT\
                       AAAAATACAAAAATTAGCCGGGCGTGGTGGCGCGCGCCTG\
                       TAATCCCAGCTACTCGGGAGGCTGAGGCAGGAGAATCGCT\
                       TGAACCCGGGAGGCGGAGGTTGCAGTGAGCCGAGATCGCG\
                       CCACTGCACTCCAGCCTGGGCGACAGAGCGAGACTCCGTCT\
                       CAAAAA";

    let iub = &[('a', 0.27), ('c', 0.12), ('g', 0.12),
                ('t', 0.27), ('B', 0.02), ('D', 0.02),
                ('H', 0.02), ('K', 0.02), ('M', 0.02),
                ('N', 0.02), ('R', 0.02), ('S', 0.02),
                ('V', 0.02), ('W', 0.02), ('Y', 0.02)];

    let homosapiens = &[('a', 0.3029549426680),
                        ('c', 0.1979883004921),
                        ('g', 0.1975473066391),
                        ('t', 0.3015094502008)];

    make_fasta2(">ONE Homo sapiens alu\n",
                    alu.iter().cycle().map(|c| *c), n * 2).unwrap();
    make_fasta(">TWO IUB ambiguity codes\n",
                    rng.clone(), make_random(iub), num_threads).unwrap();

    rng.lock().unwrap().reset(n*5);

    make_fasta(">THREE Homo sapiens frequency\n",
                    rng, make_random(homosapiens), num_threads).unwrap();

    io::stdout().flush().unwrap();
}