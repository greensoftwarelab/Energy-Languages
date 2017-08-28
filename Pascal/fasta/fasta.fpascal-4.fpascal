{ The Computer Language Benchmarks Game
  http://benchmarksgame.alioth.debian.org

  contributed by Ian Osgood
  modified by Vincent Snijders
  modified by Steve Fisher
}

{$mode objfpc}

const
  ALU : AnsiString =
  'GGCCGGGCGCGGTGGCTCACGCCTGTAATCCCAGCACTTTGG' +
  'GAGGCCGAGGCGGGCGGATCACCTGAGGTCAGGAGTTCGAGA' +
  'CCAGCCTGGCCAACATGGTGAAACCCCGTCTCTACTAAAAAT' +
  'ACAAAAATTAGCCGGGCGTGGTGGCGCGCGCCTGTAATCCCA' +
  'GCTACTCGGGAGGCTGAGGCAGGAGAATCGCTTGAACCCGGG' +
  'AGGCGGAGGTTGCAGTGAGCCGAGATCGCGCCACTGCACTCC' +
  'AGCCTGGGCGACAGAGCGAGACTCCGTCTCAAAAA';

  codes = 'acgtBDHKMNRSVWY';

  IUB : array[0..14] of double = ( 0.27, 0.12, 0.12, 0.27, 0.02,
  0.02, 0.02, 0.02, 0.02, 0.02, 0.02, 0.02, 0.02, 0.02, 0.02 );

  HomoSap : array[0..3] of double = ( 0.3029549426680,
  0.1979883004921, 0.1975473066391,  0.3015094502008 );

  //  Width of ouput lines.
  width = 60;

type
  TGene = record
    prob: double;
    code: char;
  end;
  PGene = ^TGene;

var
  n : longint;
  Genes: array of TGene;
  text_buf: array[0..$ffff] of byte;

procedure fasta_repeat( n: integer );
var
  source_alu: ansistring;
  here: integer;
begin
  source_alu := alu + alu[1 .. width];
  here := 1;
  repeat
    writeln( source_alu[ here .. here + width - 1] );
    here += width;
    if here > length( alu ) then
      here -= length( alu );
    n -= width
  until n <= width;
  writeln( source_alu[ here .. here + n - 1] )
end;


function gen_random(limit : integer): double;
const
  seed : integer = 42;
  IM = 139968;
  IA = 3877;
  IC = 29573;
begin
  seed := (seed * IA + IC) mod IM;
  result := limit * seed * (1 / IM)
end;

procedure init_genes(const probs: array of double);
var
  i : integer;
  sum_prob: double;
begin
  setLength(Genes, length(probs));
  sum_prob := 0.0;
  for i := 0 to high(probs) do
  begin
    sum_prob += probs[i];
    Genes[i].prob := sum_prob;
    Genes[i].code := codes[i+1]
  end
end;

procedure fasta_random(n : integer; const probs: array of double);

  function choose_code : char; inline;
  var r : double;
      gene: PGene;
  begin
    r := gen_random(1);
    gene := @Genes[ 0 ];
    while r >= gene^.prob do
      inc(gene);
   result := gene^.code
  end;

  procedure do_one_line( size: integer ); inline;
  var
    line : string;
    p, p_limit : pchar;
  begin
    SetLength(line, size);
    p := @line[1];
    p_limit := @line[size];
    while p <= p_limit do
    begin
      p^ := choose_code;
      inc(p)
    end;
    writeln( line )
  end;

begin
  // Make gene array.
  init_genes(probs);

  while n > width do
  begin
    do_one_line( width );
    n -= width
  end;

  do_one_line( n )
end;


begin
  SetTextBuf(output, text_buf);
  val(paramstr(1), n);

  writeln('>ONE Homo sapiens alu');
  fasta_repeat(n*2);

  writeln('>TWO IUB ambiguity codes');
  fasta_random(n*3, IUB);

  writeln('>THREE Homo sapiens frequency');
  fasta_random(n*5, HomoSap)
end.