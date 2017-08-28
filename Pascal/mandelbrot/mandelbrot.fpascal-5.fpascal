{ The Computer Language Benchmarks Game
  http://benchmarksgame.alioth.debian.org

  contributed by Ales Katona
  modified by Vincent Snijders
  optimized and multithreaded by Jean de La Taille
  modified by Jeppe Johansen
  modified by Peter Blackman
      (Restore 'CalculatePoint' as leaf function, better use of registers)
}

program mandelbrot;

uses
  {$ifdef unix}cthreads,{$endif}
  sysUtils, math;

const
  ThreadCount = 4;

var
  nInv: double;
  TextBuf: pbyte; 
  yCounter,
  n, dimx : longint;


function subThread(p: pointer) : ptrint;
var
  Cr, Ci : Double;
  x, y, bits, bit, buf_index: Longint;
  
   function CalculatePoint(Cx, Cy : double): boolean; inline;
   var
     Limit : double = double(4);
     Two   : double = double(2);
     Zr, Zi, Ti, Tr: Double;
     i: longint;

  begin
    Zr := 0;  Zi := Zr; Tr := Zr; Ti := Zr;
    for i := 1 to 50 do begin
      Zi := Two*Zr*Zi + Cy;
      Zr := Tr - Ti + Cx;
      Ti := Zi * Zi;
      Tr := Zr * Zr;
      if (Tr + Ti>=limit) then exit(true);
    end;

    CalculatePoint := false;
  end;
  
  
begin
  while true do
  begin
    y := interlockedincrement(yCounter)-1;

    if y >= n then break;

    buf_index := y*dimx;
    prefetch(TextBuf[buf_index]);

    bit := 128; // 1000 0000
    bits := 0;

    Ci := ((y + y) * nInv) - 1.0;
    for x := 0 to n-1 do
    begin
      Cr := ((x + x) * nInv) - 1.5;
           
      If CalculatePoint (Cr, Ci) then
          bits := bits or bit;
 
      bit := bit >> 1;
      if (bit = 0) then
      begin
        TextBuf[buf_index] := not bits;
        inc(buf_index);

        bits := 0;
        bit := 128;
      end;
    end;
  end;
  subThread := 0;
end;

procedure run;
var
  tt : array[0..ThreadCount-1] of TThreadID;
  i, t, buf_index: Longint;
  
begin
  nInv := 1/n;

  for i := 0 to ThreadCount-1 do
    tt[i] := BeginThread(@subThread, nil);

  for i := 0 to ThreadCount-1 do
    WaitForThreadTerminate(tt[i], 0);

  buf_index := 0;
  i := n*dimx;
  while buf_index < i do
  begin
    t := FileWrite(StdOutputHandle, TextBuf[buf_index], i-buf_index);;
    if t >= 0 then
      buf_index := buf_index + t;
  end;
end;

begin
  Val(ParamStr(1), n);
  write('P4', chr(10), n, ' ', n, chr(10));
  Flush(output);

  dimx := Ceil(n / 8);
  TextBuf := GetMem(dimx*n);

  run;
  freemem(textbuf);
end.