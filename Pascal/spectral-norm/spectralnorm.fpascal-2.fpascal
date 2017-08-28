{ The Computer Language Benchmarks Game
  http://benchmarksgame.alioth.debian.org

  contributed by Ian Osgood
  modified by Vincent Snijders
  modified by Peter Blackman
}

program spectralnorm;

uses cthreads, cmem, mtprocs;

type
    aod = array of double;
    td =
    record
        w1 : ^aod;
        w2 : ^aod;
    end;

var
    n,i : ptrint;
    u,v,tmp : aod;
    vBv,vv : double;
    w  : td;
    wp : ^td;


function A(i,j : ptrint): double; inline;
begin
    A := 1 / ((i+j)*(i+j+1) div 2 + i+1);
end;


procedure mulAv(i : ptrint; Data: Pointer; Item: TMultiThreadProcItem);
var j : ptrint;
    q : double;
begin
    q := 0;
    for j := 0 to n-1 do
        q := q + A(i,j) * td(Data^).w1^[j];

    td(Data^).w2^[i] := q;
end;

procedure mulAtv(i : ptrint; Data: Pointer; Item: TMultiThreadProcItem);
var j : ptrint;
    q : double;
begin
    q := 0;
    for j := 0 to n-1 do
        q := q + A(j,i) * td(Data^).w1^[j];

    td(Data^).w2^[i] := q;
end;


procedure mulAtAv (AtA1, AtA2 : aod);
begin
    w.w1 := @AtA1;
    w.w2 := @tmp;
    ProcThreadPool.DoParallel(@mulAv,  0, n-1, wp);

    w.w1 := @tmp;
    w.w2 := @AtA2;
    ProcThreadPool.DoParallel(@mulAtv, 0, n-1, wp);
end;


begin
    Val(paramstr(1), n, i);
    SetLength(u, n);
    SetLength(v, n);
    SetLength(tmp, n);

    vBv := 0;
    vv  := 0;
    wp  := @w;

    for i := 0 to n-1 do
        u[i] := 1.0;

    for i := 1 to 10 do
    begin
        mulAtAv (u,v);
        mulAtAv (v,u);
    end;

    for i := 0 to n-1 do
    begin
        vBv := vBv + u[i]*v[i];
        vv  := vv  + v[i]*v[i];
    end;

    writeln(sqrt(vBv/vv):0:9);
end.