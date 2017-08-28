(* The Computer Language Benchmarks Game
   http://benchmarksgame.alioth.debian.org/

   contributed by Jean de La Taille
*)

program project1;

uses
  {$IFDEF UNIX}
  cthreads,
  {$ENDIF}
  SysUtils, DateUtils, Math;

type
  arrWord = array[0..12] of Word;
  threadData = record
    num, n, count, checksum : longint;
    list, counters, limits : arrWord;
  end;

(******************************************************************************)

(* Thread *)
function run(p : pointer) : ptrint;
var
  i, k, l, (*n,*) num : longint;
  count, maxCount, checksum : longint;
  f : boolean;
  list, counters, limits : arrWord;

  // Flip function
  function flip : longint; inline;
  var
    count, f, i, j, k, tmp : longint;
    tmpList : arrWord;
  begin
    tmpList := list;
    count := 0;
    // While the head list is not 1, do lot of reverse
    f := tmpList[0];
    while (f <> 1) do
    begin
      // Reverse
      j := f >> 1;
      k := f - 1;
      for i := 0 to j - 1 do
      begin
        tmp := tmpList[i];
        tmpList[i] := tmpList[k];
        tmpList[k] := tmp;
        Dec(k);
      end;
      f := tmpList[0];
      // End of reverse
      Inc(count);
    end;
    flip := count;
  end;
  // Swap function
  procedure swap(var a, b : word); inline;
  begin
    l := a;
    a := b;
    b := l;
  end;
  // Roll3 function
  procedure roll3(var a, b, c : word); inline;
  begin
    l := a;
    a := b;
    b := c;
    c := l;
  end;
  // Roll function
  procedure roll(k : longint); inline;
  var
    j : longint;
  begin
    l := list[0];
    for j := 0 to k do
      list[j] := list[j + 1];
    list[j] := l;
  end;

begin
  /// n := threadData(p^).n;
  num := threadData(p^).num;
  limits := threadData(p^).limits;
  counters := threadData(p^).counters;
  list := threadData(p^).list;
  /// WriteLn(n, ' ', num, ' ', list[0]);
  // Main loop
  f := false;
  maxCount := 0;
  checksum := 0;
  for i := 1 to num do
  begin
    count := flip;
    // Check if the number of reverse is the max
    if (count > maxCount) then
      maxCount := count;
    // Compute checksum
    checksum := count - checksum;
    // Swap
    swap(list[0], list[1]);
    // If needed, roll 3
    if (f) then
    begin
      // Roll 3
      roll3(list[0], list[1], list[2]);
      k := 3;
      Dec(counters[3]);
      // If needed, roll next
      while ((counters[k] = 0)) do
      begin
        counters[k] := limits[k];
        roll(k);
        Inc(k);
        Dec(counters[k]);
      end;
    end;
    f := not f;
  end;
  threadData(p^).checksum := checksum;
  threadData(p^).count := maxCount;
  run := 0;
end;

(* Main routine, to launch threads *)
procedure launch(n : longint);
var
  //start, finish : TDateTime;
  list, counters, limits : arrWord;
  i, l, num, count, checksum : longint;
  tt : array of TThreadID;
  td : array of threadData;

  // Roll function
  procedure roll(k : longint); inline;
  var
    j : longint;
  begin
    l := list[0];
    for j := 0 to k do
      list[j] := list[j + 1];
    list[j] := l;
  end;

begin
  /// start := now;
  SetLength(tt, n);
  SetLength(td, n);
  // Inits the arrays
  num := 1;
  for i := 0 to n - 1 do
  begin
    limits[i] := i;
    counters[i] := i;
    list[i] := i + 1;
    num := num * (i + 1);
  end;
  num := num div n;
  // Launch threads
  for i := 0 to n - 1 do
  begin
    td[i].n := n;
    td[i].num := num;
    td[i].list := list;
    td[i].counters := counters;
    td[i].limits := limits;
    tt[i] := BeginThread(@run, @td[i]);
    roll(n - 1);
  end;
  // Wait threads
  checksum := 0;
  count := 0;
  for i := 0 to n - 1 do
  begin
    WaitForThreadTerminate(tt[i], 0);
    count := max(count, td[i].count);
    if ((n and 1) = 0) then
      checksum := td[i].checksum + checksum
    else
      checksum := checksum - td[i].checksum;
  end;
  /// finish := now;
  /// WriteLn('Time : ', (MilliSecondsBetween(start, finish) / 1000) : 0 : 4);
  WriteLn(abs(checksum));
  WriteLn('Pfannkuchen(', n, ') = ', count);
end;

(* Main program *)
begin
  if (argc > 1) then
    launch(StrToInt(argv[1]))
  else
    launch(4);
end.