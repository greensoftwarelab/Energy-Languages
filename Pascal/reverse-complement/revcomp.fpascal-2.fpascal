{  The Computer Language Benchmarks Game
   http://benchmarksgame.alioth.debian.org/

   contributed by Marco van de Voort
}

program reverse_complement;

var lookupComplement : array[#0..#255] of char;

Const FASTAXLAT : array[0..11] of array[0..1] of char = (
		  ( 'A', 'T' ), ( 'C', 'G' ),
		  ( 'B', 'V' ), ( 'D', 'H' ),
	          ( 'K', 'M' ), ( 'R', 'Y' ),
		  ( 'a', 't' ), ( 'c', 'g' ),
		  ( 'b', 'v' ), ( 'd', 'h' ),
	          ( 'k', 'm' ), ( 'r', 'y' ));

      BufferIncrement = 1024;

procedure flushbuffer(buffer:pchar;inbuf:longint);

var p,p2 : pchar;
    c  : char;

begin
  if inbuf>0 then
   begin
     p:=buffer;
     p2:=@buffer[inbuf-1];
     while p<p2 do
      begin
       c:=lookupcomplement[p^];
       p^:=lookupcomplement[p2^];
       p2^:=c;
       inc(p);
       dec(p2);
     end;
    if p2=p then
      p^:=lookupcomplement[p^];

    p:=buffer;
    p[inbuf]:=#0;

   while (inbuf > 60) do
     begin
    	c := p[60];
	p[60]:=#0;
        writeln(p);
        p[60]:=c;
	inc(p,60);
	dec(inbuf,60);
     end;
     p[inbuf]:=#0;
     writeln(p);
  end;
end;

const initialincrement=1024;

procedure run;

var s  : string;
    c  : char;
    buffersize,
    bufferptr,
    len		: longint;
    p  :pchar;
    line : integer;
    bufin,bufout : array[0..8191] of char;

begin
  settextbuf(input,bufin);
  settextbuf(output,bufout);
  for c:=#0  to #255  do
    lookupcomplement[c]:=c;
  for len:=0 to high(FASTAXLAT) do
    begin
      lookupcomplement[FASTAXLAT[len][0]]:=upcase(FASTAXLAT[len][1]);
      lookupcomplement[FASTAXLAT[len][1]]:=upcase(FASTAXLAT[len][0]);
    end;
  buffersize:=initialincrement;
  bufferptr :=0;
  getmem(p,buffersize);
  line:=0;
  while not eof do
    begin
      readln(s);
      inc(line);
      len:=length(s);
      if (len>0) and (s[1]='>') then
          begin
	    flushbuffer(p,bufferptr);
 	    writeln(s);
	    bufferptr:=0;
	  end
       else
         begin
           if (bufferptr+len+1)>buffersize then
	     begin
	        inc(buffersize,buffersize);
//	        inc(buffersize,initialincrement);
                reallocmem(p,buffersize);
	     end;
	   move (s[1],p[bufferptr],len);
	   inc(bufferptr,len);
	 end;
    end;
    flushbuffer(p,bufferptr);
end;

begin
  run;
end.