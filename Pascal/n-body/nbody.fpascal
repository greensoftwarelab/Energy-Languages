{ The Computer Language Benchmarks Game
  http://benchmarksgame.alioth.debian.org

  contributed by Ian Osgood,
  modified by Florian Klaempfl
  modified by Ales Katona
  modified by Vincent Snijders
}

{$mode objfpc}

program n_body;

uses Math;

type
  Body = record
    x, y, z,
    vx, vy, vz,
    mass : double;
  end;
  PBody = ^Body;

const pi = 3.141592653589793;
      solarMass = 4 * sqr(pi);
      daysPerYear = 365.24;

type
  tbody = array[1..5] of Body;

const b : tbody = (
  { Sun }
  ( x:0; y:0; z:0;  vx:0; vy:0; vz:0;  mass: solarMass ),
  { Jupiter }
  ( x:    4.84143144246472090e+00;
    y:   -1.16032004402742839e+00;
    z:   -1.03622044471123109e-01;
    vx:   1.66007664274403694e-03 * daysPerYear;
    vy:   7.69901118419740425e-03 * daysPerYear;
    vz:  -6.90460016972063023e-05 * daysPerYear;
    mass: 9.54791938424326609e-04 * solarMass ),
  { Saturn }
  ( x:    8.34336671824457987e+00;
    y:    4.12479856412430479e+00;
    z:   -4.03523417114321381e-01;
    vx:  -2.76742510726862411e-03 * daysPerYear;
    vy:   4.99852801234917238e-03 * daysPerYear;
    vz:   2.30417297573763929e-05 * daysPerYear;
    mass: 2.85885980666130812e-04 * solarMass ),
  { Uranus }
  ( x:    1.28943695621391310e+01;
    y:   -1.51111514016986312e+01;
    z:   -2.23307578892655734e-01;
    vx:   2.96460137564761618e-03 * daysPerYear;
    vy:   2.37847173959480950e-03 * daysPerYear;
    vz:  -2.96589568540237556e-05 * daysPerYear;
    mass: 4.36624404335156298e-05 * solarMass ),
  { Neptune }
  ( x:    1.53796971148509165e+01;
    y:   -2.59193146099879641e+01;
    z:    1.79258772950371181e-01;
    vx:   2.68067772490389322e-03 * daysPerYear;
    vy:   1.62824170038242295e-03 * daysPerYear;
    vz:  -9.51592254519715870e-05 * daysPerYear;
    mass: 5.15138902046611451e-05 * solarMass )
);

procedure offsetMomentum;
var px,py,pz : double;
    i : integer;
begin
  px:=0.0; py:=0.0; pz:=0.0;
  for i := low(b)+1 to high(b) do
    with b[i] do
    begin
      px := px - vx * mass;
      py := py - vy * mass;
      pz := pz - vz * mass;
    end;
  b[low(b)].vx := px / solarMass;
  b[low(b)].vy := py / solarMass;
  b[low(b)].vz := pz / solarMass;
end;

function distance(i,j : integer) : double;
begin
  distance := sqrt(sqr(b[i].x-b[j].x) + sqr(b[i].y-b[j].y) +
sqr(b[i].z-b[j].z));
end;

function energy : double;
var
  i,j : integer;
begin
  result := 0.0;
  for i := low(b) to high(b) do
    with b[i] do
    begin
      result := result + mass * (sqr(vx) + sqr(vy) + sqr(vz)) / 2;
      for j := i+1 to high(b) do
        result := result - mass * b[j].mass / distance(i,j);
    end;
end;

procedure advance(dt : double);
var i,j : integer;
    dx,dy,dz,mag : double;
    bi,bj : PBody;
begin
  bi:=@b[low(b)];
  for i := low(b) to high(b)-1 do begin
    bj := bi;
    for j := i+1 to high(b) do
    begin
      inc(bj);
      dx := bi^.x - bj^.x;
      dy := bi^.y - bj^.y;
      dz := bi^.z - bj^.z;
      mag := dt / (sqrt(sqr(dx)+sqr(dy)+sqr(dz))*(sqr(dx)+sqr(dy)+sqr(dz)));
      bi^.vx := bi^.vx - dx * bj^.mass * mag;
      bi^.vy := bi^.vy - dy * bj^.mass * mag;
      bi^.vz := bi^.vz - dz * bj^.mass * mag;
      bj^.vx := bj^.vx + dx * bi^.mass * mag;
      bj^.vy := bj^.vy + dy * bi^.mass * mag;
      bj^.vz := bj^.vz + dz * bi^.mass * mag;
    end;
    inc(bi);
  end;
  bi:=@b[low(b)];
  for i := low(b) to high(b) do begin
    with bi^ do
    begin
      x := x + dt * vx;
      y := y + dt * vy;
      z := z + dt * vz;
    end;
    inc(bi);
  end;
end;

var i : integer;
    n : Integer;
begin
  SetPrecisionMode(pmDouble);
  offsetMomentum;
  writeln(energy:0:9);
  Val(ParamStr(1), n, i);
  for i := 1 to n do advance(0.01);
  writeln(energy:0:9);
end.