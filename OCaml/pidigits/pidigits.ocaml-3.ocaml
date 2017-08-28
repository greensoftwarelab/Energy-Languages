(* The Computer Language Benchmarks Game
 * http://benchmarksgame.alioth.debian.org/
 *
 * contributed by Christophe TROESTLER
 * modified by Matías Giovannini
 * modified by Ethan Burns
 *)

open Big_int

type lft = big_int * big_int * big_int * big_int
type lft_ints = int * int * int * int

let unit:lft = (unit_big_int, zero_big_int, zero_big_int, unit_big_int)

(* Attempt to avoid the (apparently slow) Big_int module in some
   simple cases... seems to improve performance 30%. *)

let ( */ ) a b =
  if b = 0 then
    zero_big_int
  else if b = 1 then
    a
  else if b = 2 then
    add_big_int a a
  else
    mult_int_big_int b a

let ( +/ ) a b =
  if eq_big_int a zero_big_int then
    b
  else if eq_big_int b zero_big_int then
    a
  else
    add_big_int a b

let ( // ) a b =
  if lt_big_int a b then
    zero_big_int
  else if eq_big_int a b then
    unit_big_int
  else
    div_big_int a b

let extr (q, r, s, t) x =
  int_of_big_int ((q */ x +/ r) // (s */ x +/ t))

let comp ((q, r, s, t) : lft) ((u, v, w, x) : lft_ints) : lft =
  q */ u +/ r */ w,
  q */ v +/ r */ x,
  s */ u +/ t */ w,
  s */ v +/ t */ x

let comp' ((q, r, s, t) : lft_ints) ((u, v, w, x) : lft) : lft =
  u */ q +/ w */ r,
  v */ q +/ x */ r,
  u */ s +/ w */ t,
  v */ s +/ x */ t

let stream next safe prod cons ith_x z_0 i_0 num =
  let buf = Buffer.create (num * 2) in
  let col = ref 0 in
  let z_int = int_of_char '0' in
  let end_row left =
    col := 0;
    Buffer.add_string buf "\t:";
    Buffer.add_string buf (string_of_int (num - left));
    Buffer.add_char buf '\n'; in
  let next_digit left d =
    Buffer.add_char buf (char_of_int (d + z_int));
    incr col;
    if (!col = 10) then end_row left in
  let rec do_stream z i left =
    if left > 0 then
      let y = next z in
	if safe z y then begin
	  let left' = left - 1 in
	    next_digit left' y;
	    do_stream (prod z y) i left'
	end else begin
	  do_stream (cons z (ith_x i)) (i + 1) left
	end
  in
    do_stream z_0 i_0 num;
    if !col <> 0 then begin
      Buffer.add_string buf (String.make (10 - !col) ' ');
      end_row 0;
    end;
    print_string (Buffer.contents buf);
    print_newline

let pi num =
  let init = unit in
  let lfts k = let x = 2 * k + 1 in k, 2 * x, 0, x in
  let next z = extr z 3 in
  let safe z n = n = (extr z 4) in
  let prod z n = comp' (10, ~-10 * n, 0, 1) z in
  let cons z z' = comp z z' in
    stream next safe prod cons lfts init 1 num

let main () =
  let num =
    if Array.length Sys.argv < 2 then
      27
    else
      try int_of_string Sys.argv.(1) with _ -> 27
  in
    pi num

let _ = main ()