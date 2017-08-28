(* The Computer Language Benchmarks Game
 * http://benchmarksgame.alioth.debian.org/
 *
 * Contributed by Sebastien Loisel
 * Cleanup by Troestler Christophe
 * Parallelized by Mauricio Fernandez
 *)

open Bigarray
type v = (float, float64_elt, c_layout) Array1.t

let max_workers = 8

let map_chunks f low hi =
  let step = (hi - low) / max_workers in
  let rec loop acc n =
    if n < hi then
      let n' = n + step in loop (f (n, (min hi n')) :: acc) n'
    else acc
  in loop [] low

let wait_for = List.iter (fun f -> f())

(* original function due to Jon D. Harrop *)
let invoke (f : 'a -> 'b) x : unit -> 'b =
  let input, output = Unix.pipe() in
  match Unix.fork() with
  | -1 -> Unix.close input; Unix.close output; (let v = f x in fun () -> v)
  | 0 ->
      Unix.close input;
      let output = Unix.out_channel_of_descr output in
        Marshal.to_channel output (try `Res(f x) with e -> `Exn e) [];
        close_out output;
        exit 0
  | pid ->
      Unix.close output;
      let input = Unix.in_channel_of_descr input in fun () ->
        let v = Marshal.from_channel input in
        ignore (Unix.waitpid [] pid);
        close_in input;
        match v with `Res x -> x | `Exn e -> raise e

let eval_A i j = 1. /. float((i+j)*(i+j+1)/2+i+1)

let eval_A_times_u (u : v) (v : v) =
  let n = Array1.dim v - 1 in
  let compute (p, q) =
    for i = p to q do
      let vi = ref 0. in
        for j = 0 to n do vi := !vi +. eval_A i j *. u.{j} done;
        v.{i} <- !vi
    done
  in wait_for (map_chunks (invoke compute) 0 n)

let eval_At_times_u (u : v) (v : v) =
  let n = Array1.dim v - 1 in
  let compute (p, q) =
    for i = p to q do
      let vi = ref 0. in
        for j = 0 to n do vi := !vi +. eval_A j i *. u.{j} done;
        v.{i} <- !vi
    done
  in wait_for (map_chunks (invoke compute) 0 n)

let make_array n x =
  let v = Array1.map_file
            (Unix.openfile "/dev/zero" [Unix.O_RDWR] 0o755)
            float64 c_layout true n in
    for i = 0 to n - 1 do v.{i} <- x done;
    v

let eval_AtA_times_u u v =
  let w = make_array (Array1.dim u) 0.0 in
  eval_A_times_u u w; eval_At_times_u w v

let () =
  let n = try int_of_string(Array.get Sys.argv 1) with _ ->  2000 in
  let u = make_array n 1.0  and  v = make_array n 0.0 in
  for i = 0 to 9 do
    eval_AtA_times_u u v; eval_AtA_times_u v u
  done;

  let vv = ref 0.0  and  vBv = ref 0.0 in
  for i=0 to n-1 do
    vv := !vv +. v.{i} *. v.{i};
    vBv := !vBv +. u.{i} *. v.{i}
  done;
  Printf.printf "%0.9f\n" (sqrt(!vBv /. !vv))