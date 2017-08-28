(*
 * The Computer Language Benchmarks Game
 * http://benchmarksgame.alioth.debian.org/
 *
 * Contributed by Paolo Ribeca, August 2011
 *
 * (Based on the Java version by Oleg Mazurov)
 *)

let workers = 32

module Perm =
  struct
    type t = { p: int array;
               pp: int array;
               c: int array }
  let facts =
    let n = 20 in
    let res = Array.make (n + 1) 1 in
    for i = 1 to n do
      res.(i) <- i * res.(i - 1)
    done;
    res
  (* Setting up the permutation based on the given index *)
  let setup n idx =
    let res = { p = Array.init n (fun i -> i);
                pp = Array.make n 1;
                c = Array.make n 1 }
    and idx = ref idx in
    for i = n - 1 downto 0 do
      let d = !idx / facts.(i) in
      res.c.(i) <- d;
      idx := !idx mod facts.(i);
      Array.blit res.p 0 res.pp 0 (i + 1);
      for j = 0 to i do
        res.p.(j) <- if j + d <= i then res.pp.(j + d) else res.pp.(j + d - i - 1)
      done
    done;
    res
  (* Getting the next permutation *)
  let next { p = p; c = c } =
    let f = ref p.(1) in
    p.(1) <- p.(0);
    p.(0) <- !f;
    let i = ref 1 in
    while let aug_c = c.(!i) + 1 in c.(!i) <- aug_c; aug_c > !i do
      c.(!i) <- 0;
      incr i;
      let n = p.(1) in
      p.(0) <- n;
      let red_i = !i - 1 in
      for j = 1 to red_i do
        p.(j) <- p.(j + 1)
      done;
      p.(!i) <- !f;
      f := n
    done
  (* Counting the number of flips *)
  let count { p = p; pp = pp } =
    let f = ref p.(0) and res = ref 1 in
    if p.(!f) <> 0 then begin
      let len = Array.length p in
      let red_len = len - 1 in
      for i = 0 to red_len do pp.(i) <- p.(i) done;
      while pp.(!f) <> 0 do
        incr res;
        let lo = ref 1 and hi = ref (!f - 1) in
        while !lo < !hi do
          let t = pp.(!lo) in
          pp.(!lo) <- pp.(!hi);
          pp.(!hi) <- t;
          incr lo;
          decr hi
        done;
        let ff = !f in
        let t = pp.(ff) in
        pp.(ff) <- ff;
        f := t
      done
    end;
    !res
  end

let _ =
  let n = int_of_string Sys.argv.(1) in
  let chunk_size = Perm.facts.(n) / workers
  and rem = Perm.facts.(n) mod workers in
  let w = Array.make workers stdin
  and red_workers = workers - 1 in
  for i = 0 to red_workers do
    let lo = i * chunk_size + min i rem in
    let hi = lo + chunk_size + if i < rem then 1 else 0
    and input, output = Unix.pipe () in
    match Unix.fork () with
    | 0 ->
      let p = Perm.setup n lo
      and c = ref 0 and m = ref 0
      and red_hi = hi - 1 in
      for j = lo to red_hi do
        let r = Perm.count p in
        c := !c + r * (1 - (j land 1) lsl 1);
        if r > !m then
        m := r;
        Perm.next p
      done;
      let output = Unix.out_channel_of_descr output in
      output_binary_int output !c;
      output_binary_int output !m;
      exit 0
    | _ ->
      Unix.close output;
      w.(i) <- Unix.in_channel_of_descr input
  done;
  let c = ref 0 and m = ref 0 in
  Array.iter
    (fun input ->
      c := !c + input_binary_int input;
      m := max !m (input_binary_int input))
    w;
  Printf.printf "%d\nPfannkuchen(%d) = %d\n%!" !c n !m