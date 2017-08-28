(* The Computer Language Benchmarks Game
 * http://benchmarksgame.alioth.debian.org/
 *
 * Contributed by Paolo Ribeca
 *)

let chars_per_line = 60
and lines_per_worker =
  match Sys.word_size with
  | 32 -> 200000
  | 64 -> 500000
  | _ -> assert false

let rc_table = String.make 256 '\000'
let _ =
  rc_table.[Char.code 'A'] <- 'T'; rc_table.[Char.code 'T'] <- 'A';
  rc_table.[Char.code 'w'] <- 'W'; rc_table.[Char.code 's'] <- 'S';
  rc_table.[Char.code 'a'] <- 'T'; rc_table.[Char.code 't'] <- 'A';
  rc_table.[Char.code 'C'] <- 'G'; rc_table.[Char.code 'G'] <- 'C';
  rc_table.[Char.code 'c'] <- 'G'; rc_table.[Char.code 'g'] <- 'C';
  rc_table.[Char.code 'U'] <- 'A'; rc_table.[Char.code 'u'] <- 'A';
  rc_table.[Char.code 'M'] <- 'K'; rc_table.[Char.code 'K'] <- 'M';
  rc_table.[Char.code 'm'] <- 'K'; rc_table.[Char.code 'k'] <- 'M';
  rc_table.[Char.code 'R'] <- 'Y'; rc_table.[Char.code 'Y'] <- 'R';
  rc_table.[Char.code 'r'] <- 'Y'; rc_table.[Char.code 'y'] <- 'R';
  rc_table.[Char.code 'W'] <- 'W'; rc_table.[Char.code 'S'] <- 'S';
  rc_table.[Char.code 'w'] <- 'W'; rc_table.[Char.code 's'] <- 'S';
  rc_table.[Char.code 'V'] <- 'B'; rc_table.[Char.code 'B'] <- 'V';
  rc_table.[Char.code 'v'] <- 'B'; rc_table.[Char.code 'b'] <- 'V';
  rc_table.[Char.code 'H'] <- 'D'; rc_table.[Char.code 'D'] <- 'H';
  rc_table.[Char.code 'h'] <- 'D'; rc_table.[Char.code 'd'] <- 'H';
  rc_table.[Char.code 'N'] <- 'N'; rc_table.[Char.code 'n'] <- 'N'

let _ =
  let aug_chars_per_line = chars_per_line + 1
  and in_ack, out_ack = Unix.pipe () and in_end, out_end = Unix.pipe ()
  and put out_pipe () =
    if Unix.write out_pipe " " 0 1 <> 1 then
      failwith "Pipe problem"
  and get in_pipe () =
    let res = " " in
    if Unix.read in_pipe res 0 1 <> 1 then
      failwith "Pipe problem" in
  let put_ack = put out_ack and get_ack = get in_ack
  and put_end_ack = put out_end and get_end_ack = get in_end in
  let rec spawn tag beg first =
    let output_tag () =
      print_string tag;
      print_char '\n';
      flush stdout
    and buf = String.create (lines_per_worker * chars_per_line + 2)
    and len = ref (String.length beg) in
    String.blit beg 0 buf 0 !len;
    let process_buffer () =
      let red_len = !len - 1 in
      let mid_point = red_len / 2 in
      for i = 0 to mid_point do
	let ri = red_len - i and tmp = buf.[i] in
	buf.[i] <- rc_table.[Char.code buf.[ri]];
	buf.[ri] <- rc_table.[Char.code tmp]
      done
    and write_by_cols rem eol =
      let len = !len and dne = ref 0 in
      if rem > 0 then begin
	let to_do = min rem (len - !dne) in
	output stdout buf !dne to_do;
	output_char stdout '\n';
	dne := !dne + to_do
      end;
      while len - !dne >= chars_per_line do
	output stdout buf !dne chars_per_line;
	output_char stdout '\n';
	dne := !dne + chars_per_line
      done;
      let rem = len - !dne in
      if rem > 0 then begin
	output stdout buf !dne rem;
	if eol then
	  output_char stdout '\n'
      end;
      flush stdout;
      if eol then
	0
      else
	rem in
    try
      for i = 2 to lines_per_worker do
	really_input stdin buf !len aug_chars_per_line;
	let new_len = ref (!len + chars_per_line) in
	if buf.[!len] = '>' || buf.[!new_len] <> '\n' then begin
	  while buf.[!len] <> '>' do
	    incr len
	  done;
	  let ptr = ref !len in
	  (* Needed to patch the hideous bug in the output of the C program *)
	  if buf.[!len - 1] <> '\n' then begin
	    String.blit buf !len buf (!len + 1) aug_chars_per_line;
	    buf.[!len] <- '\n';
	    incr new_len;
	    incr ptr
	  end else
	    decr len;
	  while !ptr < !new_len && buf.[!ptr] <> '\n' do
	    incr ptr
	  done;
	  match Unix.fork () with
	  | 0 ->
	      let aug_len = !len + 1 in
	      if !ptr = !new_len then
		spawn
		  (String.sub buf
		    aug_len (!new_len - aug_len) ^ input_line stdin)
		  "" true
	      else
		let aug_ptr = !ptr + 1 in
		spawn
		  (String.sub buf aug_len (!ptr - aug_len))
		  (String.sub buf aug_ptr (!new_len - !ptr) ^ input_line stdin)
		  true
	  | _ ->
	      get_ack ();
	      output_tag ();
	      process_buffer ();
	      let rem = write_by_cols 0 first in
	      if first then
		put_ack ();
	      exit rem
	end;
	len := !new_len
      done;
      match Unix.fork () with
      | 0 -> spawn tag "" false
      | pid ->
	  process_buffer ();
	  match Unix.waitpid [] pid with
	  | _, Unix.WEXITED rem ->
	      let rem = write_by_cols (chars_per_line - rem) first in
	      if first then
		put_ack ();
	      exit rem
	  | _ -> assert false
    with End_of_file ->
      while buf.[!len] <> '\n' do
	incr len
      done;
      get_ack ();
      put_end_ack ();
      output_tag ();
      process_buffer ();
      let rem = write_by_cols 0 first in
      if first then
	put_ack ();
      exit rem in
  match Unix.fork () with
  | 0 ->
      put_ack ();
      spawn (read_line ()) "" true
  | _ ->
      get_end_ack ();
      get_ack ();
      exit 0