--  The Computer Language Benchmarks Game
--  http://benchmarksgame.alioth.debian.org/
--
--  Restarted from RegexDNA program, Georg Bauhaus in March 2017
--
--  This version uses the GNAT Spitbol Pattern matching libraries
--  rather than the more commonly used Unix-style regex libraries.

with GNAT.Spitbol.Patterns;     use GNAT.Spitbol.Patterns,
                                    GNAT.Spitbol;
with U;
package DNA is

   subtype Variant_Index is Positive range 1 .. 9;
   Variant_Labels : constant array (Variant_Index) of VString := (
      V ("agggtaaa|tttaccct"),
      V ("[cgt]gggtaaa|tttaccc[acg]"),
      V ("a[act]ggtaaa|tttacc[agt]t"),
      V ("ag[act]gtaaa|tttac[agt]ct"),
      V ("agg[act]taaa|ttta[agt]cct"),
      V ("aggg[acg]aaa|ttt[cgt]ccct"),
      V ("agggt[cgt]aa|tt[acg]accct"),
      V ("agggta[cgt]a|t[acg]taccct"),
      V ("agggtaa[cgt]|[acg]ttaccct"));

   Variant_Patterns : constant array (Variant_Index) of Pattern :=
     ( --  corresponding alternations in SPITBOL notation
       1 => ((BreakX ("a") & "agggtaaa") or
             (BreakX ("t") & "tttaccct") or
             Cancel),
       2 => ((BreakX ("cgt") & Any ("cgt") & "gggtaaa") or
             (BreakX ("t") & "tttaccc" & Any ("acg")) or
             Cancel),
       3 => ((BreakX ("a") & "a" & Any ("act") & "ggtaaa") or
             (BreakX ("t") & "tttacc" & Any ("agt") & "t") or
             Cancel),
       4 => ((BreakX ("a") & "ag" & Any ("act") & "gtaaa") or
             (BreakX ("t") & "tttac" & Any ("agt") & "ct") or
             Cancel),
       5 => ((BreakX ("a") & "agg" & Any ("act") & "taaa") or
             (BreakX ("t") & "ttta" & Any ("agt") & "cct") or
             Cancel),
       6 => ((BreakX ("a") & "aggg" & Any ("acg") & "aaa") or
             (BreakX ("t") & "ttt" & Any ("cgt") & "ccct") or
             Cancel),
       7 => ((BreakX ("a") & "agggt" & Any ("cgt") & "aa") or
             (BreakX ("t") & "tt" & Any ("acg") & "accct") or
             Cancel),
       8 => ((BreakX ("a") & "agggta" & Any ("cgt") & "a") or
             (BreakX ("t") & "t" & Any ("acg") & "taccct") or
             Cancel),
       9 => ((BreakX ("a") & "agggtaa" & Any ("cgt")) or
             (BreakX ("acg") & Any ("acg") & "ttaccct") or
             Cancel));

   type IubSub is
      record
         Element     : Pattern;
         Replacement : VString;
      end record;

   Iub : constant array (1 .. 5) of IubSub :=
     --  tHa[Nt]
     (("tHa" & Any ("Nt"), V ("<4>")),

      --  aND|caN|Ha[DS]|WaS
      ("aND" or "caN" or ("Ha" & Any ("DS")) or "WaS", V ("<3>")),

      --  a[NSt]|BY
      (("a" & Any ("NSt")) or "BY", V ("<2>")),

      --  A POSIX quantifier "*" attached to a character class means greedy
      --  matching.  In SPITBOL, a quantified character class with greed added
      --  is the realm of SPAN, and similarly of BREAK in case of negation.

      --  <[^>]*>
      ("<" & Break (">") & ">", V ("|")),

      --   \|[^|][^|]*\|
      ("|" & NotAny ("|") & Break ("|") & "|",  V ("-")));

   Seq : U.String_Access;
end DNA;

with Ada.Strings.Unbounded;
package U renames Ada.Strings.Unbounded;

with Ada.Text_IO;          use Ada.Text_IO;
with Ada.Integer_Text_IO;  use Ada.Integer_Text_IO;
with GNAT.Spitbol;         use GNAT.Spitbol;

with DNA.Matching;
with DNA.Replacing;        use DNA;
with Block_Input;
with Preprocessing;

with U;

procedure Regexredux is

   Initial_Length, Code_Length, Processed_Length : Natural;
   Input_Text                                    : U.String_Access;

begin  -- Regexredux

   Ada.Integer_Text_IO.Default_Width := 1; --  format output number display

   --  Read FASTA Sequence
   Block_Input.Open_Stdin;
   Input_Text := Block_Input.Read;
   Block_Input.Close_Stdin;

   Initial_Length := Input_Text'Length;

   DNA.Seq := new String (1 .. Initial_Length);
   --  remove unwanted elements
   declare
      Cleaner : Preprocessing.Removal (Input_Text);
   begin
      Cleaner.Run (Clean => Seq);
      Cleaner.Done (Last => Code_Length);
      U.Free (Input_Text);
   end;

   DNA.Matching.Count_Matches (Seq, Limit => Code_Length);

   --  print counts for patterns
   for Variant in Variant_Index loop
      Put (S (Variant_Labels (Variant)) & " ");
      Put (Item => DNA.Matching.Get (Variant));
      New_Line;
   end loop;

   --  perform replacements and get the new length
   DNA.Replacing.Perform_Replacements
     (Limit      => Code_Length,
      New_Length => Processed_Length);

   New_Line;
   Put (Item => Initial_Length);
   New_Line;
   Put (Item => Code_Length);
   New_Line;
   Put (Item => Processed_Length);
   New_Line;

end Regexredux;

with U;
package Preprocessing is

   --  removal of line feeds and FASTA sequence descriptions

   Separator  : constant String := (1 => ASCII.LF);

   task type Removal (Input_Text : access constant String) is
      pragma Storage_Size (2**16);
      entry Run (Clean : U.String_Access);
      entry Done (Last : out Natural);
      --  number of characters after removal
   end Removal;

end Preprocessing;


package DNA.Matching is

   procedure Count_Matches (Seq : U.String_Access; Limit : Positive);

   function Get (Variant : Variant_Index) return Natural;

end DNA.Matching;

package DNA.Replacing is
   Number_Of_Tasks : constant Positive := 4;

   procedure Perform_Replacements
     (Limit      : Positive;
      New_Length : out Natural);
end DNA.Replacing;


with GNAT.Spitbol.Patterns;    use GNAT.Spitbol.Patterns;

package body Preprocessing is

   task body Removal is

      Sequence : U.String_Access;
      Start,
        Stop   : aliased Natural := 0;
      Last     : Natural         := 0;
      Tail     : Natural         := 0;
      --  Tail is also the value for Removal.Done.Last

      function Transfer return Boolean is
         --  puts good substrings in the resulting sequence
      begin
         if Start > Last then
            Sequence (Tail + 1 ..
                      Tail + 1 + (Start - Last) - 1) :=
              Input_Text (Last + 1 .. Start);
            Tail := Tail + (Start - Last);
         end if;
         Last := Stop;
         return Stop >= Input_Text'Length;
      end Transfer;

      Unwanted : constant Pattern :=
        (Setcur (Start'Access)
           & (('>' & Break (Separator)) or Separator)
           & Setcur (Stop'Access)
           & (+Transfer'Unrestricted_Access));

   begin
      accept Run (Clean : U.String_Access) do
         Sequence := Clean;
      end Run;

      Match (Input_Text.all, Pat => Unwanted);

      accept Done (Last : out Natural) do
         Last := Tail;
      end Done;
   end Removal;

end Preprocessing;

with Ada.Unchecked_Conversion, Ada.Synchronous_Task_Control;
package body DNA.Matching is

   task type Service (Sequence : U.String_Access) is
      --  matches one pattern concurrently

      entry Match_Variant (Variant : Variant_Index);
      entry Get (Number : out Natural);
   end Service;

   package Sem renames Ada.Synchronous_Task_Control;

   Ready         : Sem.Suspension_Object;
   Done          : Boolean                          := False with Volatile;
   No_Of_Matches : array (Variant_Index) of Integer := (others => -1);

   function Get (Variant : Variant_Index) return Natural is
   begin
      if not Done then
         Sem.Suspend_Until_True (Ready);
         Done := True;
      end if;
      return No_Of_Matches (Variant);
   end Get;

   procedure Count_Matches (Seq : U.String_Access; Limit : Positive) is
      subtype P is U.String_Access (1 .. Limit);
      function To_P is new Ada.Unchecked_Conversion (U.String_Access, P);
      Worker : array (Variant_Index) of Service (Sequence => To_P (Seq));
   begin
      --  assign tasks
      for Variant in Variant_Index loop
         Worker (Variant).Match_Variant (Variant);
      end loop;
      for Variant in Variant_Index loop
         Worker (Variant).Get (Number => No_Of_Matches (Variant));
      end loop;
      Sem.Set_True (Ready);
   end Count_Matches;

   task body Service is
      Count : Natural;

      function Inc_Count return Boolean is
         --  another occurrence of a pattern
      begin
         Count := Count + 1;
         return False;
      end Inc_Count;

      Variant : Variant_Index;
   begin  --  Service
      accept Match_Variant (Variant : Variant_Index) do
         Service.Variant := Variant;
      end Match_Variant;

      Count := 0;
      Match (Subject => Sequence.all,
             Pat => (Variant_Patterns (Variant)
                       & (+Inc_Count'Unrestricted_Access)));

      accept Get (Number : out Natural) do
         Number := Count;
      end Get;
   end Service;

end DNA.Matching;

with Ada.Unchecked_Deallocation;
with U;
package body DNA.Replacing is

   function Find_UB (Est, Ub : Positive) return Positive;
   --  position in Seq safe for splitting

   task type Service
     (Sequence : access String; From, To : Natural; Bordering : Boolean)
     --  Perform substitutions for matches between From and To.  If Bordering,
     --  then use the last of the patterns in DNA.Iub, otherwise the ones
     --  preceding it, in sequence.
   is
      entry Save (Pointer : out U.String_Access; Last : out Natural);
      --  Pointer at new text, which runs up to Last.
   end Service;

   procedure Perform_Replacements
     (Limit : Positive; New_Length : out Natural)
   is
      type Worker is access Service;
      Here  : Positive;
      There : Natural;
      Sz    : constant Positive := (Limit + Number_Of_Tasks) / Number_Of_Tasks;
      Work  : array (1 .. Number_Of_Tasks) of Worker;
   begin
      There := 0;
      for N in 1 .. Number_Of_Tasks loop
         Here     := There + 1;
         There    := Find_UB (N*Sz, Limit);
         Work (N) := new Service
           (Sequence => Seq, From => Here, To => There, Bordering => False);
      end loop;
      --  concatenate the buffers and perform the final replacements in that:
      declare
         Wipe    : Worker;
         Scratch : U.String_Access;
         procedure Free is new Ada.Unchecked_Deallocation (Service, Worker);
      begin
         Here  := 1;
         There := 0;
         for Job of Work loop
            Here := Here + There;
            Job.Save (Scratch, There);
            Seq (Here .. Here+There-1) := Scratch (1 .. There);
            Free (Job);
         end loop;
         Wipe := new Service (Sequence => Seq, From => 1, To => Here+There-1,
                              Bordering => True);
         Wipe.Save (Scratch, There);
      end;
      New_Length := There;
   end Perform_Replacements;

   function Safe_Split (Near : String) return Natural is
      N          : aliased Natural;
      Looking_At : constant Pattern :=
        ((Break ("A") & Setcur (N'Access))
           or
         (Break ("a") & Setcur (N'Access) & "aaa"));
   begin
      if Match (Near, Pat => Looking_At) then
         return Near'First + N;
      end if;
      raise Constraint_Error with "cannot safely split up seq";
   end Safe_Split;

   function Find_UB (Est, Ub : Positive) return Positive is
      Limit : constant Natural  := Positive'Min (Est + 1000, Ub);
   begin
      if Est < Ub then
         return Safe_Split (Seq (Est .. Limit));
      else
         return Ub;
      end if;
   end Find_UB;

   task body Service is
      Sub : array (Boolean) of U.String_Access;  --  flipping buffers
      Rpl : U.String_Access;

      --  framing matches and replacements so far:
      Tail  : Positive;
      Start : Positive;
      Hit   : aliased Natural;
      Stop  : aliased Natural;

      Source : Boolean;

      function Last_Repl return Boolean is
         L : constant Natural := Stop - Hit;
      begin
         Sub (not Source)(Tail .. Tail+L-1) := Sub (Source) (Hit+1 .. Hit+L);
         Tail := Tail+L;
         return True;
      end Last_Repl;

      function Next_Repl return Boolean is
         Dest : U.String_Access renames Sub (not Source);
         L1   : constant Natural := Hit - Start + 1;
      begin
         pragma Assert (Tail'Valid);

         Dest (Tail .. Tail+L1-1)           := Sub (Source) (Start .. Hit);
         Dest (Tail+L1
                 .. Tail+L1+Rpl'Length - 1) := Rpl.all;
         Tail                               := Tail + L1 + Rpl'Length;
         Start                              := Stop + 1;
         return False;
      end Next_Repl;

      procedure Run_Matcher (Iub_Pattern : Pattern; Ub : Positive) is

         function Ge return Boolean is (Hit >= Stop);

         Suffix : constant Pattern :=
           (Tab (Stop'Access)
              & Setcur (Hit'Access)
              & Rest
              & Setcur (Stop'Access)
              & (+Last_Repl'Unrestricted_Access));

         Code : constant Pattern :=
           (Setcur (Hit'Access)
              & (+Ge'Unrestricted_Access)
              & Iub_Pattern
              & Setcur (Stop'Access)
              & (+Next_Repl'Unrestricted_Access));
      begin
         Stop  := 0;
         Tail  := 1;
         Start := Sub (Source)'First;
         Match (Sub (Source) (1 .. Ub), Pat => Code);
         Match (Sub (Source) (1 .. Ub), Pat => Suffix);
      end Run_Matcher;

      Ub : Positive;
      Need : constant Positive := 1 + Natural (1.3 * Float (To-From+1));
   begin                              --  Replacement
      Source := True;
      Ub     := To-From+1;
      if Bordering then         -- Sequence is the concatenation
         Sub (Source)     := Sequence;
         Sub (not Source) := new String (1 .. To-From+1);
         Rpl              := new String'(S (Iub (Iub'Last).Replacement));
         Run_Matcher (Iub (Iub'Last).Element, Ub);
         Ub               := Tail - 1;
         Source           := not Source;
      else
         Sub (True)  := new String (1 .. Need);
         Sub (False) := new String (1 .. Need);
         Sub (Source) (1 .. (To-From+1)) := Sequence (From .. To);
         for Job in Iub'First .. Iub'Last-1 loop
            Rpl    := new String'(S (Iub (Job).Replacement));
            Run_Matcher (Iub (Job).Element, Ub);
            Ub     := Tail - 1;
            Source := not Source;
         end loop;
      end if;
      U.Free (Sub (not Source));
      accept Save (Pointer : out U.String_Access; Last : out Natural) do
         Pointer := Sub (Source);
         Last    := Ub;
      end Save;
   end Service;

end DNA.Replacing;

with U;
package Block_Input is

   function Read return U.String_Access;
   procedure Open_Stdin;
   procedure Close_Stdin;

end Block_Input;

with Ada.Streams.Stream_IO;
with Interfaces.C_Streams;

package body Block_Input is

   use Ada.Streams;

   cin : Stream_IO.File_Type;

   function Read return U.String_Access is
      use Interfaces.C_Streams;
      Items_To_Read : Stream_Element_Offset;
      Items_Read    : Stream_Element_Offset;
      Buffer        : U.String_Access;
   begin
      if fseek (stdin, 0, SEEK_END) /= -1 then
         Items_To_Read := Stream_Element_Offset (ftell (stdin));
         rewind (stdin);
         Buffer := new String (1 .. Positive (Items_To_Read));
         declare
            View : Stream_Element_Array (1 .. Items_To_Read);
            pragma Import (Ada, View);
            for View'Address use Buffer.all'Address;
         begin
            Stream_IO.Read (File => cin,
                            Item => View,
                            Last => Items_Read);
         end;
      end if;
      return Buffer;
   end Read;

   procedure Open_Stdin is
   begin
      Stream_IO.Open
        (File => cin,
         Mode => Stream_IO.In_File,
         Name => "/dev/stdin");
   end Open_Stdin;

   procedure Close_Stdin is
   begin
      Stream_IO.Close (cin);
   end Close_Stdin;

end Block_Input;