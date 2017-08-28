--  The Computer Language Benchmarks Game
--  http://benchmarksgame.alioth.debian.org/
--
--  Based on Ada versions created by
--    Jim Rogers and Brian Drummons as well as the
--    C version by Francesco Abbate
--
--  Contributed by Brad Moore
--  *reset*

with Trees;                  use Trees;
with Ada.Text_IO;            use Ada.Text_IO;
with Ada.Integer_Text_IO;    use Ada.Integer_Text_IO;
with Ada.Command_Line;       use Ada.Command_Line;
with Ada.Characters.Latin_1; use Ada.Characters.Latin_1;

procedure Binarytrees is

   function Get_Depth return Positive is
   begin
      if Argument_Count > 0 then
         return Positive'Value (Argument (1));
      else
         return 10;
      end if;
   end Get_Depth;

   Min_Depth     : constant := 4;
   Requested_Depth : constant Positive := Get_Depth;
   Max_Depth     : constant Positive := Positive'Max (Min_Depth + 2,
                                                      Requested_Depth);
   Depth_Iterations : constant Positive := (Max_Depth - Min_Depth) / 2 + 1;

   function Get_Worker_Count return Positive is
   begin
      if Argument_Count > 1 then
         return Positive'Value (Argument (2));
      else
         --  This seems to be the sweet spot assuming max depth of 20
         return 5;
      end if;
   end Get_Worker_Count;

   Worker_Count     : constant Positive := Get_Worker_Count;

   task type Depth_Worker
     (Start, Finish : Positive := Positive'Last) is
      pragma Storage_Size (16#100#);
   end Depth_Worker;

   Results : array (1 .. Depth_Iterations) of Integer;
   Iteration_Tracking : array (1 .. Depth_Iterations) of Positive;

   task body Depth_Worker
   is
      Depth         : Natural;
      Check         : Integer;
      Iterations    : Positive;
   begin

      for Depth_Iter in Start .. Finish loop

         Depth := Min_Depth + (Depth_Iter - 1) * 2;
         Iterations := 2 ** (Max_Depth - Depth + Min_Depth);
         Iteration_Tracking (Depth_Iter) := Iterations;

         Check      := 0;

         for I in 1 .. Iterations loop
            declare
               Short_Lived_Pool : Node_Pool;
               Short_Lived_Tree_1, Short_Lived_Tree_2 : Tree_Node;
            begin

               Short_Lived_Tree_1 :=
                 Create
                   (Short_Lived_Pool,
                    Depth => Depth);

               Short_Lived_Tree_2 :=
                  Create
                    (Short_Lived_Pool,
                     Depth => Depth);

               Check := Check +
                 Item_Check (Short_Lived_Tree_1);
            end;
         end loop;

         Results (Depth_Iter) := Check;
      end loop;

   end Depth_Worker;

   subtype Worker_Id is Positive range 1 .. Worker_Count;

   Start_Index         : Positive := 1;
   End_Index           : Positive := Depth_Iterations;

   Iterations_Per_Task : constant Positive :=
     Depth_Iterations / Worker_Count;

   Remainder           : Natural :=
     Depth_Iterations rem Worker_Count;

   function Create_Worker return Depth_Worker is
   begin
      if Remainder = 0 then
         End_Index := Start_Index + Iterations_Per_Task - 1;
      else
         End_Index := Start_Index + Iterations_Per_Task;
         Remainder := Remainder - 1;
      end if;

      return New_Worker : Depth_Worker
        (Start => Start_Index,
         Finish => End_Index)
      do
         Start_Index := End_Index + 1;
      end return;
   end Create_Worker;

   Long_Lived_Node_Pool : Node_Pool;

   Long_Lived_Tree      : Tree_Node;

   Check : Integer;

begin

   declare
      task Stretch_Depth_Task is
      end Stretch_Depth_Task;

      task body Stretch_Depth_Task is
         Stretch_Depth : constant Positive := Max_Depth + 1;

         Pool : Trees.Node_Pool;
         Stretch_Tree : constant Tree_Node :=
           Trees.Create (Pool  => Pool,
                         Depth => Stretch_Depth);
      begin
         Check        := Item_Check (Stretch_Tree);
         Put ("stretch tree of depth ");
         Put (Item => Stretch_Depth, Width => 1);
         Put (HT & " check: ");
         Put (Item => Check, Width => 1);
         New_Line;
      end Stretch_Depth_Task;

      task Create_Long_Lived_Tree_Task is
      end Create_Long_Lived_Tree_Task;

      task body Create_Long_Lived_Tree_Task is
      begin
         Long_Lived_Tree := Create (Long_Lived_Node_Pool, Max_Depth);
      end Create_Long_Lived_Tree_Task;
   begin
      null;
   end;

   declare
      Workers : array (Worker_Id) of Depth_Worker
        := (others => Create_Worker);
      pragma Unreferenced (Workers);
   begin
      null;
   end;

   for I in Results'Range loop
      Put (Item => Iteration_Tracking (I), Width => 0);
      Put (HT & " trees of depth ");
      Put (Item => Min_Depth + 2 * (I - 1), Width => 0);
      Put (HT & " check: ");
      Put (Item => Results (I), Width => 0);
      New_Line;
   end loop;

   Put ("long lived tree of depth ");
   Put (Item => Max_Depth, Width => 0);
   Put (HT & " check: ");
   Check := Item_Check (Long_Lived_Tree);
   Put (Item => Check, Width => 0);
   New_Line;

end Binarytrees;

--  The Computer Language Benchmarks Game
--  http://shootout.alioth.debian.org/
--
--  Based on Ada versions created by
--    Jim Rogers and Brian Drummond as well as
--    C version by Francesco Abbate
--
--  Contributed by Brad Moore

private with Ada.Finalization;
private with Apache_Runtime.Pools;

package Trees is

   type Tree_Node is private;
   function Item_Check (Item : Tree_Node) return Integer;

   type Node_Pool is limited private;

   function Create
     (Pool : Node_Pool;
      Depth : Integer) return Tree_Node;

private

   use Apache_Runtime;

   type Node;
   type Tree_Node is access all Node;

   type Node is record
      Left  : Tree_Node;
      Right : Tree_Node;
   end record;

   type Node_Pool is
     new Ada.Finalization.Limited_Controlled with
      record
         Pool : aliased Pools.Pool_Type;
      end record;

   overriding procedure Initialize (Item : in out Node_Pool);
   overriding procedure Finalize   (Item : in out Node_Pool);

end Trees;

--  The Computer Language Benchmarks Game
--  http://shootout.alioth.debian.org/
--
--  Based on Ada versions created by
--    Jim Rogers and Brian Drummond as well as the
--    C version by Francesco Abbate
--
--  Contributed by Brad Moore

with Ada.Unchecked_Conversion;
with Interfaces;
with System;

package body Trees is

   Pools_Status : constant Apache_Runtime.Apr_Status :=
     Apache_Runtime.Pools.Initialize;
   pragma Unreferenced (Pools_Status);

   function New_Node (Pool : Node_Pool) return Tree_Node;

   function Create
     (Pool : Node_Pool;
      Depth : Integer) return Tree_Node
   is
      Result : constant Tree_Node := New_Node (Pool);
   begin
      if Depth > 0 then
         Result.all := (Left => Create (Pool, Depth - 1),
                        Right => Create (Pool, Depth - 1));
      else
         Result.all := (Left | Right => null);
      end if;

      return Result;

   end Create;

   overriding procedure Finalize   (Item : in out Node_Pool) is
   begin
      Pools.Destroy (Item.Pool);
   end Finalize;

   overriding procedure Initialize (Item : in out Node_Pool) is
      Status : constant Apr_Status :=
        Pools.Create
          (New_Pool => Item.Pool'Address,
           Parent   => System.Null_Address);
      pragma Unreferenced (Status);
   begin
      null;
   end Initialize;

   function Item_Check (Item : Tree_Node) return Integer is
   begin
      if Item.Left = null then
         return 1;
      else
         return 1 + Item_Check (Item.Left) + Item_Check (Item.Right);
      end if;
   end Item_Check;

   function New_Node (Pool : Node_Pool) return Tree_Node
   is
      function Node_Convert is new Ada.Unchecked_Conversion
        (Source => System.Address,
         Target => Tree_Node);
   begin
      return Node_Convert
        (Pools.Allocate (Pool => Pool.Pool,
                         Size => Node'Size / Interfaces.Unsigned_8'Size));
   end New_Node;
end Trees;

--  The Computer Language Benchmarks Game
--  http://shootout.alioth.debian.org/
--
--  Contributed by Brad Moore

package Apache_Runtime is
   pragma Pure;

   type Apr_Status is new Integer;

   type Apr_Size is new Integer;

end Apache_Runtime;

--  The Computer Language Benchmarks Game
--  http://shootout.alioth.debian.org/
--
--  Contributed by Brad Moore

with System;

package Apache_Runtime.Pools is

   subtype Pool_Type is System.Address;
   subtype Pool_Access is System.Address;

   function Initialize return Apr_Status;

   function Create
     (New_Pool : Pool_Access;
      Parent : Pool_Type) return Apr_Status;

   procedure Destroy (Pool : Pool_Type);

   function Allocate (Pool : Pool_Type; Size : Apr_Size) return System.Address;

private

   pragma Import (C, Initialize, "apr_initialize");
   pragma Import (C, Destroy, "apr_pool_destroy");
   pragma Import (C, Allocate, "apr_palloc");

end Apache_Runtime.Pools;

--  The Computer Language Benchmarks Game
--  http://shootout.alioth.debian.org/
--
--  Contributed by Brad Moore

package body Apache_Runtime.Pools is

   function Create_Ex
     (New_Pool : Pool_Access;
      Parent : Pool_Type;
      Reserved_1, Reserved_2 : System.Address) return Apr_Status;
   pragma Import (C, Create_Ex, "apr_pool_create_ex");

   ------------
   -- Create --
   ------------

   function Create
     (New_Pool : Pool_Access;
      Parent : Pool_Type)
      return Apr_Status
   is
   begin
      return Create_Ex
        (New_Pool,
         Parent,
         System.Null_Address,
         System.Null_Address);
   end Create;

end Apache_Runtime.Pools;