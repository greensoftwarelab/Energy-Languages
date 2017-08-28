(*
  The Computer Language Benchmarks Game
  http://benchmarksgame.alioth.debian.org/

  contributed by Vitaly Trifonof based on a contribution of Ales Katona
  *reset*
*)

program BinaryTrees;

type
  PNode = ^TNode;
  TNode = record
    l, r: PNode;
  end;

function CreateNode(l2, r2: PNode): PNode; inline;
begin
  CreateNode := GetMem(SizeOf(TNode));
  CreateNode^.l:=l2;
  CreateNode^.r:=r2;
end;


(* Destroy node and it subnodes in one procedure *)

procedure DestroyNode(ANode: PNode); inline;
var
  LNode, RNode: PNode;
begin
  LNode := ANode^.l;
  if LNode <> nil then
  begin
    RNode := ANode^.r;
    if LNode^.l <> nil then
    begin
      DestroyNode(LNode^.l);
      DestroyNode(LNode^.r);
      FreeMem(LNode, SizeOf(TNode));

      DestroyNode(RNode^.l);
      DestroyNode(RNode^.r);
      FreeMem(RNode, SizeOf(TNode));
    end
    else
    begin
      DestroyNode(LNode);
      DestroyNode(RNode);
    end
  end;

  FreeMem(ANode, SizeOf(TNode));
end;


(* Left subnodes check in cycle, right recursive *)

function CheckNode(ANode: PNode): Longint; inline;
begin
  CheckNode := 0;
  while ANode^.l <> nil do
  begin
    CheckNode += 1 + CheckNode(ANode^.r);
    ANode := ANode^.l
  end;
  CheckNode += 1;
end;


(*
   Create node and it subnodes in one function

   make(1,a)=(2I-1)=Ia make(2,Ia-1)=(2(2I-1)-1)=(4I-3)
                       make(2,Ia)  =(2(2I-1))  =(4I-2)

   make(1,b)=(2I)=Ib   make(2,Ib-1)=(2(2I)-1)  =(4I-1)
                       make(2,Ib)  =(2(2I))    =(4I)
*)

function Make(d: Longint): PNode;
var
  fi: Longint;
begin
  case d of
   0: Make:=CreateNode(nil, nil);
   1: Make:=CreateNode(CreateNode(nil, nil), CreateNode(nil, nil));
  else
      d -= 2; 
      Make:=CreateNode(
                           CreateNode( Make(d),Make(d) ),
                           CreateNode( Make(d),Make(d) )
                        )
  end
end;

const
  mind = 4;

var
  maxd : Longint = 10;
  strd,
  iter,
  c, d, i : Longint;
  tree, llt : PNode;

begin
  if ParamCount = 1 then
    Val(ParamStr(1), maxd);

  if maxd < mind+2 then
     maxd := mind + 2;

  strd:=maxd + 1;
  tree:=Make(strd);
  Writeln('stretch tree of depth ', strd, #9' check: ', CheckNode(tree));
  DestroyNode(tree);

  llt:=Make(maxd);

  d:=mind;
  while d <= maxd do begin
    iter:=1 shl (maxd - d + mind);
    c:=0;
    for i:=1 to Iter do begin
      tree:=Make(d);
      c:=c + CheckNode(tree);
      DestroyNode(tree);
    end;
    Writeln(Iter, #9' trees of depth ', d, #9' check: ', c);
    Inc(d, 2);
  end;

  Writeln('long lived tree of depth ', maxd, #9' check: ', CheckNode(llt));
  DestroyNode(llt);
end.