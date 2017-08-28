% The Computer Language Benchmarks Game
% http://benchmarksgame.alioth.debian.org/
%
% contributed by Isaac Gouy (Erlang novice)
% parallelized by Kevin Scaldeferri

-module(binarytrees).
-export([main/1]).
-export([depth/2]).

-define(Min,4).

main([Arg]) ->
   N = list_to_integer(Arg),
   Max = lists:max([?Min+2,N]),

   Stretch = Max + 1,
   io:fwrite("stretch tree of depth ~w\t check: ~w~n",
      [ Stretch, itemCheck(bottomUp(Stretch)) ]),

   LongLivedTree = bottomUp(Max),
   depthLoop(?Min,Max),

   io:fwrite("long lived tree of depth ~w\t check: ~w~n",
      [ Max, itemCheck(LongLivedTree) ]),

   halt(0).


depthLoop(D,M) ->
    Results = rpc:pmap({?MODULE, depth}, [M], lists:seq(D, M, 2)),
    lists:foreach(fun(Result) ->
                          io:fwrite("~w\t trees of depth ~w\t check: ~w~n", Result)
                  end,
                  Results).

depth(D,M) ->
    N = 1 bsl (M-D + ?Min),
    [ N, D, sumLoop(N,D,0) ].

sumLoop(0,_,Sum) -> Sum;
sumLoop(N,D,Sum) ->
   sumLoop(N-1,D, Sum + itemCheck(bottomUp(D))).

bottomUp(0) -> {nil, nil};
bottomUp(D) -> {bottomUp(D-1), bottomUp(D-1)}.

itemCheck(nil) -> 0;
itemCheck({Left,Right}) ->
   1 + itemCheck(Left) + itemCheck(Right).