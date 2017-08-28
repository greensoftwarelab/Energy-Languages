% The Computer Language Benchmarks Game
% http://benchmarksgame.alioth.debian.org/
%  
%   contributed by Mark Scandariato
%
%   erl -noshell -noinput -run pidigits main 7 


-module(pidigits).
-export([main/1]).

% conversion
is_safe(Z, N) -> N == extr(Z, 4).
next(Z)       -> extr(Z, 3).
prod(Z, N)    -> comp({10, -10*N, 0, 1}, Z).
cons(Z, Zp)   -> comp(Z, Zp).

% LFT
-define(unit, {1,0,0,1}).
comp({Q,R,S,T}, {Qp, Rp, Sp, Tp}) ->
    {Q*Qp + R*Sp, Q*Rp + R*Tp, S*Qp + T*Sp, S*Rp + T*Tp}.
extr({Q,R,S,T}, X) -> (Q * X + R) div (S * X + T).

lft(K) -> {K, 4*K+2, 0, 2*K+1}.

stream(N) -> stream(N, 0, 1, ?unit, []).
stream(N, N, _, _, P) -> print(N,P);
stream(N, C, K, Z, P) ->
    Y = next(Z),
    case is_safe(Z, Y) of
        true  ->
            stream(N, C+1, K, prod(Z,Y), update(C,Y,P));
        false ->
            stream(N, C, K+1, cons(Z, lft(K)), P)
    end.


update(C, D, P) when C rem 10 == 0, C > 0 ->
    print(C, P),
    [D];

update(_, D, P) -> [D|P].


print(C, P) -> do_print(C, lists:reverse(P)).


do_print(C, []) when C rem 10 == 0 -> io:fwrite("\t:~p~n", [C]);
do_print(C, []) -> io:fwrite("~*.1c:~p~n", [10 - C rem 10, $\t, C]);
do_print(C, [H|T]) -> io:fwrite("~p", [H]), do_print(C, T).


main([Arg]) ->
    N = list_to_integer(Arg),
    main(N),
    erlang:halt(0);

main(N) when N > 1 -> stream(N).