-module(perf_test).

-compile(export_all).

-include_lib("eunit/include/eunit.hrl").


simple_test() ->
    Opts = [{size, 10}],
    {ok, B} = bitmap:new(Opts),
    {ok, B1} = bitmap:set(9, B),
    {ok, B2} = bitmap:from_list([9], Opts),
    ?assertEqual(B1, B2).

insert_5000_2_test_() ->
    run_bench(5000, 50, 2).

insert_500_20_test_() ->
    run_bench(500, 50, 20).

insert_50_200_test_() ->
    run_bench(50, 500, 20).

insert_50000_2_test_() ->
    run_bench(50000, 50, 2).

insert_5000_20_test_() ->
    run_bench(5000, 50, 20).

insert_500_200_test_() ->
    run_bench(500, 50, 200).

run_bench(X, N, Spacing) ->
    {timeout, 10000,
     fun () ->
             {ok, B} = bitmap:new([{size, X * Spacing}]),
             Pos = [P * 2 || P <- lists:seq(0, X - 1)],
             Ts = [begin
                       {T, _} = timer:tc(perf_test, do_set, [B, Pos]),
                       T
                   end || _ <- lists:seq(1, N)],
             Ts2 = [begin
                        {T, _} = timer:tc(bitmap, set_many, [Pos, B]),
                        T
                   end || _ <- lists:seq(1, N)],
             io:format(user, "T(~p @ ~p):~n", [X, Spacing]),
             io:format(user, "  set:      ~pns~n", [lists:sum(Ts)/(N * X)]),
             io:format(user, "  set many: ~pns~n", [lists:sum(Ts2)/(N * X)]),
             ?_assertEqual(perf_test:do_set(B, Pos), bitmap:set_many(Pos, B))
     end}.


do_set(B, []) ->
    B;
do_set(B, [P | R]) ->
    {ok, B1} = bitmap:set(P, B),
    do_set(B1, R).
