%%%-------------------------------------------------------------------
%%% @author Heinz N. Gies <heinz@licenser.net>
%%% @copyright (C) 2016, Heinz N. Gies
%%% @doc Library for dealing with bitmaps in erlang.
%%%
%%% @end
%%% Created :  5 Dec 2016 by Heinz N. Gies <heinz@licenser.net>
%%%-------------------------------------------------------------------

-module(bitmap).

%% API exports
-export([
         new/1,
         set/2,
         unset/2,
         test/2,
         diff/2,
         size/1,
         display_diff/3,
         display/2
        ]).

-type opts() ::
        [{size, pos_integer()}].
-type bitmap() :: <<_:64, _:_*8>>.
-type diff_set() :: [pos_integer()].
-type diff() :: {OnlyA :: diff_set(), OnlyB :: diff_set()}.
%%====================================================================
%% API functions
%%====================================================================

%%--------------------------------------------------------------------
%% @doc Generates a new bitmap with the given options. The total size
%%      will always be a multiople of 8, prefixed with a size.
%% @end
%%--------------------------------------------------------------------
-spec new(opts()) ->
                  {ok, bitmap()}.

new([{size, Size}]) when Size > 0->
    Bits = ceiling(Size / 8) * 8,
    {ok, <<Size:64/unsigned, 0:Bits/unsigned>>}.

%%--------------------------------------------------------------------
%% @doc Sets a position in the bitmap.
%% @end
%%--------------------------------------------------------------------
-spec set(Position :: pos_integer(), bitmap()) ->
                 {ok, bitmap()}.

set(Position, <<Size:64/unsigned, Bitmap/binary>>)
  when Position >= 0,
       Position < Size ->
    <<Head:Position/bitstring, _:1, Tail/bitstring>> = Bitmap,
    Bitmap1 = <<Size:64/unsigned,
                Head/bitstring,
                1:1,
                Tail/bitstring>>,
    {ok, Bitmap1}.

%%--------------------------------------------------------------------
%% @doc Unsets a position in the bitmap.
%% @end
%%--------------------------------------------------------------------
-spec unset(Position :: pos_integer(), bitmap()) ->
                 {ok, bitmap()}.

unset(Position, <<Size:64/unsigned, Bitmap/binary>>)
  when Position >= 0,
       Position < Size ->
    <<Head:Position/bitstring, _:1, Tail/bitstring>> = Bitmap,
    Bitmap1 = <<Size:64/unsigned,
                Head/bitstring,
                0:1,
                Tail/bitstring>>,
    {ok, Bitmap1}.

%%--------------------------------------------------------------------
%% @doc Tests weather a position is set in the bitmap.
%% @end
%%--------------------------------------------------------------------
-spec test(Position :: pos_integer(), bitmap()) ->
                   boolean() |
                   {error, out_of_range}.

test(Position, <<Size:64/unsigned, Bitmap/binary>>)
  when Position >= 0,
       Position < Size ->
    <<_Head:Position/bitstring, R:1, _Tail/bitstring>> = Bitmap,
    R =:= 1.



%%--------------------------------------------------------------------
%% @doc Returns a diff of bitmaps or an error if they have a different
%%      size.
%% @end
%%--------------------------------------------------------------------
-spec diff(bitmap(), bitmap()) ->
                  {ok, diff()} |
                  {error, bad_size}.

diff(Bitmap, Bitmap) ->
    {ok, {[], []}};
diff(<<Size:64/unsigned, BitmapL/binary>>,
     <<Size:64/unsigned, BitmapR/binary>>) ->
    diff_(0, BitmapL, BitmapR, [], []).

diff_(_N, _Bitmap, _Bitmap, L, R) ->
    {ok, {lists:reverse(L), lists:reverse(R)}};
diff_(N,
      <<X:1, BitmapL/bitstring>>,
      <<X:1, BitmapR/bitstring>>, L, R) ->
    diff_(N+1, BitmapL, BitmapR, L, R);
diff_(N,
      <<1:1, BitmapL/bitstring>>,
      <<0:1, BitmapR/bitstring>>, L, R) ->
    diff_(N+1, BitmapL, BitmapR, [N | L], R);
diff_(N,
      <<0:1, BitmapL/bitstring>>,
      <<1:1, BitmapR/bitstring>>, L, R) ->
    diff_(N+1, BitmapL, BitmapR, L, [N | R]).


%%--------------------------------------------------------------------
%% @doc returns the size of a bitmap.,
%% @end
%%--------------------------------------------------------------------
-spec size(bitmap()) ->
                  pos_integer().

size(<<Size:64/unsigned, _/binary>>) ->
    Size.

%%--------------------------------------------------------------------
%% @doc Visualizes the difference between to bitmaps.
%% @end
%%--------------------------------------------------------------------

display_diff(<<Size:64, _/binary>> = LB, <<Size:64, _/binary>> = RB, Width) ->
    {ok, {L, R}} = diff(LB, RB),
    D = diff_view(Size, L, R),
    print_grid(D, Width).

%%--------------------------------------------------------------------
%% @doc Visualizes a bitmap.
%% @end
%%--------------------------------------------------------------------
display(<<Size:64, X/binary>>, Width) ->
    {V, _} = lists:split(Size, to_view(X, [])),
    print_grid(V, Width).

%%====================================================================
%% Internal functions
%%====================================================================
diff_view(Size, L, R) ->
    diff_to_view(0, Size, L, R, []).

diff_to_view(_Size, _Size, _L, _R, Acc) ->
    lists:reverse(Acc);
diff_to_view(P, Size, [P | L], R, Acc) ->
    diff_to_view(P + 1, Size, L, R, [cf:format("~!r<") | Acc]);
diff_to_view(P, Size, L, [P | R], Acc) ->
    diff_to_view(P + 1, Size, L, R, [cf:format("~!r>") | Acc]);
diff_to_view(P, Size, L, R, Acc) ->
    diff_to_view(P + 1, Size, L, R, [cf:format("~!g*") | Acc]).

to_view(<<>>, Acc) ->
    lists:reverse(Acc);
to_view(<<1:1, R/bitstring>>, Acc) ->
    to_view(R, [cf:format("~!g*") | Acc]);
to_view(<<0:1, R/bitstring>>, Acc) ->
    to_view(R, [cf:format("~!yo") | Acc]).

print_grid(List, Width) ->
    Log = trunc(math:log10(length(List))) + 1,
    Space = integer_to_list(Log),
    header(Space, Width),
    S = "~" ++ Space ++ "b ~s~n",
    print_grid(S, List, 0, Width).


print_grid(_S, [], _N, _Count) ->
    ok;
print_grid(S, List, N, Count) when length(List) > Count ->
    {H, T} = lists:split(Count, List),
    io:format(S, [Count * N, H]),
    print_grid(S, T, N+1, Count);
print_grid(S, List, N, Count) ->
    io:format(S, [Count * N, List]).

header(Space, Width) ->
    Log = trunc(math:log10(Width)),
    io:format("Log: ~p~n", [Log]),
    Pfx = io_lib:format("~" ++ Space ++ "c", [$\s]),
    Idx = lists:seq(0, Width-1),
    print_hdrs(Log, Idx, Pfx),
    io:format("~n").

print_hdrs(0, Idx, Pfx) ->
    io:format("~s ~s", [Pfx, [integer_to_list(X rem 10) || X <- Idx]]);
print_hdrs(N, Idx, Pfx) ->
    io:format("~s ~s~n", [Pfx, remove_zero([to_s(N, X) || X <- Idx])]),
    print_hdrs(N - 1, Idx, Pfx).

to_s(N, X) ->
    R =  (X div round(math:pow(10, N))) rem 10,
    integer_to_list(R).
remove_zero(["0" | R]) ->
    [$\s | remove_zero(R)];
remove_zero(R) ->
    R.

ceiling(X) when X < 0 ->
    trunc(X);
ceiling(X) ->
    T = trunc(X),
    case X - T == 0 of
        true -> T;
        false -> T + 1
    end.
