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
         visualize/4
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
    {ok, <<Size:64/unsigned>>}.

%%--------------------------------------------------------------------
%% @doc Sets a position in the bitmap.
%% @end
%%--------------------------------------------------------------------
-spec set(Position :: pos_integer(), bitmap()) ->
                 {ok, bitmap()}.

set(Position, Bitmap) when Position > 0->
    {ok, Bitmap}.

%%--------------------------------------------------------------------
%% @doc Unsets a position in the bitmap.
%% @end
%%--------------------------------------------------------------------
-spec unset(Position :: pos_integer(), bitmap()) ->
                 {ok, bitmap()}.

unset(Position, Bitmap) when Position > 0->
    {ok, Bitmap}.

%%--------------------------------------------------------------------
%% @doc Tests weather a position is set in the bitmap.
%% @end
%%--------------------------------------------------------------------
-spec test(Position :: pos_integer(), bitmap()) ->
                   boolean() |
                   {error, out_of_range}.

test(Position, _Bitmap) when Position > 0->
    true.



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
diff(_BitmapA, _BitmapB) ->
    {error, bad_size}.

%%--------------------------------------------------------------------
%% @doc returns the size of a bitmap.,
%% @end
%%--------------------------------------------------------------------
-spec size(bitmap()) ->
                  pos_integer().

size(<<Size:64/unsigned, _/binary>>) ->
    Size.

%%--------------------------------------------------------------------
%% @doc returns the size of a bitmap.,
%% @end
%%--------------------------------------------------------------------


visualize(Size, L, R, Count) ->
    Log = trunc(math:log10(Size)) + 1,
    Space = integer_to_list(Log),
    S = "~" ++ Space ++ "b ",
    %%header(Count, Space),
    io:format(S, [0]),
    visualize(0, Size, L, R, "~n"++S, 1, Count, Count).

%%====================================================================
%% Internal functions
%%====================================================================
visualize(Size, Size, _, _, _S, _N, _Line, _Count) ->
    ok;
visualize(P, Size, L, R, S, N, 0, Count) ->
    io:format(S, [N * Count]),
    visualize(P, Size, L, R, S, N + 1, Count, Count);
visualize(P, Size, [P | L], R, S, N, Pos, Count) ->
    cf:print("~!r<"),
    visualize(P + 1, Size, L, R, S, N, Pos - 1, Count);
visualize(P, Size, L, [P | R], S, N, Pos, Count) ->
    cf:print("~!r>"),
    visualize(P + 1, Size, L, R, S, N, Pos - 1, Count);
visualize(P, Size, L, R, S, N, Pos, Count) ->
    cf:print("~!g*"),
    visualize(P + 1, Size, L, R, S, N, Pos - 1, Count).

%% header(Count, Space) ->
%%     Log = trunc(math:log10(Count))+1,
%%     S = "~n ~" ++ Space ++ "c",
%%     header_(S, Count, Log, 0),
%%     io:format("~n").

%% header_(S, Count, 0, _) ->
%%     ok;
%% header_(S, Count, Log, N)  ->
%%     header_(S, Count div 10, Log - 1, N + 1),
%%     io:format(S, [$\s]),
%%     header_line(N, Count, Log).

%% header_line(N, Count, 0) ->
%%     ok;
%% header_line(N, Count, R) ->
%%     header_line(N, Count, R - 1),
%%     [io:format("~" ++ integer_to_list(round(math:pow(10, N))) ++ "c", [c(N, C)])
%%      || C <- lists:seq(0, min(Count-1, 9))].

%% c(0, I) ->
%%     [C] = integer_to_list(I),
%%     C;
%% c(_, 0) ->
%%     $\s;
%% c(_, I) ->
%%     [C] = integer_to_list(I),
%%     C.
