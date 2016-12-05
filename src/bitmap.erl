-module(bitmap).

%% API exports
-export([
         new/1,
         set/2,
         unset/2,
         test/2,
         diff/2,
         size/1
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

size(Bitmap) ->
    1.

%%====================================================================
%% Internal functions
%%====================================================================
