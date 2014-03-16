-module(simplicitydb_utils).

-export([
        max_pool_size/0,
        hash32/1,
        hash_to_shard_num/1,
        key_to_filename/1,
        hash_to_filepath/1,
        worker_name/1,
        init_dirs/0
    ]).

%%%===================================================================
%%% API
%%%===================================================================

max_pool_size() ->
    256. % 2 ** 8

hash32(BinaryKey) when is_binary(BinaryKey) ->
    erlang:crc32(BinaryKey).

hash_to_shard_num(X) ->
    X band 16#000000ff.

key_to_filename(BinaryKey) when is_binary(BinaryKey) ->
    [ $a + T || <<T:4>> <= BinaryKey ].

hash_to_filepath(X) ->
    A = (X band 16#000000ff),
    B = (X band 16#0000ff00) bsr 8,
    C = (X band 16#03ff0000) bsr 16,
    "/" ++ lists:flatten([ integer_to_list(D) ++ "/" || D <- [A, B, C] ]).

worker_name(N) ->
    list_to_atom("simplicitydb_storage_srv_" ++ integer_to_list(N)).

init_dirs() ->
    {ok, Dir} = application:get_env(simplicitydb, dir),
    file:make_dir(Dir),
    [
        begin
            Root = Dir ++ "/" ++ integer_to_list(A),
            file:make_dir(Root),
            [ file:make_dir(Root ++ "/" ++ integer_to_list(B)) || B <- lists:seq(0, 255) ]
        end || A <- lists:seq(0, max_pool_size()-1)
    ].

%%%===================================================================
%%% Internal functions
%%%===================================================================

