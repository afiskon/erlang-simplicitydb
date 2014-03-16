-module(simplicitydb).

-export([
        read/1,
        write/2,
        delete/1,
        freeze/0,
        unfreeze/0,
        is_frozen/0
    ]).

%%%===================================================================
%%% API
%%%===================================================================

read(Key) ->
    case call(Key, read) of
        {ok, Value} when is_binary(Value) ->
            <<Hash:32, Data/binary>> = Value,
            case simplicitydb_utils:hash32(Data) =:= Hash of
                true -> {ok, binary_to_term(Data)};
                false -> undefined % corrupted data
            end;
        {error,enoent} -> undefined;
        Other -> Other
    end.

write(Key, Value) ->
    case is_frozen() of
        true -> frozen;
        false -> 
            Data = term_to_binary(Value),
            Hash = simplicitydb_utils:hash32(Data),
            call(Key, {write, <<Hash:32, Data/binary>>})
    end.

delete(Key) ->
    case is_frozen() of
        true -> frozen;
        false -> call(Key, delete)
    end.

freeze() ->
    application:set_env(simplicitydb, frozen, true).

unfreeze() ->
    application:set_env(simplicitydb, frozen, false).

is_frozen() ->
    {ok, Frozen} = application:get_env(simplicitydb, frozen),
    Frozen.

%%%===================================================================
%%% Internal functions
%%%===================================================================

call(Key, Action) ->
    BinaryKey = term_to_binary(Key),
    Hash = simplicitydb_utils:hash32(BinaryKey),
    Path = simplicitydb_utils:hash_to_filepath(Hash),
    File = simplicitydb_utils:key_to_filename(BinaryKey),
    {ok, Dir} = application:get_env(simplicitydb, dir),
    Filename = Dir ++ Path ++ File,
    Backup = Filename ++ ".bak",
    gen_server:call(get_worker(Hash), {Action, Filename, Backup}).

get_worker(Hash) ->
    {ok, PoolSize} = application:get_env(simplicitydb, pool_size),
    ShardNum = simplicitydb_utils:hash_to_shard_num(Hash),
    Num = ShardNum rem PoolSize,
    simplicitydb_utils:worker_name(Num).
