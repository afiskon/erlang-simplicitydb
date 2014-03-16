-module(simplicitydb_sup).

-behaviour(supervisor).

%% API
-export([start_link/0]).

%% Supervisor callbacks
-export([init/1]).

%% Helper macro for declaring children of supervisor
-define(CHILD(Mod, Name, Type), {Name, {Mod, start_link, [Name]}, permanent, 5000, Type, [Mod]}).

%% ===================================================================
%% API functions
%% ===================================================================

start_link() ->
    supervisor:start_link({local, ?MODULE}, ?MODULE, []).

%% ===================================================================
%% Supervisor callbacks
%% ===================================================================

init([]) ->
    {ok, PoolSize} = application:get_env(simplicitydb, pool_size),
    MaxPoolSize = simplicitydb_utils:max_pool_size(),
    PoolSize > MaxPoolSize andalso throw({invalid_pool_size, PoolSize}),
    Children = [ ?CHILD(simplicitydb_storage_srv, simplicitydb_utils:worker_name(I), worker) || I <- lists:seq(0, PoolSize-1) ],
    {ok, { {one_for_one, 5, 10}, Children} }.


