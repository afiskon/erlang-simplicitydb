-module(simplicitydb_storage_srv).

-behaviour(gen_server).

%% API
-export([
        start_link/1
    ]).

%% gen_server callbacks
-export([
        init/1,
        handle_call/3,
        handle_cast/2,
        handle_info/2,
        terminate/2,
        code_change/3
    ]).

-record(state, {}).

%%%===================================================================
%%% API
%%%===================================================================

start_link(Name) ->
    gen_server:start_link({local, Name}, ?MODULE, [], []).

%%%===================================================================
%%% gen_server callbacks
%%%===================================================================

init([]) ->
    {ok, #state{}}.

handle_call({read, Filename, Backup}, _From, State) ->
    file:rename(Backup, Filename), % will do nothing if Backup doesn't exist
    {reply, file:read_file(Filename), State};

handle_call({{write, Value}, Filename, Backup}, _From, State) ->
    file:make_dir(filename:dirname(Filename)),
    {ok, Sync} = application:get_env(simplicitydb, sync),

    % if something goes wrong, just restart a process
    % such approach prevents resource leak
    file:rename(Filename, Backup),
    {ok, Fid} = file:open(Filename, [write, raw]),
    ok = file:write(Fid, Value),
    case Sync of
        true ->     ok = file:datasync(Fid);
        false ->    ok
    end,
    ok = file:close(Fid),
    file:delete(Backup),
    {reply, ok, State};

handle_call({delete, Filename, Backup}, _From, State) ->
    file:delete(Filename),
    file:delete(Backup),
    {reply, ok, State};

handle_call(_Request, _From, State) ->
    {reply, ok, State}.

handle_cast(_Msg, State) ->
    {noreply, State}.

handle_info(_Info, State) ->
    {noreply, State}.

terminate(_Reason, _State) ->
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%%%===================================================================
%%% Internal functions
%%%===================================================================
