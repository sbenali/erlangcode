-module(sqlser).
-author("samir").
-behaviour(gen_server).

%% API
-export([start_link/0, connect/1, exec/1, disconnect/0]).

%%"DSN=firstdb"
%% gen_server callbacks
-export([init/1,
  handle_call/3,
  handle_cast/2,
  handle_info/2,
  terminate/2,
  code_change/3]).

-define(SERVER, ?MODULE).
-record(state, {ref=undefined}).

%%%===================================================================
%%% API
%%%===================================================================
start_link() ->
  %%start_link calls init and its there we connect, thus name of this function is connect
  gen_server:start_link({global, ?SERVER}, ?MODULE, [], []).

connect(DSN)->
  gen_server:call({global,?MODULE}, {connect_to_server, DSN}).

disconnect()->
  gen_server:call({global,?MODULE}, {disconnect_from_server}).

exec(S)->
  gen_server:call({global,?MODULE}, {exec_statement, S}).

%%%===================================================================
%%% gen_server callbacks
%%%===================================================================
init([]) ->
  odbc:start(permanent),
  {ok, #state{}}.

handle_call({connect_to_server,DSN},_From,_State=#state{})->
  {ok, Ref} = odbc:connect(DSN,[{binary_strings, off},{tuple_row, on}]),
  {reply, ok, #state{ref=Ref}};
handle_call({disconnect_from_server},_From,_State=#state{})->
  {reply, odbc:disconnect(_State#state.ref), _State};
handle_call({exec_statement, S},_From,_State=#state{})->
  case odbc:sql_query(_State#state.ref, S) of
    {error, connection_closed} ->
      {reply, not_connected,_State};
    Res ->
      {reply,Res,_State}
  end;
handle_call(_Request, _From, State) ->
  {reply, ok_unknown, State}.

handle_cast(_Request, State) ->
  {noreply, State}.

handle_info(_Info, State) ->
  {noreply, State}.

terminate(_Reason, _State) ->
  odbc:stop(),
  ok.

code_change(_OldVsn, State, _Extra) ->
  {ok, State}.
