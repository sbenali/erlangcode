%%%-------------------------------------------------------------------
%%% @author samir
%%% @copyright (C) 2016
%%% @doc
%%%
%%% @end
%%% Created : 02. Dec 2016 19:01
%%%-------------------------------------------------------------------
-module(rnd_server).
-author("samir").

-behaviour(gen_server).
-import(rand,[uniform/1]).

%% API
-export([start_link/0,
  generate_first_names/1,
  generate_last_names/1,
  generate_full_names/1,
  generate_random_date/0,
  generate_business_dates/1,
  generate_person/1]).

%% gen_server callbacks
-export([init/1,
  handle_call/3,
  handle_cast/2,
  handle_info/2,
  terminate/2,
  code_change/3]).

-define(SERVER, ?MODULE).

-record(state, {}).

%%%===================================================================
%%% API
%%%===================================================================
start_link() ->
  gen_server:start_link({local, ?SERVER}, ?MODULE, [], []).

generate_first_names(Max) ->
  gen_server:call(?MODULE, {generate_fn,Max}).

generate_last_names(Max) ->
  gen_server:call(?MODULE, {generate_ln,Max}).

generate_full_names(Max)->
  gen_server:call(?MODULE, {generate_full, Max}).

generate_business_dates(Yr)->
  gen_server:call(?MODULE, {generate_dt, Yr}).

generate_person(Max)->
  gen_server:call(?MODULE, {generate_person, Max}).

generate_random_date()->
  gen_server:call(?MODULE, {generate_date}).

%%%===================================================================
%%% gen_server callbacks
%%%===================================================================

init([]) ->
  %% read in first names
  read_and_save_csv("c:/samples/first_names.csv","FN"),
  read_and_save_csv("c:/samples/last_names.csv","LN"),
  read_and_save_csv("c:/samples/Towns_List.csv","TN"),
  {ok, #state{}}.

handle_call({generate_fn, N}, _From, State )->
  {reply, gen_first_names(N), State};
handle_call({generate_ln,N},_From, State)->
  {reply, gen_last_names(N), State};
handle_call({generate_full,N},_From, State)->
  {reply, gen_full(N), State};
handle_call({generate_dt,N},_From, State)->
  {reply, calendar_sb:get_business_calendar(N), State};
handle_call({generate_person,N},_From, State)->
  {reply,gen_person(N),State};
handle_call({generate_date},_From, State)->
  {reply,gen_date(),State};
handle_call(_Request, _From, State) ->
  {reply, ok, State}.



handle_cast(_Request, State) ->
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
gen_person(M)->
  Towns = get("TN"),
  Tl = erlang:length(Towns),
  io:format("No. towns ~p~n",[Tl]),
  People = gen_full(M),
  Persons = [#{fullname => string:join([Y,X], ", "), age => uniform(99), town => lists:nth(uniform(Tl),Towns)}||{X,Y}<-People],
  %%sort by fullname, if use sort/1 then it will sort by age
  SortedPersons = lists:sort(fun(M1, M2)-> maps:get(fullname,M1) < maps:get(fullname,M2)  end, Persons),
  SortedPersons.

gen_full(M)->
  Fn=gen_first_names(M),
  Ln=gen_last_names(M),
  lists:zip(Fn,Ln).


gen_first_names(Max)->
  gen_fn([],Max).

gen_last_names(Max)->
  gen_ln([],Max).

gen_ln(A,Max) when Max>0 ->
  N = get_name("LN"),
  [N | gen_fn(A,Max-1)];
gen_ln(A,0)->
  A.

gen_fn(A,Max) when Max>0 ->
  N = get_name("FN"),
  [N | gen_fn(A,Max-1)];
gen_fn(A,0)->
  A.


get_name(K)->
  Lfn=get(K),
  lists:nth(rand:uniform(length(Lfn)), Lfn).

read_and_save_csv(P,K)->
  case get(K) of
    undefined ->
      case file:open(P,read) of
        {ok,Device} ->
          {ok,V} = file:read_line(Device),
          L = string:tokens(V,"\r"),
          if
            length(L) =:= 1 ->
              io:format("*** Found file with carriage return line feed ***~n"),
              {ok,V2}=file:read_file(P),
              L2 = string:tokens(erlang:binary_to_list(V2),"\r\n"),
              put(K,L2);
            true ->
              io:format("*** Found file with carriage return only ***~n"),
              put(K, L)
          end;
        _ ->
          io:format("File ~p not found.",[P])
      end;
    _ ->
      ignore
  end.


gen_date() ->
  Yrs = lists:seq(1900,2020),
  {{lists:nth(uniform(121),Yrs),uniform(12), uniform(28)},{uniform(24),uniform(60),uniform(60)}}.
