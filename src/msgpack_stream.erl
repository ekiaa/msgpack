%%%---------------------------------------------------------------------------------------------------------------------------------------------
%%% File        : msgpack_stream.erl
%%% Author      : Ekimov Artem <ekimov-artem@ya.ru>
%%% Description : Message-pack stream api and callback functions
%%% Created     : 28.04.2012
%%%---------------------------------------------------------------------------------------------------------------------------------------------

-module(msgpack_stream).

-author('ekimov-artem@ya.ru').

-behaviour(gen_server).

-include("msgpack.hrl").

%% API functions

-export([start/2, start_link/2, stop/1]).

%% gen_server callbacks

-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).

-record(state, {stream, socket, send, recv, trid=0, list=[]}).

%%==============================================================================================================================================
%% API functions
%%==============================================================================================================================================

start(Stream, Socket) ->
	start_link(Stream, Socket).
	
start_link(Stream, Socket) ->
	gen_server:start_link(?MODULE, {Stream, Socket}, []).
	
stop(Pid) ->
	gen_server:call(Pid, stop).

%%==============================================================================================================================================
%% gen_server callbacks
%%==============================================================================================================================================

%%----------------------------------------------------------------------------------------------------------------------------------------------
%% Description : Initiates the decode manager
%%----------------------------------------------------------------------------------------------------------------------------------------------

init({Stream, Socket}) ->
	{ok, Recv} = msgpack:start_recv(self(), Socket),
	{ok, Send} = msgpack:start_send(self(), Socket),
	erlang:monitor(process, Recv),
	erlang:monitor(process, Send),
	erlang:monitor(process, Stream),
	{ok, #state{stream = Stream, socket = Socket, recv = Recv, send = Send}};
	
init(Args) ->
	{stop, {error, {init, Args}}}.

%%----------------------------------------------------------------------------------------------------------------------------------------------
%% Description : Handling call messages
%%----------------------------------------------------------------------------------------------------------------------------------------------

handle_call({request, Function, Args}, _From, S) ->
	msgpack:send(S#state.send, [0, S#state.trid, Function, Args]),
	{reply, {ok, S#state.trid}, S#state{trid = S#state.trid + 1}};
%	{reply, S#state.trid, S#state{trid = S#state.trid + 1, list = [{S#state.trid, Function} | S#state.list]}};
	
handle_call(stop, _From, State) ->
	{stop, normal, ok, State};

handle_call(Request, From, State) ->
	{stop, {error, {?MODULE, handle_call, Request, From}}, State}.

%%----------------------------------------------------------------------------------------------------------------------------------------------
%% Description : Handling cast messages
%%----------------------------------------------------------------------------------------------------------------------------------------------

handle_cast({response, TrID, Response}, S) ->
	msgpack:send(S#state.send, [1, TrID, null, Response]),
	{noreply, S};

handle_cast({error, TrID, Error}, S) ->
	msgpack:send(S#state.send, [1, TrID, Error, null]),
	{noreply, S};

handle_cast({notify, Function, Args}, S) ->
	?LOG(?MODULE, self(), "notify", []),
	msgpack:send(S#state.send, [2, Function, Args]),
	{noreply, S};

handle_cast({send, Msg}, S) ->
	msgpack:send(S#state.send, Msg),
	{noreply, S};
	
handle_cast({message, [2, Function, Args | []]}, S) ->
%	io:format("msgpack_stream (~w): receive notify~n~p(~p)~n", [self(), Function, Args]),
	msgpack:notify_msg(S#state.stream, Function, Args),
	{noreply, S};
	
handle_cast({message, [0, TrID, Function, Args | []]}, S) ->
	msgpack:request_msg(S#state.stream, TrID, Function, Args),
	{noreply, S};

handle_cast({message, [1, TrID, Error, null | []]}, S) ->
%	case lists:keyfind(TrID, 1, S#state.list) of
%		{_, Function} ->
	msgpack:error_msg(S#state.stream, TrID, Error),
%			{noreply, S#state{list = lists:keydelete(TrID, 1, S#state.list)}};
%		false ->
	{noreply, S};
%	end;

handle_cast({message, [1, TrID, null, Response | []]}, S) ->
%	case lists:keyfind(TrID, 1, S#state.list) of
%		{_, Function} ->
	msgpack:response_msg(S#state.stream, TrID, Response),
%			{noreply, S#state{list = lists:keydelete(TrID, 1, S#state.list)}};
%		false ->
	{noreply, S};
%	end;
	
handle_cast(Msg, State) ->
	{stop, {error, {?MODULE, handle_cast, Msg}}, State}.

%%----------------------------------------------------------------------------------------------------------------------------------------------
%% Description : Handling all non call/cast messages
%%----------------------------------------------------------------------------------------------------------------------------------------------

handle_info({'DOWN', _MonRef, process, _Pid, _Info}, S) ->
	{stop, normal, S};
	
handle_info(Info, State) ->
	{stop, {error, {?MODULE, handle_info, Info}}, State}.

%%----------------------------------------------------------------------------------------------------------------------------------------------
%% Description : This function is called by a gen_server when it is about to terminate
%%----------------------------------------------------------------------------------------------------------------------------------------------

terminate(_Reason, _State) ->
	ok.

%%----------------------------------------------------------------------------------------------------------------------------------------------
%% Description : Convert process state when code is changed
%%----------------------------------------------------------------------------------------------------------------------------------------------

code_change(_OldVsn, State, _Extra) ->
	{ok, State}.

%%==============================================================================================================================================
%% Internal functions
%%==============================================================================================================================================

%%%---------------------------------------------------------------------------------------------------------------------------------------------
%%% End of file : msgpack_stream.erl
%%%---------------------------------------------------------------------------------------------------------------------------------------------