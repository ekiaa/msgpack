%%%---------------------------------------------------------------------------------------------------------------------------------------------
%%% File        : msgpack_recv.erl
%%% Author      : Ekimov Artem <ekimov-artem@ya.ru>
%%% Description : Message-pack receive api and callback functions
%%% Created     : 25.04.2012
%%%---------------------------------------------------------------------------------------------------------------------------------------------

-module(msgpack_recv).

-author('ekimov-artem@ya.ru').

-behaviour(gen_server).

-include("msgpack.hrl").

%% API functions

-export([start/2, start_link/2, stop/1]).

%% gen_server callbacks

-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).

-record(state, {stream, socket, stack = [], rest = <<>>}).

%%==============================================================================================================================================
%% API functions
%%==============================================================================================================================================

start(Stream, Socket) ->
	start_link(Stream, Socket).
	
start_link(Stream, Socket) ->
	gen_server:start_link(msgpack_recv, {Stream, Socket}, []).
	
stop(Pid) ->
	gen_server:call(Pid, stop).

%%==============================================================================================================================================
%% gen_server callbacks
%%==============================================================================================================================================

%%----------------------------------------------------------------------------------------------------------------------------------------------
%% Description : Initiates the msgpack_recv
%%----------------------------------------------------------------------------------------------------------------------------------------------

init({Stream, Socket}) ->
	erlang:monitor(process, Stream),
	gen_server:cast(self(), recv),
 	{ok, #state{stream = Stream, socket = Socket}}.

%%----------------------------------------------------------------------------------------------------------------------------------------------
%% Description : Handling call messages
%%----------------------------------------------------------------------------------------------------------------------------------------------

handle_call(stop, _From, State) ->
	{stop, normal, ok, State};

handle_call(Request, From, State) ->
	{stop, {error, {?MODULE, handle_call, Request, From}}, State}.

%%----------------------------------------------------------------------------------------------------------------------------------------------
%% Description : Handling cast messages
%%----------------------------------------------------------------------------------------------------------------------------------------------

handle_cast(recv, S) ->
	case gen_tcp:recv(S#state.socket, 0, 1000) of
		{ok, Data} ->
			decode(Data, S);
		{error, timeout} ->
			gen_server:cast(self(), recv),
			{noreply, S};
		{error, closed} ->
			{stop, normal, S};
		{error, _Reason} ->
			{stop, normal, S}
	end;
	
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

decode(Data, S) ->
	case msgpack:decode(S#state.stack, <<(S#state.rest)/binary, Data/binary>>) of
		{incomplete, Stack} ->
			gen_server:cast(self(), recv),
			{noreply, S#state{stack = Stack, rest = <<>>}};
		{complete, Msg, Rest} ->
		%	?LOG(?MODULE, self(), "recv: msg ~p", [Msg]),
			gen_server:cast(S#state.stream, {message, Msg}),
			gen_server:cast(self(), recv),
			{noreply, S#state{stack = [], rest = Rest}};
		{ok, Msg} ->
		%	?LOG(?MODULE, self(), "recv: msg ~p", [Msg]),
			gen_server:cast(S#state.stream, {message, Msg}),
			gen_server:cast(self(), recv),
			{noreply, S#state{stack = [], rest = <<>>}}
	end.

%%%---------------------------------------------------------------------------------------------------------------------------------------------
%%% End of file : msgpack_recv.erl
%%%---------------------------------------------------------------------------------------------------------------------------------------------