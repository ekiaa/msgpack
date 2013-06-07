%%%---------------------------------------------------------------------------------------------------------------------------------------------
%%% File        : msgpack.erl
%%% Author      : Artem A. Ekimov <ekimov-artem@ya.ru>
%%% Description : Msgpack application API module
%%% Created     : 25.04.2012
%%%---------------------------------------------------------------------------------------------------------------------------------------------

-module(msgpack).

-bahaviour(application).

-include("msgpack.hrl").

%% application callbacks

-export([start/2, stop/1]).

%% External API functions

-export([
	start/0, 
	stop/0,
	start_stream/2,
	request/3,
	message/2,
	error/3,
	response/3,
	notify/3
]).

%% Internal API functions

-export([
	request_msg/4,
	error_msg/3,
	response_msg/3,
	notify_msg/3,
	start_recv/2,
	start_send/2,
	send/2,
	decode/1,
	decode/2,
	encode/1,
	log/4
]).

%%==============================================================================================================================================
%% application callbacks
%%==============================================================================================================================================

start(_Type, _Args) ->
	msgpack_sup:start().

stop(_State) ->
	ok.

%%=============================================================================
%% External API functions
%%=============================================================================

start() ->
	application:load(?APPLICATION),
	application:start(?APPLICATION).
	
stop() ->
	application:stop(?APPLICATION),
	application:unload(?APPLICATION).
	
start_stream(Stream, Socket) ->
	supervisor:start_child(?STREAMSUP, [Stream, Socket]).
	
request(Destination, Function, Args) ->
	gen_server:call(Destination, {request, Function, Args}).
	
response(Stream, TrID, Response) ->
	gen_server:cast(Stream, {response, TrID, Response}).
	
error(Stream, TrID, Error) ->
	gen_server:cast(Stream, {error, TrID, Error}).

notify(Stream, Function, Args) ->
%	?LOG(?MODULE, self(), "notify:~n~p", [{Stream, Function, Args}]),
	gen_server:cast(Stream, {notify, Function, Args}).

%%=============================================================================
%% Internal API functions
%%=============================================================================
	
message(Stream, Message) ->
	gen_server:cast(Stream, {message, Message}).

request_msg(Stream, TrID, Function, Args) ->
	gen_server:cast(Stream, {msgpack, {request, TrID, Function, Args}}).
	
error_msg(Stream, TrID, Error) ->
	gen_server:cast(Stream, {msgpack, {error, TrID, Error}}).
	
response_msg(Stream, TrID, Response) ->
	gen_server:cast(Stream, {msgpack, {response, TrID, Response}}).
	
notify_msg(Stream, Function, Args) ->
%	?LOG(?MODULE, self(), "notify_msg:~n~p", [{Stream, Function, Args}]),
	gen_server:cast(Stream, {msgpack, {notify, Function, Args}}).
	
start_recv(Stream, Socket) ->
	supervisor:start_child(?RECVSUP, [Stream, Socket]).
	
start_send(Stream, Socket) ->
	supervisor:start_child(?SENDSUP, [Stream, Socket]).
	
send(Pid, Msg) ->
	gen_server:cast(Pid, {send, Msg}).
	
decode(Msg) ->
	msgpack_code:decode(Msg).
	
decode(Stack, Msg) ->
	msgpack_code:decode(Stack, Msg).
	
encode(Msg) ->
	msgpack_code:encode(Msg).
	
log(Module, Pid, Format, Data) ->
	io:format("~-" ++ ?TITLE_LENGTH ++ "w (~w): " ++ Format ++ "~n", [Module, Pid] ++ Data).