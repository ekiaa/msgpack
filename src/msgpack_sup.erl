%%%-------------------------------------------------------------------
%%% File        : msgpack_sup.erl
%%% Author      : Artem A. Ekimov <ekimov-artem@ya.ru>
%%% Description : Supervisor API and callbacks module
%%% Created     : 25.04.2012
%%%-------------------------------------------------------------------

-module(msgpack_sup).

-bahaviour(supervisor).

-include("msgpack.hrl").

%% API functions

-export([start/0, start_link/0]).

%% supervisor callbacks

-export([init/1]).

%%====================================================================
%% API functions
%%====================================================================

start() ->
	start_link().

start_link() ->
	supervisor:start_link({local, ?MODULE}, ?MODULE, []).

%%====================================================================
%% supervisor callbacks
%%====================================================================

init([]) ->
	{ok, {{one_for_all, 1, 10}, [
		{msgpack_stream_sup, {msgpack_stream_sup, start, []}, permanent, infinity, supervisor, [msgpack_stream_sup]},
		{msgpack_recv_sup, {msgpack_recv_sup, start, []}, permanent, infinity, supervisor, [msgpack_recv_sup]},
		{msgpack_send_sup, {msgpack_send_sup, start, []}, permanent, infinity, supervisor, [msgpack_send_sup]}
	]}}.

	