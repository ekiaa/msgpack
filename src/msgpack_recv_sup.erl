%%%-------------------------------------------------------------------
%%% File        : msgpack_recv_sup.erl
%%% Author      : Artem A. Ekimov <ekimov-artem@ya.ru>
%%% Description : Supervisor API and callbacks module
%%% Created     : 25.04.2012
%%%-------------------------------------------------------------------

-module(msgpack_recv_sup).

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
	supervisor:start_link({local, ?RECVSUP}, ?MODULE, []).

%%====================================================================
%% supervisor callbacks
%%====================================================================

init([]) ->
	{ok, {{simple_one_for_one, 1, 10}, [
		{undefined, {msgpack_recv, start, []}, temporary, 1000, worker, [msgpack_recv]}
	]}}.

	