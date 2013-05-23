-module(msgpack_test).
-export([test/0, test_msg/0]).
-include("ptcl.hrl").

-define(MP_PFN,		2#0).      % Positive FixNum   0xxxxxxx   0x00 - 0x7f
-define(MP_FR,		2#101).    % FixRaw            101xxxxx   0xa0 - 0xbf
-define(MP_NFN,		2#111).    % Negative FixNum   111xxxxx   0xe0 - 0xff
-define(MP_FM,		2#1000).   % FixMap            1000xxxx   0x80 - 0x8f
-define(MP_FA,		2#1001).   % FixArray          1001xxxx   0x90 - 0x9f
-define(MP_NIL,		16#C0).    % nil               11000000   0xc0
-define(MP_FALSE,	16#C2).    % false             11000010   0xc2
-define(MP_TRUE,	16#C3).    % true              11000011   0xc3
-define(MP_FLOAT,	16#CA).    % float             11001010   0xca
-define(MP_DOUBLE,	16#CB).    % double            11001011   0xcb
-define(MP_UINT8,	16#CC).    % uint8             11001100   0xcc
-define(MP_UINT16,	16#CD).    % uint16            11001101   0xcd
-define(MP_UINT32,	16#CE).    % uint32            11001110   0xce
-define(MP_UINT64,	16#CF).    % uint64            11001111   0xcf
-define(MP_INT8,	16#D0).    % int8              11010000   0xd0
-define(MP_INT16,	16#D1).    % int16             11010001   0xd1
-define(MP_INT32,	16#D2).    % int32             11010010   0xd2
-define(MP_INT64,	16#D3).    % int64             11010011   0xd3
-define(MP_RAW16,	16#DA).    % raw16             11011010   0xda
-define(MP_RAW32,	16#DB).    % raw32             11011011   0xdb
-define(MP_ARRAY16,	16#DC).    % array16           11011100   0xdc
-define(MP_ARRAY32,	16#DD).    % array32           11011101   0xdd
-define(MP_MAP16,	16#DE).    % map16             11011110   0xde
-define(MP_MAP32,	16#DF).    % map32             11011111   0xdf

-define(TEST_MSG,
		[
			{?STRING, "This is test message for module msgpack"},
			{?STRING, "Test map 16"},
			{map, [
				{{?STRING, "Test positive fixnum"},	1},
				{{?STRING, "Test negative fixnum"},	-1},
				{{?STRING, "Test uint8"},			16#FF},
				{{?STRING, "Test uint16"},			16#100},
				{{?STRING, "Test uint32"},			16#10000},
				{{?STRING, "Test uint64"},			16#100000000},
				{{?STRING, "Test int8"},			-16#80},
				{{?STRING, "Test int16"},			-16#8000},
				{{?STRING, "Test int32"},			-16#80000000},
				{{?STRING, "Test int64"},			-16#8000000000000000},
				{{?STRING, "Test nil"},				null},
				{{?STRING, "Test boolean"},			[true, false]},
				{{?STRING, "Test float"},			math:pi()},
				{{?STRING, "Test fix raw"},			{?BINARY, <<1,2,3,4,5,6,7,8,9,0>>}},
				{{?STRING, "Test fix array"},		[1,2,3,4,5,6,7,8,9,0]},
				{{?STRING, "Test fix map"},			{map, [{1,2},{3,4},{5,6},{7,8},{9,0}]}}
			]},
			{?STRING, "Test array 16"},
			[1,2,3,4,5,6,7,8,9,0,9,8,7,6,5,4,3,2,1],
			{?STRING, "Test raw 16"},
			{?BINARY, <<1,2,3,4,5,6,7,8,9,0,9,8,7,6,5,4,3,2,1>>}
		]
	).
-define(TEST_COUNT, 1000).
-define(TEST_LIST, [?MP_PFN, ?MP_NFN, ?MP_UINT8, ?MP_UINT16, ?MP_UINT32, ?MP_UINT64, ?MP_INT8, ?MP_INT16, ?MP_INT32, ?MP_INT64]).

	
test_msg() ->
	io:format("Test message:~n~p~n", [?TEST_MSG]),
	test_msg(?TEST_MSG).
test_msg(Msg) ->
	{Te, Bin} = timer:tc(msgpack, encode, [Msg]),
	Size = byte_size(Bin),
	{Td, _Res} = timer:tc(msgpack, decode, [Bin]),
	{round(Te/1000), round(Td/1000), Size}.
	
test() ->
	test(?TEST_COUNT).
test(Count) ->
	F = fun(Type) ->
		{Name, Msg} = get_test_msg(Type, Count),
		Res = test_msg(Msg),
		io:format("Test: ~w (~w), count: ~w, result: ~w~n", [Name, Type, Count, Res])
	end,
	lists:foreach(F, ?TEST_LIST).
	
get_test_msg(?MP_PFN, Count) ->
	{pfn, get_msg(1, Count)};
get_test_msg(?MP_NFN, Count) ->
	{nfn, get_msg(-1, Count)};
get_test_msg(?MP_UINT8, Count) ->
	{uint8, get_msg(16#FF, Count)};
get_test_msg(?MP_UINT16, Count) ->
	{uint16, get_msg(16#100, Count)};
get_test_msg(?MP_UINT32, Count) ->
	{uint32, get_msg(16#10000, Count)};
get_test_msg(?MP_UINT64, Count) ->
	{uint64, get_msg(16#100000000, Count)};
get_test_msg(?MP_INT8, Count) ->
	{int8, get_msg(-16#80, Count)};
get_test_msg(?MP_INT16, Count) ->
	{int16, get_msg(-16#8000, Count)};
get_test_msg(?MP_INT32, Count) ->
	{int32, get_msg(-16#80000000, Count)};
get_test_msg(?MP_INT64, Count) ->
	{int64, get_msg(-16#8000000000000000, Count)}.
	
get_msg(Value, Count) ->
	get_msg(Value, Count, []).
get_msg(_Value, 0, List) ->
	lists:reverse(List);
get_msg(Value, Count, List) ->
	get_msg(Value, Count-1, [Value | List]).