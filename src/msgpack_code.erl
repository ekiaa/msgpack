-module(msgpack_code).
-author('ekimov-artem@ya.ru').
-export([decode/1, decode/2, encode/1]).
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



decode(<<>>) ->
	nil;
decode(Data) ->
	decode(value, [], empty, empty, Data).
	
decode([], Data) ->
	decode(Data);
decode({incomplete, Stack}, Data) ->
	decode(Stack, Data);
decode({Type, Stack, Msg, Value, Data}, Bin) ->
	decode(Type, Stack, Msg, Value, <<Data/binary, Bin/binary>>).
	
decode(value, Stack, empty, empty, Data) ->
	decode(type, [{value, empty} | Stack], empty, empty, Data);
decode(value, [], empty, Value, <<>>) ->
	{ok, Value};
decode(value, [], empty, Value, Data) ->
	{complete, Value, Data};
decode(value, [{STATE, MSG} | Stack], empty, Value, Data) ->
	decode(STATE, Stack, MSG, Value, Data);
	
decode(map, Stack, {LM, Map}, empty, Data) ->
	decode(mapkey, [{map, {LM-1, Map}} | Stack], empty, empty, Data);
decode(map, [{STATE, MSG} | Stack], {0, Map}, KeyValue, Data) ->
	decode(STATE, Stack, MSG, {map, lists:reverse([KeyValue | Map])}, Data);
decode(map, Stack, {LM, Map}, KeyValue, Data) ->
	decode(mapkey, [{map, {LM-1, [KeyValue | Map]}} | Stack], empty, empty, Data);
	
decode(mapkey, Stack, empty, empty, Data) ->
	decode(type, [{mapkey, empty} | Stack], empty, empty, Data);
decode(mapkey, Stack, empty, MapKey, Data) ->
	decode(mapvalue, Stack, MapKey, empty, Data);
	
decode(mapvalue, Stack, MapKey, empty, Data) ->
	decode(type, [{mapvalue, MapKey} | Stack], empty, empty, Data);
decode(mapvalue, [{STATE, MSG} | Stack], MapKey, MapValue, Data) ->
	decode(STATE, Stack, MSG, {MapKey, MapValue}, Data);

decode(array, [{STATE, MSG} | Stack], {0, []}, empty, Data) ->
	decode(STATE, Stack, MSG, [], Data);	
decode(array, Stack, {LA, Array}, empty, Data) ->
	decode(type, [{array, {LA-1, Array}} | Stack], empty, empty, Data);
decode(array, [{STATE, MSG} | Stack], {0, Array}, Value, Data) ->
	decode(STATE, Stack, MSG, lists:reverse([Value | Array]), Data);
decode(array, Stack, {LA, Array}, Value, Data) ->
	decode(type, [{array, {LA-1, [Value | Array]}} | Stack], empty, empty, Data);
	
decode(raw, [{STATE, MSG} | Stack], <<?STRING:8, Raw/binary>>, empty, Data) ->
	decode(STATE, Stack, MSG, {?STRING, binary_to_list(Raw)}, Data);
decode(raw, [{STATE, MSG} | Stack], <<Type:8, Raw/binary>>, empty, Data) ->
	decode(STATE, Stack, MSG, {Type, Raw}, Data);
	
decode(type, [{STATE, MSG} | Stack], empty, empty, <<?MP_PFN:1, PFN:7, Data/binary>>) ->
	decode(STATE, Stack, MSG, PFN, Data);
decode(type, Stack, empty, empty, <<?MP_FR:3, LR:5, Raw:LR/binary, Data/binary>>) ->	
	decode(raw, Stack, Raw, empty, Data);
decode(type, [{STATE, MSG} | Stack], empty, empty, <<?MP_NFN:3, NFN:5, Data/binary>>) ->
	decode(STATE, Stack, MSG, -1*NFN, Data);
decode(type, Stack, empty, empty, <<?MP_FM:4, LM:4, Data/binary>>) ->
	decode(map, Stack, {LM, []}, empty, Data);
decode(type, Stack, empty, empty, <<?MP_FA:4, LA:4, Data/binary>>) ->
	decode(array, Stack, {LA, []}, empty, Data);
decode(type, [{STATE, MSG} | Stack], empty, empty, <<?MP_NIL:8, Data/binary>>) ->
	decode(STATE, Stack, MSG, null, Data);
decode(type, [{STATE, MSG} | Stack], empty, empty, <<?MP_FALSE:8, Data/binary>>) ->
	decode(STATE, Stack, MSG, false, Data);
decode(type, [{STATE, MSG} | Stack], empty, empty, <<?MP_TRUE:8, Data/binary>>) ->
	decode(STATE, Stack, MSG, true, Data);
decode(type, [{STATE, MSG} | Stack], empty, empty, <<?MP_FLOAT:8, Float:32/float, Data/binary>>) ->
	decode(STATE, Stack, MSG, Float, Data);
decode(type, [{STATE, MSG} | Stack], empty, empty, <<?MP_DOUBLE:8, Double/float, Data/binary>>) ->
	decode(STATE, Stack, MSG, Double, Data);
decode(type, [{STATE, MSG} | Stack], empty, empty, <<?MP_UINT8:8,  Uint:8/unsigned,  Data/binary>>) ->
	decode(STATE, Stack, MSG, Uint, Data);
decode(type, [{STATE, MSG} | Stack], empty, empty, <<?MP_UINT16:8, Uint:16/unsigned, Data/binary>>) ->
	decode(STATE, Stack, MSG, Uint, Data);
decode(type, [{STATE, MSG} | Stack], empty, empty, <<?MP_UINT32:8, Uint:32/unsigned, Data/binary>>) ->
	decode(STATE, Stack, MSG, Uint, Data);
decode(type, [{STATE, MSG} | Stack], empty, empty, <<?MP_UINT64:8, Uint:64/unsigned, Data/binary>>) ->
	decode(STATE, Stack, MSG, Uint, Data);
decode(type, [{STATE, MSG} | Stack], empty, empty, <<?MP_INT8:8,   Int:8/signed,  Data/binary>>) ->
	decode(STATE, Stack, MSG, Int, Data);
decode(type, [{STATE, MSG} | Stack], empty, empty, <<?MP_INT16:8,  Int:16/signed, Data/binary>>) ->
	decode(STATE, Stack, MSG, Int, Data);
decode(type, [{STATE, MSG} | Stack], empty, empty, <<?MP_INT32:8,  Int:32/signed, Data/binary>>) ->
	decode(STATE, Stack, MSG, Int, Data);
decode(type, [{STATE, MSG} | Stack], empty, empty, <<?MP_INT64:8,  Int:64/signed, Data/binary>>) ->
	decode(STATE, Stack, MSG, Int, Data);
decode(type, Stack, empty, empty, <<?MP_RAW16:8, LR:16, Raw:LR/binary, Data/binary>>) ->	
	decode(raw, Stack, Raw, empty, Data);
decode(type, Stack, empty, empty, <<?MP_RAW32:8, LR:32, Raw:LR/binary, Data/binary>>) ->	
	decode(raw, Stack, Raw, empty, Data);
decode(type, Stack, empty, empty, <<?MP_ARRAY16:8, LA:16, Data/binary>>) ->
	decode(array, Stack, {LA, []}, empty, Data);
decode(type, Stack, empty, empty, <<?MP_ARRAY32:8, LA:32, Data/binary>>) ->
	decode(array, Stack, {LA, []}, empty, Data);
decode(type, Stack, empty, empty, <<?MP_MAP16:8, LM:16, Data/binary>>) ->
	decode(map, Stack, {LM, []}, empty, Data);
decode(type, Stack, empty, empty, <<?MP_MAP32:8, LM:32, Data/binary>>) ->
	decode(map, Stack, {LM, []}, empty, Data);
	
decode(Type, Stack, Msg, Value, Data) ->
%	io:format("~w (~w): message-pack decode error:~nType = ~w~nStack = ~w~nMsg = ~w~nValue = ~w~nData = ~w~n", [?MODULE, self(), Type, Stack, Msg, Value, Data]),
%	null.
	{incomplete, {Type, Stack, Msg, Value, Data}}.
	
	
	
encode(Msg) ->
	encode(value, [], Msg, <<>>).
	
encode(value, [], empty, Data) ->
	Data;
encode(value, [], Msg, Data) ->
	encode(type, [{value, empty}], Msg, Data);
	
encode(integer, [{STATE, MSG} | Stack], Int, Data) ->
	Value = if
		Int < -16#80000000 -> <<?MP_INT64:8,  Int:64/signed>>;
		Int < -16#8000     -> <<?MP_INT32:8,  Int:32/signed>>;
		Int < -16#80       -> <<?MP_INT16:8,  Int:16/signed>>;
		Int < -32          -> <<?MP_INT8:8,   Int:8/signed>>;
		Int < 0            -> <<?MP_NFN:3,    (abs(Int)):5/unsigned>>;
		Int < 16#80        -> <<?MP_PFN:1,    Int:7/unsigned>>;
		Int < 16#100       -> <<?MP_UINT8:8,  Int:8/unsigned>>;
		Int < 16#10000     -> <<?MP_UINT16:8, Int:16/unsigned>>;
		Int < 16#100000000 -> <<?MP_UINT32:8, Int:32/unsigned>>;
		true               -> <<?MP_UINT64:8, Int:64/unsigned>>
	end,
	encode(STATE, Stack, MSG, <<Data/binary, Value/binary>>);
	
encode(float, [{STATE, MSG} | Stack], Float, Data) ->
	encode(STATE, Stack, MSG, <<Data/binary, ?MP_DOUBLE:8, Float:64/float>>);
	
encode(raw, [{STATE, MSG} | Stack], Raw, Data) ->
	LR = erlang:byte_size(Raw),
	Type = if
		LR < 2#100000 ->
			<<?MP_FR:3, LR:5>>;
		LR < 16#10000 ->
			<<?MP_RAW16:8, LR:16>>;
		true ->
			<<?MP_RAW32:8, LR:32>>
	end,
	encode(STATE, Stack, MSG, <<Data/binary, Type/binary, Raw/binary>>);

encode(array, [{STATE, MSG} | Stack], {0, []}, Data) ->
	encode(STATE, Stack, MSG, Data);
encode(array, Stack, {LA, [Element | Array]}, Data) ->
	encode(type, [{array, {LA-1, Array}} | Stack], Element, Data);
encode(array, Stack, Array, Data) ->
	LA = erlang:length(Array),
	Type = if
		LA < 2#10000 ->
			<<?MP_FA:4, LA:4>>;
		LA < 16#10000 ->
			<<?MP_ARRAY16:8, LA:16>>;
		true ->
			<<?MP_ARRAY32:8, LA:32>>
	end,
	encode(array, Stack, {LA, Array}, <<Data/binary, Type/binary>>);

encode(map, [{STATE, MSG} | Stack], {0, []}, Data) ->
	encode(STATE, Stack, MSG, Data);
encode(map, Stack, {map, List}, Data) ->
	LM = erlang:length(List),
	Type = if
		LM < 16 ->
			<<?MP_FM:4, LM:4>>;
		LM < 16#10000 ->
			<<?MP_MAP16:8, LM:16>>;
		true ->
			<<?MP_MAP32:8, LM:32>>
	end,
	encode(map, Stack, {LM, List}, <<Data/binary, Type/binary>>);
encode(map, Stack, {LM, [Element | List]}, Data) ->
	encode(mapkey, [{map, {LM-1, List}} | Stack], Element, Data);
encode(mapkey, Stack, {Key, Value}, Data) ->
	encode(type, [{type, Value} | Stack], Key, Data);	
	
encode(type, Stack, Msg, Data) when is_integer(Msg) ->
	encode(integer, Stack, Msg, Data);
encode(type, Stack, Msg, Data) when is_float(Msg) ->
	encode(float, Stack, Msg, Data);
encode(type, Stack, Raw, Data) when is_binary(Raw) ->
	encode(raw, Stack, <<0:8, Raw/binary>>, Data);
encode(type, Stack, Msg, Data) when is_list(Msg) ->
	encode(array, Stack, Msg, Data);
encode(type, Stack, {map, _} = Msg, Data) ->
	encode(map, Stack, Msg, Data);
encode(type, Stack, {?STRING, String}, Data) ->
	Raw = list_to_binary(String),
	encode(raw, Stack, <<?STRING:8, Raw/binary>>, Data);
encode(type, Stack, {Type, Raw}, Data) ->
	encode(raw, Stack, <<Type:8, Raw/binary>>, Data);
encode(type, [{STATE, MSG} | Stack], null, Data) ->
	encode(STATE, Stack, MSG, <<Data/binary, ?MP_NIL:8>>);
encode(type, [{STATE, MSG} | Stack], true, Data) ->
	encode(STATE, Stack, MSG, <<Data/binary, ?MP_TRUE:8>>);
encode(type, [{STATE, MSG} | Stack], false, Data) ->
	encode(STATE, Stack, MSG, <<Data/binary, ?MP_FALSE:8>>);


encode(Type, Stack, Msg, Data) ->
	io:format("~w (~w): message-pack encode error:~nType = ~w~nStack = ~w~nMsg = ~w~nData = ~w~n", [?MODULE, self(), Type, Stack, Msg, Data]),
	<<?MP_NIL:8>>.
