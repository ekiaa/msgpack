-define(BINARY,		0).        % Raw type: binary
-define(STRING,		1).        % Raw type: utf8 string
-define(FLOATNANS,	4).        % Raw type: Not float values

-define(POS_INFINITY, <<0:1,16#7FF:11,0:52>>).
-define(NEG_INFINITY, <<1:1,16#7FF:11,0:52>>).
-define(QNAN,         <<0:1,16#7FF:11,1:1,0:51>>).
-define(SNAN,         <<0:1,16#7FF:11,0:1,1:51>>).
