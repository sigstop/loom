%% loom.hrl

-include_lib("../deps/of_protocol/include/of_protocol.hrl").
-include_lib("../deps/of_protocol/include/ofp_v4.hrl").
-include_lib("../deps/pkt/include/pkt.hrl").

-define(DEFAULT_CNTL_PORT, 6633).

-define(CLEAR_ALL_FLOW_MODS,  #ofp_message{version = 4, type = flow_mod, body = #ofp_flow_mod{table_id = 0,command = delete, priority = 1, cookie = <<0,0,0,0,0,0,0,10>>,cookie_mask = <<0,0,0,0,0,0,0,0>>,match = #ofp_match{fields = []}, instructions = [] } }).
