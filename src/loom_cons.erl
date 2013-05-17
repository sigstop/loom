-module(loom_cons).

-behavior(gen_server).

start_link(Port,OFProtocolVersion)->
    gen_server:start_link({local, ?MODULE}, ?MODULE, [{port,Port},{ofp_ver,OFProtocolVersion}], []).
