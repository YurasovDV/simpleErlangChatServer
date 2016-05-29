-module(entry_point).
-author("1").

%% API
-export([main/0]).

%% shit, черт

main() ->
%%   request_handler:start_link(1234),
application:load(simpleErlangChatServer),
application:start(simpleErlangChatServer),
ok.
