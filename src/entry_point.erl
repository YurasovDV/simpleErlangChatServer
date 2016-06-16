-module(entry_point).

%% API
-export([main/0]).

main() ->
	ok = application:load(simpleErlangChatServer),
	ok = application:start(simpleErlangChatServer),
ok.
