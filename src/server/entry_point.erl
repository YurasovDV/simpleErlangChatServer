-module(entry_point).
-author("1").

%% API
-export([server_main/0]).

%% shit, черт

server_main() ->
%%   request_handler:start_link(1234),
  gen_server:start(request_handler, [1234], []),
  receive
    X -> error(X)
  end.
