%%%-------------------------------------------------------------------
%% @doc simpleErlangChatServer public API
%% @end
%%%-------------------------------------------------------------------

-module(simpleErlangChatServer_app).

-behaviour(application).

%% Application callbacks
-export([start/2, stop/1]).

%%====================================================================
%% API
%%====================================================================

-spec start(_,_) -> none().
start(_StartType, _StartArgs) ->
  chat_server_sup:start_link()
%  gen_server:start(request_handler, [1234], []),
  ,receive
    X -> error(X)
  end    
.

%%--------------------------------------------------------------------
-spec stop(_) -> 'ok'.
stop(_State) ->
    ok.

%%====================================================================
%% Internal functions
%%====================================================================
