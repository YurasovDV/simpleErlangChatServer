-module(request_handler).
-author("1").

-include("definitions.hrl").

-behaviour(gen_server).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3, format_status/2]).

%% API
-export([start_link/1, stop/0, accept_client/2]).

-define(NAME, ?MODULE).

-spec start_link/1 :: (Port :: integer()) -> atom().

start_link(Port) ->
  gen_server:start_link({local, ?NAME}, ?MODULE, [Port], []).

-spec stop/0 :: () -> ok.

stop() ->
  gen_server:cast(?NAME, stop).

-spec init(nonempty_maybe_improper_list()) -> {'ok',#state{clientsOnlineDict::dict:dict(_,_),listeningSocket::port(),port::char(),messages::[]},0}.
init(Args) ->

  Port = hd(Args),
  %% data sent from socket right in the mailbox
  {ok, Sock} = gen_tcp:listen(Port, [{active, true}]),
  ok = server_core:init(),
  Timeout = 0,

  {ok,
    #state
    {
      port = Port,
      listeningSocket = Sock,
      messages = [],
      clientsOnlineDict = dict:new()
    },
    Timeout}.


-spec accept_client(#state{clientsOnlineDict::'undefined' | dict:dict({_,_},#client{nick::'undefined' | string(),sock::'undefined' | port()}),listeningSocket::port(),port::'undefined' | integer(),messages::'undefined' | [#message{text::'undefined' | [any()],sentFrom::'undefined' | {_,_},sent_when::'undefined' | {_,_}}]},pid()) -> 'ok'.
accept_client(State, Pid) ->
Accept = gen_tcp:accept(State#state.listeningSocket),
case Accept of 
  {ok, AcceptedSock} ->  ok = gen_tcp:controlling_process(AcceptedSock, Pid);
  {error, closed} -> error(closed)
end
.

-spec handle_info('timeout' | {'tcp_closed',port()} | {'tcp',port(),binary() | maybe_improper_list(binary() | maybe_improper_list(any(),binary() | []) | char(),binary() | [])},_) -> {'noreply',_} | {'noreply',_,0}.
handle_info(timeout, State) ->
  spawn(?MODULE, accept_client, [State, self()]),
  {noreply, State};

handle_info({tcp_closed, Socket}, State) ->
  State1 = server_core:process_request(State, Socket, #command{action_kind = logout}),
  {noreply, State1};

handle_info({tcp, Socket, RawData}, State) ->
  debug_print(Socket, RawData),
  Timeout = 0,
  Pair = re:run(RawData,
    "(.+)\s+(.*)\s*$",
    [{capture, [1, 2], list}, ungreedy]),

  case Pair of
    {match, [Command, Text]} ->
      CommandAtom = server_core:command_to_atom(Command),
      State1 = server_core:process_request(State, Socket, #command{action_kind = CommandAtom, text = Text}),
      {noreply, State1, Timeout};

    nomatch ->
      SingleCommand = re:run(RawData,
        "(.+)$",
        [{capture, [1], list}, ungreedy]),
      case SingleCommand of
        {match, [Command]} ->
          CommandAtom = server_core:command_to_atom(Command),
          State1 = server_core:process_request(State, Socket, #command{action_kind = CommandAtom}),
          {noreply, State1, Timeout};
        _ -> {noreply, State, Timeout}
      end;
    _ ->
      log(RawData),
      {noreply, State, Timeout}
  end.


-spec log(binary() | maybe_improper_list(binary() | maybe_improper_list(any(),binary() | []) | char(),binary() | [])) -> 'ok'.
log(RawData) ->
  io:format("~p", [RawData]).

-spec debug_print(port(),_) -> 'ok'.
debug_print(Socket, RawData) ->
  {ok, {Address, Port}} = inet:peername(Socket),
  io:format("~p:~p ~p~n", [Address, Port, RawData]),
  ok.


-spec handle_call(_,_,_) -> {'reply','ok',_}.
handle_call(_Request, _From, State) ->
  {reply, ok, State}.

-spec handle_cast('stop',_) -> {'stop','normal',_}.
handle_cast(stop, State) ->
  {stop, normal, State}.


-spec terminate(_,_) -> 'ok'.
terminate(Reason, _State) ->
  ok.

-spec code_change(_,_,_) -> {'ok',_}.
code_change(_OldVsn, State, _Extra) ->
  {ok, State}.

-spec format_status(_,_) -> none().
format_status(_Opt, _StatusData) ->
  error(not_implemented).
