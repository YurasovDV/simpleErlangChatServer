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


accept_client(State, Pid) ->
Accept = gen_tcp:accept(State#state.listeningSocket),
case Accept of 
  {ok, AcceptedSock} ->  ok = gen_tcp:controlling_process(AcceptedSock, Pid);
  {error, closed} -> error(closed)
end
.

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


log(RawData) ->
  io:format("~p", [RawData]).

debug_print(Socket, RawData) ->
  {ok, {Address, Port}} = inet:peername(Socket),
  io:format("~p:~p ~p~n", [Address, Port, RawData]),
  ok.


handle_call(_Request, _From, State) ->
  {reply, ok, State}.

handle_cast(stop, State) ->
  {stop, normal, State}.


terminate(Reason, _State) ->
  ok.

code_change(_OldVsn, State, _Extra) ->
  {ok, State}.

format_status(_Opt, _StatusData) ->
  error(not_implemented).
