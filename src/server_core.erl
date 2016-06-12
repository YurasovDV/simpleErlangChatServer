-module(server_core).
-author("1").

-include("definitions.hrl").

%% API
-export([
  process_request/3,
  command_to_atom/1
]).

-spec command_to_atom/1 :: (string()) -> atom().
command_to_atom("/login") -> login;
command_to_atom("/logout") -> logout;
command_to_atom("/send") -> send;
command_to_atom("/set_nick") -> set_nick;
command_to_atom("/poll_messages") -> poll_messages.



-spec(process_request(State :: #state{}, Socket :: gen_tcp:socket(), user_command()) -> #state{}).
process_request(State, Socket, #command{action_kind = login}) ->
  Users = State#state.clientsOnlineDict,
  {ok, {Address, Port}} = inet:peername(Socket),
  Key = {Address, Port},
  Nick = create_default_nick(Address, Port),
  UserDescriptor = #client{nick = Nick, sock = Socket},

  UsersUpdated = dict:store(Key, UserDescriptor, Users),
  StateUpdated = State#state{clientsOnlineDict = UsersUpdated},
  gen_tcp:send(Socket, <<"login successful">>),
  StateUpdated;

process_request(State, Socket, #command{action_kind = send, text = Text}) ->

  NewMessage = fill_message_desc(State, Text, Socket),
  NewMessages = [NewMessage | State#state.messages],
  State1 = State#state{messages = NewMessages},

  ok = broadcast(State, NewMessage),
  State1;


process_request(State, Socket, #command{action_kind = set_nick, text = Text}) ->

  Users = State#state.clientsOnlineDict,
  {ok, Key} = inet:peername(Socket),
%%   Key = {Address, Port},
%% TODO можно убрать лишний поиск
  DescriptorOld = dict:fetch(Key, Users),
  NewDescriptor = DescriptorOld#client{nick = Text},

  UsersUpdated = dict:store(Key, NewDescriptor, Users),

  Check = dict:fetch(Key, UsersUpdated),

  StateUpdated = State#state{clientsOnlineDict = UsersUpdated},
  StateUpdated;

process_request(State, _Socket, #command{action_kind = poll_messages}) ->
  %% TODO реализовать это
  State;

process_request(State, Socket, #command{action_kind = logout}) ->
  %% Hooray, bruteforce!
  Filtered = dict:filter(fun(_Key, Value) -> Value /= Socket end, State#state.clientsOnlineDict),
  ClientsUpdated = Filtered,
  State1 = State#state{clientsOnlineDict = ClientsUpdated},
  State1.

fill_message_desc(State, Text, Socket) ->
  {ok, Key = {Address, Port}} = inet:peername(Socket),
  SenderDescriptor = dict:fetch(Key, State#state.clientsOnlineDict),
  DateTime = erlang:universaltime(),
  TextWrapped = wrap_message(Text, SenderDescriptor, DateTime),
  Msg = #message{sentFrom = {Address, Port}, text = TextWrapped, sent_when = DateTime},
  Msg.


-spec wrap_message/3 :: (string(), #client{}, {{integer(), integer(), integer()}, {integer(), integer(), integer()}}) -> string().
wrap_message(Text, SenderDescriptor, DateTime) ->
  Nick = SenderDescriptor#client.nick,
  format_time(DateTime, "[", "]") ++ " "
    ++ "(" ++ Nick ++ ")"
    ++ "  " ++ ellipsis(Text).

format_time(DateTime, Left, Right) ->
  {_Date = {_Y, _Mon, _D}, _Time = {H, Min, _S}} = DateTime,
  Left ++ integer_to_list(H) ++ ":" ++ integer_to_list(Min) ++ Right.

-spec ellipsis/1 ::(string()) -> string().
ellipsis(OldText)->
case string:len(OldText) > ?MESSAGE_MAX_LENGTH of 
  false -> OldText;
  _ ->
      Truncated = string:substr(OldText, 1, ?MESSAGE_MAX_LENGTH),
      string:concat(Truncated, ?ELLIPSIS)
end.


create_default_nick(Address, Port) ->
  %% TODO ipv6?
  {A1, A2, A3, A4} = Address,
  Nick = integer_to_list(A1)
    ++ " " ++ integer_to_list(A2)
    ++ " " ++ integer_to_list(A3)
    ++ " " ++ integer_to_list(A4)
    ++ " " ++ integer_to_list(Port),
  Nick.

-spec broadcast/2 :: (#state{}, message()) -> ok.
broadcast(State, Msg) ->
  {SenderAddress, SenderPort} = Msg#message.sentFrom,

  AllClients = dict:fetch_keys(State#state.clientsOnlineDict),


  OnlineIds = lists:filter(
    fun({Addr, Port}) -> (Addr /= SenderAddress) or (Port /= SenderPort) end,
    AllClients),

  ok = lists:foreach(fun(Id) -> send_to(Id, Msg, State) end, OnlineIds).

send_to(_Id = {Addr, Port}, Msg, State) ->
  Client = dict:fetch({Addr, Port}, State#state.clientsOnlineDict),
  Sock = Client#client.sock,
  ok = gen_tcp:send(Sock, Msg#message.text).
