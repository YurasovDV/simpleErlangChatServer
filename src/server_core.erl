-module(server_core).
-author("1").

-include("definitions.hrl").

%% API
-export([
  process_request/3,
  command_to_atom/1,
  send_last_messages/2,
  init/0
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
 case check_if_logged(Socket, Users) of
  true -> State;
false ->
  {ok, {Address, Port}} = inet:peername(Socket),
  Key = {Address, Port},
  Nick = create_default_nick(Address, Port),
  UserDescriptor = #client{nick = Nick, sock = Socket},

  UsersUpdated = dict:store(Key, UserDescriptor, Users),
  StateUpdated = State#state{clientsOnlineDict = UsersUpdated},
  ok = send_last_messages(State, Socket),
  StateUpdated
end;

process_request(State, Socket, #command{action_kind = send, text = Text}) ->
  Users = State#state.clientsOnlineDict,
  IsLogged = check_if_logged(Socket, Users),
  case IsLogged of
 	true ->  

		State1 = case validate_message(Text) of
		    	ok ->
				NewMessage = fill_message_desc(State, Text, Socket),
				NewMessages = [NewMessage | State#state.messages],
  				ok = broadcast(State, NewMessage),
  				State#state{messages = NewMessages} ;
			shouldKick ->
				send_kick(Socket),
				process_request(State, Socket, #command{action_kind = logout}) 
		end,
               State1;
 	false -> State
end;


process_request(State, Socket, #command{action_kind = set_nick, text = Text}) ->

  Users = State#state.clientsOnlineDict,

 case check_if_logged(Socket, Users) of
true -> 
 {ok, Key} = inet:peername(Socket),
  DescriptorOld = dict:fetch(Key, Users),
  NewDescriptor = DescriptorOld#client{nick = Text},

  UsersUpdated = dict:store(Key, NewDescriptor, Users),

  StateUpdated = State#state{clientsOnlineDict = UsersUpdated},
  StateUpdated;
false -> State
end;

process_request(State, Socket, #command{action_kind = poll_messages}) ->
  ok = send_last_messages(State, Socket),
  State;

process_request(State, Socket, #command{action_kind = logout}) ->
  %% Hooray, bruteforce!
Filtered = dict:filter(
fun(_Key, Value) -> 
       Value#client.sock /= Socket end, 
State#state.clientsOnlineDict),

ClientsUpdated = Filtered,
  State1 = State#state{clientsOnlineDict = ClientsUpdated},
  State1.

-spec fill_message_desc(#state{}, string(), port()) -> #message{text::nonempty_string(),sentFrom::{{byte(),byte(),byte(),byte()},non_neg_integer()}}.
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

-spec format_time({{byte(), byte(), byte()}, {byte(), byte(), byte()}}, string(), string()) -> nonempty_string().
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


-spec create_default_nick({byte(),byte(),byte(),byte()},non_neg_integer()) -> nonempty_string().
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


   ok = lists:foreach(fun(Id = {Addr, Port}) ->
	if 
 	  (Addr /= SenderAddress) or (Port /= SenderPort) 
 		-> send_to(Id, Msg, State);
 	  true -> ok
 	end
  end, 
 AllClients).
 
-spec send_to({{byte(), byte(), byte(), byte()},non_neg_integer},#message{},#state{}) -> ok.
send_to(_Id = {Addr, Port}, Msg, State) ->
  Client = dict:fetch({Addr, Port}, State#state.clientsOnlineDict),
  Sock = Client#client.sock,
  TextToSend = io_lib:format("~p~n", [Msg#message.text]),
  ok = gen_tcp:send(Sock, TextToSend).

-spec(send_last_messages(State :: #state{}, ClientSock :: gen_tcp:socket()) -> ok).

send_last_messages(State, ClientSock) ->
MessageList = State#state.messages,

LastN =  select_last_messages(MessageList),

CountStr = io_lib:format("~p~n", [length(LastN)]),
ok = gen_tcp:send(ClientSock, CountStr),

ok = lists:foreach(
fun(Msg) -> 
	% io:format("sending msgs ~p~n", [Msg#message.text]),
	TextToSend = io_lib:format("~p~n", [Msg#message.text]),
	ok = gen_tcp:send(ClientSock, TextToSend)
end, 
LastN).

% -spec calc_tail(Total :: non_negative_int()) -> {ShouldTakeMessages :: boolean(), StartFrom :: non_negative_int()}.
-spec select_last_messages([#message{}]) -> [#message{}].
select_last_messages(MessageList) ->

  Total = length(MessageList),
  % either last (LAST_MESSAGES_COUNT) messages or just all messages
  Count = min(Total, ?LAST_MESSAGES_COUNT), 

LastN =  lists:reverse(lists:sublist(MessageList, Count)),
LastN.


-spec check_if_logged(port(),dict:dict(_,_)) -> boolean().
check_if_logged(Socket, ClientsOnline) ->
 {ok, {Address, Port}} = inet:peername(Socket),
 dict:is_key({Address, Port}, ClientsOnline).

-spec send_kick(port()) -> 'ok'.
send_kick(Socket) ->
ok = gen_tcp:send(Socket, io_lib:format("You were kicked~n", [])),
ok = gen_tcp:close(Socket).

-spec validate_message(string()) -> 'ok' | 'shouldKick'.
validate_message(Text) ->
	init(),
	Words = lists:map(fun(Word) -> string:to_lower(Word) end, string:tokens(Text, ",. ?!:;")),
	case check_if_rude_words(Words) of
		true ->
			 case random:uniform(10) * 10 =< ?RUDE_WORDS_KICK_CHANCE of
				true -> shouldKick;
				_ -> ok
			 end;
		false -> ok
	end.

-spec check_if_rude_words([[byte()] | char()]) -> boolean().
check_if_rude_words([]) -> false;

check_if_rude_words([W | Rest]) -> 
	case ets:lookup(rude_words_table, W) of
		[] -> false or check_if_rude_words(Rest);
		_List -> true
	end.

-spec init() -> 'ok'.
init() ->
	case ets:info(rude_words_table) of
		undefined ->
			random:seed(erlang:timestamp()),
			TabId = ets:new(rude_words_table, [set, named_table, public]),
			% Words = sets:from_list([]),
			ets:insert(TabId, {"shit", true}),
			ok;
		_ -> ok
	end.
