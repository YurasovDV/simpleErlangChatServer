-author("1").

-export_type([user_command/0]).

-record(client,
{
  nick :: string(),
  sock :: gen_tcp:socket()
}).

-record(state, {
  clientsOnlineDict :: dict:dict({any(), any()}, client()),
  listeningSocket :: gen_tcp:socket(),
  port :: integer(),
  messages :: list(message())
}).

-record(message,
{
  text :: string(),
  sentFrom :: {any(), any()},
  sent_when :: {{integer(), integer(), integer()}, {integer(), integer(), integer()}}}).

-record(command,
{action_kind :: atom(),
  text :: string()}).


-type client() :: #client{}.
-type user_command() :: #command{}.
-type message() :: #message{}.

-define(MESSAGE_MAX_LENGTH, 10).

-define(ELLIPSIS, "[cut. message too long]").

-define(LAST_MESSAGES_COUNT, 5).