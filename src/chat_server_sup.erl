-module(chat_server_sup).

-behaviour(supervisor).

-export([init/1, start_link/0]).

start_link() ->
	Args = [],
	%			module with init/1
	supervisor:start_link(chat_server_sup, Args).

init(_Args) ->
	SupFlags = #{startegy => one_for_one, 
			intensity => 100,
			period => 1},
	ChildSpecs = [#{
			id => server_proc,
			start => { request_handler, start_link, [1234]},
			restart => transient,
			modules => [request_handler]
			}],
	{ok, {SupFlags, ChildSpecs}}.
