#!/bin/bash
erl -pa ~/Documents/Code/Erlang/simpleErlangChatServer/_build/default/lib/simpleErlangChatServer/ebin -noshell -s entry_point main -s init stop
