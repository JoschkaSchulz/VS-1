%% @author carola & joschka
%% @doc @todo Add description to client.


-module(client).
-import(werkzeug, [to_String/1,timeMilliSecond/0,logging/2]).
%% ====================================================================
%% API functions
%% ====================================================================
-export([start/2]).



%% ====================================================================
%% Internal functions
%% ====================================================================

start(Name, Server) ->
	PID = spawn(fun() -> loop(Server, 0, Name) end),
	register(Name,PID),
	PID.

get_msg_id(Server) ->
	Server ! {getmsgid, self()},
	receive
		{nnr, Number} ->
			Number		
	end.

create_message(Name, Number) -> 
	%"{~°.°}~".
	Nachricht = to_String(Name) ++ "@lab-" ++ to_String(self()) ++ "-C-1-07: " ++ to_String(Number) ++ "te_Nachtricht. C Out: " ++ timeMilliSecond() ++ "(" ++ to_String(Number) ++ ");",
	Nachricht. %io_lib:format(Nachricht ++ "~n", []).

send_message(Server,Name) ->
	Number = get_msg_id(Server),
	Nachricht = create_message(Name, Number),
	Server ! {dropmessage, {Nachricht, Number}},
	logging(to_String(Name) ++ "@lab.log", io_lib:format(Nachricht ++ "~n",[])).

read_messages(Server) ->
	Server ! {getmessages, self()},
	receive
		{reply, Number, Nachricht, Terminated} ->
			if Terminated == false ->
				read_messages(Server);
		   		true -> ok 
			end
	end.

msg_got_by_slender(Number) ->
	Nachricht = to_String(Number) ++ "te_Nachtricht um " ++ timeMilliSecond() ++ "vergessen zu senden ******",
	io_lib:format(Nachricht ++ "~n", []).

loop(Server, Counter, Name) ->
	% Redakteurclient
	if Counter < 100000 ->
		send_message(Server, Name),
		loop(Server, Counter+1, Name);
		
	   	% Leseclient
	   	true ->
			Number = get_msg_id(Server),	% 11. Client vergisst die Nachricht
			logging(to_String(Name) ++ "@lab.log",msg_got_by_slender(Number)),
			read_messages(Server),
			loop(Server, 0, Name)
	end.
