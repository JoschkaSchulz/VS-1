%% @author carola & joschka
%% @doc @todo Add description to client.


-module(client).
-import(werkzeug, [to_String/1,timeMilliSecond/0,logging/2,get_config_value/2]).
%% ====================================================================
%% API functions
%% ====================================================================
-export([start/2]).



%% ====================================================================
%% Internal functions
%% ====================================================================

start(Name, Server) ->
	
	%Lade Client Konfiguration
	{ok, ConfigListe} = file:consult("client.cfg"),
  	{ok, Anzahl_Clients} = get_config_value(clients, ConfigListe),
	{ok, Lifetime} = get_config_value(lifetime, ConfigListe),
	{ok, Servername} = get_config_value(servername, ConfigListe),
	{ok, Sendeintervall} = get_config_value(sendeintervall, ConfigListe),
	
	ets:new(client_config, [named_table, protected, set, {keypos,1}]),
	
	ets:insert(client_config, {clients, Anzahl_Clients}),
	ets:insert(client_config, {lifetime, Lifetime}),
	ets:insert(client_config, {servername, Servername}),
	ets:insert(client_config, {sendeintervall, Sendeintervall}),
	
	PID = spawn(fun() -> loop(Server, 0, Name) end),
	register(Name,PID),
	timer:send_after(Lifetime * 1000, PID, exit), %exit bei client muss noch gemacht werden
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
	logging(to_String(Name) ++ "@lab.log", io_lib:format(Nachricht ++ "gesendet ~n",[])).

read_messages(Server, Name) ->
	Server ! {getmessages, self()},
	receive
		{reply, Number, Nachricht, Terminated} ->
			case Terminated == false of
				true ->
					logging(to_String(Name) ++ "@lab.log", io_lib:format(Nachricht ++ "gelesen, Terminated = false ~n",[])),
					read_messages(Server, Name);
		   		false ->
					logging(to_String(Name) ++ "@lab.log", io_lib:format(Nachricht ++ "gelesen, Terminated = true ~n",[]))
			end
	end.

msg_got_by_slender(Number) ->
	Nachricht = to_String(Number) ++ "te_Nachtricht um " ++ timeMilliSecond() ++ "vergessen zu senden ******",
	io_lib:format(Nachricht ++ "~n", []).

loop(Server, Counter, Name) ->
	% Redakteurclient
	if Counter < 5 ->
		send_message(Server, Name),
		loop(Server, Counter+1, Name);
		
	   	% Leseclient
	   	true ->
			Number = get_msg_id(Server),	% 11. Client vergisst die Nachricht
			logging(to_String(Name) ++ "@lab.log",msg_got_by_slender(Number)),
			read_messages(Server, Name),
			loop(Server, 0, Name)
	end.
