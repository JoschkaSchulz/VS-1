%% @author carola & joschka
%% @doc @todo Add description to client.


-module(client).
-import(werkzeug, [to_String/1,timeMilliSecond/0,logging/2,get_config_value/2]).
%% ====================================================================
%% API functions
%% ====================================================================
-export([start/1]).



%% ====================================================================
%% Internal functions
%% ====================================================================

start(Server_node) ->
	
	%Lade Client Konfiguration
	{ok, ConfigListe} = file:consult("client.cfg"),
  	{ok, Anzahl_Clients} = get_config_value(clients, ConfigListe),
	{ok, Lifetime} = get_config_value(lifetime, ConfigListe),
	{ok, Servername} = get_config_value(servername, ConfigListe),
	{ok, Sendeintervall} = get_config_value(sendeintervall, ConfigListe),
	
	%ets:new(client_config, [named_table, protected, set, {keypos,1}]),
	
	%ets:insert(client_config, {clients, Anzahl_Clients}),
	%ets:insert(client_config, {lifetime, Lifetime}),
	%ets:insert(client_config, {servername, Servername}),
	%ets:insert(client_config, {sendeintervall, Sendeintervall}),
	
	Server = {Servername,Server_node},
	start_client(Anzahl_Clients, Server, Sendeintervall, Lifetime),
	ok.

% starte alle Clienten
start_client(Number, Server, Sendeintervall, Lifetime) ->
	case Number =< 0 of
		true ->
			io:format("Clients gestartet.");
		false ->
			Name = "client" ++ to_String(Number),
			PID = spawn(fun() -> loop(Server, 0, Name, dict:new(), Sendeintervall) end),
			%register(Name,PID),
			{ok, Hostname} = inet:gethostname(),
			logging(lists:append([[Name,"@",Hostname,".log"]]), io_lib:format("~s@~s Start: ~s~n",[Name,Hostname,timeMilliSecond()])),
			% Timer für die Lebenszeit des Clients starten
			timer:exit_after(Lifetime * 1000, PID, "End of Lifetime"),
			start_client(Number-1, Server, Sendeintervall, Lifetime)
	end.

% fordere Message-ID vom Server an
get_msg_id(Server) ->
	Server ! {getmsgid, self()},
	receive
		{nnr, Number} ->
			Number		
	end.

% erzeuge die Nachricht als String
create_message(Name, Number) -> 
	{ok, Hostname} = inet:gethostname(),
	lists:flatten(io_lib:format("~s@~s-C-1-07: ~p ~pte_Nachtricht. C Out: ~s (~p);", [Name, Hostname,self(),Number,timeMilliSecond(),Number])).
	%Nachricht. %io_lib:format(Nachricht ++ "~n", []).

% erfrage eine Message-ID und sende die Nachricht an den Server
send_message(Server,Name,Own_Messages, Sendeintervall) ->
	Number = get_msg_id(Server),
	Nachricht = create_message(Name, Number),
	% Timer starten, da erst nach x Sekunden gesendet werden soll
	timer:send_after(Sendeintervall*1000, sendmessage),
	receive
		sendmessage ->
			Server ! {dropmessage, {Nachricht, Number}},
			{ok, Hostname} = inet:gethostname(),
			logging(lists:append([[Name,"@",Hostname,".log"]]), io_lib:format(Nachricht ++ " gesendet ~n",[])),
			% Message-ID des eigenen Redakteurs wird gespeichert
			New_Own_Messages = dict:store(Number, Nachricht, Own_Messages),
			New_Own_Messages
	end.

% lese Nachrichten vom Server
read_messages(Server, Name,Own_Messages) ->
	Server ! {getmessages, self()},
	receive
		{reply, Number, Nachricht, Terminated} ->
			case dict:is_key(Number, Own_Messages) of
				% wenn die Message-ID vom eigenen Redakteur gesendet wurde, hänge ****** an
				true ->
					Is_own_msg = " *******";
				false ->
					Is_own_msg = ""
			end,
			{ok, Hostname} = inet:gethostname(),
			case Terminated == false of
				true ->
					logging(lists:append([[Name,"@",Hostname,".log"]]), io_lib:format(Nachricht ++ Is_own_msg ++ " gelesen, Terminated = false ~n",[])),
					% wenn Terminated = false, weiter Nachrichten vom Server lesen
				  	read_messages(Server, Name, Own_Messages);
		   		false ->
					logging(lists:append([[Name,"@",Hostname,".log"]]), io_lib:format(Nachricht ++ Is_own_msg ++ " gelesen, Terminated = true ~n",[]))
			end
	end.

% generiere die "Vergessen zu senden"-Nachricht
message_lost(Number) ->
	Nachricht = to_String(Number) ++ "te_Nachtricht um " ++ timeMilliSecond() ++ " vergessen zu senden ******",
	io_lib:format(Nachricht ++ "~n", []).

% berechne Sendeintervall neu
calculate_interval(Sendeintervall) ->
	random:seed(now()),
	Faktor = case random:uniform(2) of
    	1 -> 0.5;
    	_ -> -0.5
  	end,
  	Sendeintervall_neu = Sendeintervall + (Sendeintervall * Faktor),

	% Sendeintervall darf nicht unter 1 fallen
  	case Sendeintervall_neu < 1 of
  		true -> 
			1;
   		false -> 
			round(Sendeintervall_neu)
  	end.

% Endlosschleife des Servers
loop(Server, Counter, Name, Own_Messages, Sendeintervall) ->
	% Redakteurclient
	if Counter < 5 ->
		New_Own_Messages = send_message(Server, Name, Own_Messages, Sendeintervall),
		loop(Server, Counter+1, Name, New_Own_Messages, Sendeintervall);
			
	   	% Leseclient
	   	true ->
			{ok, Hostname} = inet:gethostname(),
			Number = get_msg_id(Server),	% Client vergisst die Nachricht
			logging(lists:append([Name,"@",Hostname,".log"]), message_lost(Number)),
			read_messages(Server, Name,Own_Messages),
			Sendeintervall_neu = calculate_interval(Sendeintervall),
			logging(lists:append([Name,"@",Hostname,".log"]), io_lib:format("Neues Sendeintervall: ~s Sekunden ~n", [to_String(Sendeintervall_neu)])),
			loop(Server, 0, Name,Own_Messages, Sendeintervall_neu)
	end.
