%% @author carola & joschka
%% @doc @todo Add description to server.


-module(server).
-import(werkzeug, [to_String/1,timeMilliSecond/0,logging/2,get_config_value/2,reset_timer/3]).
%% ====================================================================
%% API functions
%% ====================================================================
-export([start/0, test/0]).


%% ====================================================================
%% Internal functions
%% ====================================================================

start() ->
	HBQ = dict:new(),
	DLQ = dict:new(),
	Clients = dict:new(),
	
	%Lade Server Konfiguration
	{ok, ConfigListe} = file:consult("server.cfg"),
  	{ok, Lifetime} = get_config_value(lifetime, ConfigListe),
	{ok, Clientlifetime} = get_config_value(clientlifetime, ConfigListe),
	{ok, Dlqlimit} = get_config_value(dlqlimit, ConfigListe),
	{ok, Servername} = get_config_value(servername, ConfigListe),
	
	ets:new(server_config, [named_table, protected, set, {keypos,1}]),
	
	ets:insert(server_config, {lifetime, Lifetime}),
	ets:insert(server_config, {clientlifetime, Clientlifetime}),
	ets:insert(server_config, {dlqlimit, Dlqlimit}),
	ets:insert(server_config, {servername, Servername}),
	
	%[{servername,SN}] = ets:lookup(config, servername),

	PID = spawn(fun() -> loop(0, HBQ, DLQ, Clients) end),
	register(Servername,PID),
	timer:send_after(Lifetime * 1000, PID, exit),
	PID.

% empfangene Nachricht in HBQ speichern
dropmessage(HBQ, Nachricht, Number) ->
	%Systemzeit anhängen
	New_Nachricht = Nachricht ++ " HBQ In: " ++ timeMilliSecond(),
	New_HBQ = dict:store(Number, New_Nachricht, HBQ),
    logging("NServer.log", io_lib:format(New_Nachricht ++ "~n",[])),
	New_HBQ.

test() ->
	DLQ = dict:new(),
	Tmp_HBQ = dict:new(),
	HBQ = dict:store(0, "Test 0", dict:store(1, "Test 1", dict:store(3, "Test 3", Tmp_HBQ))),
	New_DLQ = check_dlq(DLQ,HBQ),
	New_HBQ = clear_hbq(lists:max(dict:fetch_keys(New_DLQ)),HBQ),
	io:format("HBQ: ~p DLQ: ~p~n", [dict:fetch_keys(New_HBQ),dict:fetch_keys(New_DLQ)]).

% evtl. Übertragen von Nachrichten von HBQ nach DLQ
check_dlq(DLQ, HBQ) ->
	% wenn Elemente in DLQ vorhanden
	case dict:size(DLQ) > 0 of
		true ->
			% dann ist Next_number = Maximale Nummer in DLQ + 1
			Next_number = lists:max(dict:fetch_keys(DLQ))+1;
		    % sonst (wenn DLQ leer) ist Next_number = 0
		false ->
			Next_number = 0
	end,
	% wenn Next_number in HBQ enthalten ist
	case dict:is_key(Next_number, HBQ) of
		true ->
			% dieses Element in HBQ finden und speichern
			Tupel = dict:find(Next_number, HBQ),
			Nachricht = element(2, Tupel),
			%Systemzeit anhängen
			New_Nachricht = Nachricht ++ " DLQ In: " ++ to_String(timeMilliSecond()),
			% prüfen, ob DLQ ihr Limit erreicht hat
			[{_,Dlqlimit}] = ets:lookup(server_config, dlqlimit),
			case dict:size(DLQ) >= Dlqlimit of
			  % wenn ja, kleinste Nummer in DLQ löschen und durch neue Nachricht ersetzen
			  true ->
				Min = lists:min(dict:fetch_keys(DLQ)),
			  	New_DLQ = dict:store(Next_number, New_Nachricht, dict:erase(Min, DLQ));
			  % wenn nicht, Nachricht in DLQ speichern
		  	  false ->
				New_DLQ = dict:store(Next_number, New_Nachricht, DLQ)
			end,
			logging("NServer.log", io_lib:format(New_Nachricht ++ "~n", [])),
			% rekursiver Aufruf, da noch mehr Elemente von HBQ nach DLQ übertragen werden könnten
			check_dlq(New_DLQ, HBQ);
		% sonst (wenn Next_number nicht in HBQ enthalten) abbrechen
		false ->
			DLQ
	end.

% löscht alle Werte aus HBQ, die <= Max sind
clear_hbq(Max,HBQ) ->
	case Max < 0 of
		true ->
			HBQ;
		false ->
			clear_hbq(Max-1,dict:erase(Max, HBQ))
	end.

check_error(DLQ, HBQ, From, To) ->
	case dict:is_key(To, HBQ) of
		true ->
			Fehler = "***Fehlernachricht fuer Nachrichtennummern " ++ to_String(From) ++ " bis " ++ to_String(To-1) ++ " um " ++ to_String(timeMilliSecond()),
			New_DLQ = dict:append(To-1, Fehler, DLQ),
			logging("NServer.log", io_lib:format(Fehler ++ "~n", [])),
			New_DLQ;
		false ->
			check_error(DLQ,HBQ,From,To+1)
	end.
		 	
check_error(DLQ, HBQ) ->
	[{_,Dlqlimit}] = ets:lookup(server_config, dlqlimit),
	case dict:size(HBQ) > Dlqlimit/2 of
		true ->
			Current = lists:max(dict:fetch_keys(DLQ))+1,
			check_error(DLQ, HBQ, Current, Current);
		false ->
			DLQ
	end.

getmessages(Client, Clients, DLQ) ->
	[{_, Sekunden}] = ets:lookup(server_config, clientlifetime),
	% wenn DLQ keine Nachrichten enthält
	case dict:size(DLQ) == 0 of
		true ->
		  	case dict:is_key(Client, Clients) of
			  true ->
		    	{ok,{_,Timer}} = dict:find(Client, Clients),
			    Client ! {reply, -1, "Keine Eintraege vorhanden!", true},
		  		reset_timer(Timer,Sekunden,{endoflifetime, Client}),
			    dict;
			  false ->
				Client ! {reply, -1, "Keine Eintraege vorhanden!", true},
				{ok,New_Timer} = timer:send_after(Sekunden * 1000,{endoflifetime, Client}),
				dict:store(Client, {0, New_Timer}, Clients)
			end;
		false ->
			% wenn der Client beim Server bekannt ist
			case dict:is_key(Client, Clients) of
				true ->
				  	{ok,{Current,Timer}} = dict:find(Client, Clients),
					Max = lists:max(dict:fetch_keys(DLQ)),
					% ist die zuletzt gelesene Nachrichtennr des Clients = dem Maximalwert in der DLQ
					case Current == Max of
						true ->
							% schicke Fehlernachricht, da der Client keine weiteren Nachrichten lesen kann
							reset_timer(Timer,Sekunden,{endoflifetime, Client}),
						  	Client ! {reply, -1, "Keine Eintraege vorhanden!", true},
							dict;
						false ->
							% wenn nicht, suche nächste Nachricht in DLQ (get_next_message)
							% Anwort besteht aus dem 4er Tupel {reply, Number, Nachricht, true/false}
							Antwort = get_next_message(DLQ, Current+1, Max),
							io:format("Antwort: ~p~n", [Antwort]),
							Client ! Antwort,
							% zuletzt gelesene Nachricht des Clients aktualisieren
							reset_timer(Timer,Sekunden,{endoflifetime, Client}),
							dict:store(Client, {element(3,Antwort),Timer}, dict:erase(Client, Clients))
					end;
				false ->
					% kennt der Server den Client nicht, schickt er ihm die Nachricht mit kleinster Nummer
					Min = lists:min(dict:fetch_keys(DLQ)),
					{ok, Nachricht} = dict:find(Min, DLQ),
					% wenn nur 1 Element in DLQ, lesen beenden (Terminated = true)
					Client ! {reply, Min, Nachricht, dict:size(DLQ) == 1},
					% Client in Dictionary speichern, damit er beim nächsten Aufruf bekannt ist
					{ok,New_Timer} = timer:send_after(Sekunden * 1000,{endoflifetime, Client}),
					dict:store(Client, {Min, New_Timer}, Clients)
			end
	end.

% nächste Nachricht aus der DLQ für Client suchen
get_next_message(DLQ, Current, Max) ->
	case dict:is_key(Current, DLQ) of
		true ->
			{ok, Nachricht} = dict:find(Current, DLQ),
			case Current == Max of
				true ->
					{reply, Current, Nachricht, true};
				false ->
				    {reply, Current, Nachricht, false}
			end;
		false ->
			get_next_message(DLQ, Current+1, Max)
	end.

shutdown(Clients) ->
	case dict:size(Clients) == 0 of
		true ->
			exit;
		false ->
			Client = lists:min(dict:fetch_keys(Clients)),
			{ok,{_,Timer}} = dict:find(Client, Clients),
			timer:cancel(Timer),
			shutdown(dict:erase(Client, Clients))
	end.
	
loop(Counter, HBQ, DLQ, Clients) ->
	receive
		{getmsgid, Client} ->
			Client ! {nnr,Counter},
			loop(Counter+1, HBQ, DLQ, Clients);
		% Server empfängt Message
		{dropmessage, {Nachricht, Number}} ->
		    % Message wird in HBQ gespeichert
			TMP_HBQ = dropmessage(HBQ, Nachricht, Number),
		  	% evtl. Übertragen von Messages von HBQ nach DLQ
			TMP_DLQ = check_dlq(DLQ, TMP_HBQ),
		    % löschen der Nachrichten aus HBQ, die jetzt in der DLQ sind
			New_HBQ = clear_hbq(lists:max(dict:fetch_keys(TMP_DLQ)),TMP_HBQ),
		   	logging("NServer.log", io_lib:format("HBQ: ~p DLQ: ~p~n", [dict:fetch_keys(New_HBQ),dict:fetch_keys(TMP_DLQ)])),
		    New_DLQ = check_error(TMP_DLQ, New_HBQ),
			loop(Counter,New_HBQ,New_DLQ, Clients);
		{getmessages, Client} ->
	   		Changed_Clients = getmessages(Client, Clients, DLQ),
			loop(Counter, HBQ, DLQ, Changed_Clients);
		{endoflifetime, Client} ->
			loop(Counter, HBQ, DLQ, dict:erase(Client, Clients));
		exit ->
	   		shutdown(Clients)
	end.


