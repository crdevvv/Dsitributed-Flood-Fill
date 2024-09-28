-module(graph).
-behaviour(gen_server). 
% definisco il comportamento del modulo, in quanto dovrà gestire molteplici chiamate 
% dai nodi del grafo. Il modulo principale dunque funge da "server" per quanto riguarda 
% le chiamate da parte dei processi, e da "monitor/supervisor" per poter rianimare i nodi 
% che vanno in crash. La nostra architettura dunque è assimilabile al tipo Client-Server, 
% dove i nodi della rete sono i Client interconnessi, mentre il modulo grafo rappresenta 
% il Server al quale arrivano le chiamate.

% l'export delle funzioni
-export([from_file/1, from_file/3, del_graph/1, vertices/1, edges/1, edge_weight/2,
        edges_with_weights/1, out_neighbours/2, num_of_vertices/1, equal/2,
        num_of_edges/1, pprint/1, empty/1, empty/2, add_vertex/2, add_edge/3,
        add_edge/4, graph_type/1, del_edge/2, weight_type/1, export/3, import/2, demo/0,
        rnd_color/3, spawnP/3, spawnI/3, assign_status/2, print_tuple_list_with_foreach/1,
        merge_list/2,communicate/3, extract/1, my_color/2,handle_mess/0, update_target/3, assign_target/2, my_target/2,
        init/1, start_link/0, write_result_to_file/4, handle_call/3, handle_cast/2, handle_info/2, restart_process/1,
        write_to_file/2, read_file/1, read_lines/2, parse_line/1, merge_nc/3, update_list/4]).

-export_type([graph/0, vertex/0, edge/0, weight/0,edge_list/0]).

-define(NODELIST_FILE, "NodeList.txt"). % definisco il nome del file con una macro, così lo richiamo con ?NODELIST_FILE

%% @type graph(). Un grafo diretto o indiretto
-record(graph, {
  type       :: graphtype(),
  graph      :: digraph:graph(),
  weightType :: weighttype()
}).
-type graph()      :: #graph{}.
-type vertex()     :: term().
-type edge()       :: {vertex(), vertex()}.
-type graphtype()  :: directed | undirected.
-type weight()     :: number().
-type weighttype() :: unweighted | d | f.
-type edge_list()  :: [{edge(),weight()}].


%% Crea un nuovo grafo non pesato vuoto
-spec empty(graphtype()) -> graph().

empty(Type) when Type =:= directed; Type =:= undirected ->
  #graph{type=Type, graph=digraph:new(), weightType = unweighted}.

%% Crea un nuovo grafo con un certo peso specificato vuoto
-spec empty(graphtype(), weighttype()) -> graph().

empty(T, WT) when (T =:= directed orelse T =:= undirected) andalso
                  (WT =:= unweighted orelse WT =:= d orelse WT =:= f) ->
  #graph{type=T, graph=digraph:new(), weightType=WT}.

%% main demo 
demo() ->

  io:format("Inserisci il numero del grafo da utilizzare (nr. vertici grafo 1: 13, nr. vertici grafo 2: 10, nr. vertici grafo 3: 23) >> "),
  {ok, InputGraph} = io:read(""),
  io:format("Inserisci il numero di colori da utilizzare (compreso fra 2 e 4) >> "),
  {ok, InputColor} = io:read(""),
  io:format("Inserisci il numero di nodi iniziatori >> "),
  {ok, InputInitiator} = io:read(""),
  
  %% lettura grafo scelto
  {ReadVertices,ListEdges, IO, N} = case InputGraph of
        1 -> from_file("graph1.txt");
        2 -> from_file("graph2.txt");
        3 -> from_file("graph3.txt");
        _ -> io:format("Inserire un numero compreso fra 1 e 3~n"), demo()
    end,

  % scrittura vertici grafo su vettore Vertices
  Vertices = ReadVertices(IO, N),

  % lista di colori
  ColorList = ["Red","Green","Blue","Yellow"],

  % handle color selection
  Result = case InputColor of
      2 -> rnd_color(Vertices, ColorList, 2);
      3 -> rnd_color(Vertices, ColorList, 3);
      4 -> rnd_color(Vertices, ColorList, 4);
      _ -> io:format("Inserire un numero compreso fra 2 e 4 ~n"), demo()
  end,

  % fase iniziale : scrivo sul file i colori che i nodi hanno all'inizio
  % successivamente, la sovrascrittura del file sarà gestita dal gen_server, per evitare sovrascritture concorrenti
  % richiamo la macro ?NODELIST_FILE per ottenere il nome del file
  {ok, File} = file:open(?NODELIST_FILE, [write]),
  % Write each tuple in the result list to the file
  lists:foreach(fun({Vertex, Color}) -> 
                    io:format(File, "~w ~s~n", [Vertex, Color]) 
                end, Result),

  % Close the file
  file:close(File),
  
  spawn(fun() -> % creo un processo dedicato per la gestione dell'esecuzione di Python
      os:cmd("C:/Users/alber/AppData/Local/Microsoft/WindowsApps/python3.7.exe \"c:/Program Files/Erlang OTP/usr/draw_graph.py\" graph"++integer_to_list(InputGraph)++".txt")
    end),  % uso os:cmd all'interno di spawn, altrimenti richiamando solo os:cmd blocca l'esecuzione del resto del codice Erlang per eseguire il programma Python  

  timer:sleep(5000), % after 5 seconds I start everything

  % NodesL: {id nodo, colore, stato, target}
  NodesStatus = assign_status(Result,InputInitiator),
  NodesTarget = [{IdN,C,S,null} || {IdN,C,S} <- NodesStatus],
  NodesL = assign_target(NodesTarget,InputInitiator),
  print_tuple_list_with_foreach(NodesL), % stampo la lista iniziale
  % Lista egdes filtrati senza il peso ovvero tuple composte da {Nodo1,Nodo2} ordinati secondo il primo nodo della tupla  
  AdjList = [ {N1,N2} || {N1,N2, _} <- ListEdges],
  Init1 = [ {IdN,C,S,T} || {IdN,C,S,T} <- NodesL, S == "iniziale"],
  % io:format("adj ~p~n",[AdjList]),

  InitLen = length(Init1),                % lunghezza della lista di nodi Initiator
  NonInitLen = length(NodesL) - InitLen,  % lunghezza della lista di nodi Non Initiator

  start_link(), % faccio partire il gen_server, che rimane in attesa di chiamate attraverso il suo link
  NonInitP = spawnP(NonInitLen,N,[]), % NonInitP: {id nodo,id processo}
  InitP = spawnI(InputInitiator,N-NonInitLen,[]), % InitP: {id nodo,id processo}
  MergedProc = InitP ++ NonInitP,
  % NodesL = addPrToNodes(Nodes,MergedProc),
  % io:format("merged proc : ~p~n", [MergedProc]),
  % dopo spawnP si creano N nodi che vengono registrati e rimangono attivi finchè uno di questi termina
  % FinalL contiene l'unione delle liste ProcL, Nodes
  % [{idN,processo,colore,stato},...]
  FinalL = merge_list(MergedProc,NodesL),

  %% InitiatorL : lista dei nodo initiator con i campi {id nodo,colore, stato(iniziale), id processo,Target}
  InitiatorL = extract(FinalL),
  
  run_initiators(NodesL,InitiatorL,AdjList), % avvia gli initiators, iniziando la comunicazione nel body della funzione
  %timer di un secondo prima di uccidere tutti i processi
  
  timer:sleep(60000), % dopo 1 minuto, uccide i processi
  kill_all_registered_processes(MergedProc). % uccido tutti i processi relativi ai nodi

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%% GEN SERVER
init([]) -> {ok, []}. % inizializza il gen_server

start_link() -> % apre il collegamento del gen_server
  gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

write_result_to_file(FileName, OldList, PidF, PidP) -> % la write result contatta il server con call
  gen_server:call(?MODULE, {write, FileName, OldList, PidF, PidP}).

% From = PidF, self() = PidP
% gestisce le chiamate al server
handle_call({write, FileName, OldList, PidF, PidP}, _From, State) ->

  % calcolo target e aggiorno liste in maniera sequenziale (un nodo alla volta, perchè più nodi potrebbero entrare in response)
  TargetColor = my_target(PidP, OldList),
  Updated1 = update_color(TargetColor,PidP,OldList),% aggiorno il colore del padre con il suo target
  Updated2 = update_target(TargetColor,PidF,Updated1),% aggiorno il colore target dell'adiacente con il target del padre
  io:format("Il colore target di ~p è stato aggiornato con ~p~n", [get_alias(PidF),TargetColor]),

  % quale processo/nodo sta scrivendo sul file
  io:format("Il nodo ~p scrive su file ~n", [get_alias(element(1,_From))]),

  % liste di tuple con 2 parametri {Nodo, Colore}
  NodeColOld = read_file(FileName),
  NodeColUpdated = [{N,C} || {N,C,_, _} <- Updated2],

  % liste di tuple con 4 parametri {Nodo, Colore, Stato, Target}
  MergeNodCol = merge_nc(NodeColOld,NodeColUpdated,[]),
  MergedFinal = update_list(Updated2,OldList, PidF, PidP),

  {ok, File} = file:open(FileName, [write]),
  % Write each tuple in the result list to the file
  lists:foreach(fun({Vertex, Color}) -> 
                    io:format(File, "~w ~s~n", [Vertex, Color]) 
                end, MergeNodCol),
  % Close the file
  file:close(File),
  {reply, {ok, MergedFinal}, State}. % la tupla che il server deve ritornare deve avere esattamente 3 elementi

% gestisce messaggi asincroni
handle_cast({add_process, Alias, Pid}, State) ->
    {noreply, [{Alias, Pid} | State]}.

% quando riceve un messaggio di down da un processo, lo fa ripartire
handle_info({'DOWN', process, Pid, _Reason}, State) ->
  {ok, NewPid} = restart_process(Pid),
  NewState = lists:keyreplace(Pid, 2, State, {get_alias(NewPid), NewPid}),
  {noreply, NewState}.

% spawna il nuovo processo e lo registra con l'alias precedente
% questo perchè quando il processo manda il messaggio di DOWN, ha ancora l'alias con cui era registrato
% chiamando la get_alias, si ottiene l'alias precedente, che viene usato per registrare il nuovo processo creato
restart_process(Pid) ->
  OldAlias = get_alias(Pid),
  {ok, NewPid} = spawn(graph, handle_mess, []),
  register(OldAlias, NewPid),
  {ok, NewPid}.

% funzione che legge le righe dal file NodeList.txt, e le mette in una lista
read_file(FileName) ->
    {ok, File} = file:open(FileName, [read]), 
    List = read_lines(File, []),  
    file:close(File), 
    List.

% Funzione per leggere le righe del file
read_lines(File, List) ->
    case io:get_line(File, "") of
        eof -> % quando si raggiunge la fine del file, viene ritornata la lista
            List; 
        Line -> 
            % Rimuovi eventuali newline dalla fine della riga
            CleanLine = string:trim(Line),
            Tup = parse_line(CleanLine),
            read_lines(File, List++[Tup])
    end.

% gestisce le righe lette, e separa i due campi Nodo Colore
parse_line(Line) ->
    [NodoStr, ColorStr] = string:tokens(Line, " "),
    Nodo = list_to_integer(NodoStr),  
    Color = list_to_atom(ColorStr), 
    {Nodo, Color}. 

% qualora il colore sia diverso tra una tupla e l'altra,
% si sceglie la tupla con il nuovo colore
merge_nc([],[],Lista) -> Lista;
%merge_nc(NodeColOld, NodeColUpdated, [])
merge_nc([{N1,C1} | T1],[{_,C2} | T2], Lista) ->
  case C1 of
    C2 -> merge_nc(T1,T2, Lista++[{N1,C1}]);
    _ -> merge_nc(T1,T2, Lista++[{N1,C2}])
  end.

% aggiorna la lista, per permettere le ricolorazioni
update_list(UpdatedList, OldList, PidFiglio, PidPadre) ->
      lists:foldl(fun({IdNodo, Color, Status, Target}, Acc) ->
          IdNodoConvert = list_to_atom(integer_to_list(IdNodo)),
          NewTuple = case whereis(IdNodoConvert) of %IdNodo è un intero, va convertito in atomo  % Controllo il pid
              PidFiglio ->  % Se il pid dentro la tupla è PidFiglio
                  {IdNodo, Color, Status, my_target(PidFiglio, UpdatedList)};  % Aggiorno il target
              PidPadre ->  % Se il pid dentro la tupla è PidPadre
                  {IdNodo, my_color(PidPadre, UpdatedList), Status, Target};  % Aggiorno il colore
              _ ->
                  {IdNodo, Color, Status, Target}  % Non aggiorno
          end,
          [NewTuple | Acc]  % Aggiungo la nuova tupla alla lista accumulata
      end, [], OldList).  % Parto con un accumulatore vuoto

%% spawn nodi initiator 
%% i nodi vengono registrati ciascuno con un alias associato al suo pid
%% Alias: identificativo del nodo ovvero numero da 1 a |G|
spawnI(0, _, Lista ) -> Lista;
spawnI(N, Acc, Lista) -> 
      Pid = spawn(graph, handle_mess, []),
      % conversione del numero intero in lista di caratteri, e da lista in atomo
      Alias = list_to_atom(integer_to_list(Acc)),
      % registrazione del processo con un alias (che corrisponde al numero)
      register(Alias, Pid),
      io:format("Process registered with name ~p and pid ~p~n", [Alias,Pid]),
      gen_server:cast(?MODULE, {add_process, Alias, Pid}),  
      spawnI(N-1,Acc-1,[{Alias,Pid} | Lista]). % acc (accumulatore) è necessario per poter stampare i processi in ordine crescente, da 1 a N

%% spawn nodi non initiator
%% ritorna lista {alias,pid nodo}
%% i nodi vengono registrati ciascuno con un alias associato al suo pid
%% Alias: identificativo del nodo ovvero numero da 1 a |G|
spawnP(0, _, Lista) -> Lista; 
spawnP(N, Acc, Lista) -> 
    Pid = spawn(graph, handle_mess, []),
    % conversione del numero intero in lista di caratteri, e da lista in atomo
    Alias = list_to_atom(integer_to_list(Acc)),
    % registrazione del processo con un alias (che corrisponde al numero)
    register(Alias, Pid),
    io:format("Process registered with name ~p and pid ~p~n", [Alias,Pid]),
    gen_server:cast(?MODULE, {add_process, Alias, Pid}),
    spawnP(N-1,Acc-1,[{Alias,Pid} | Lista]). % acc (accumulatore) è necessario per poter stampare i processi in ordine crescente, da 1 a N

%% InitiatorL: [{idNodo,idProcesso ,colore, iniziator, Target},...]
%% NodesL: [{idNodo, colore, stato (initiator/non initiator),Target},...]
%% AdjList: [{Nodo1,NodoAdiacente}, {Nodo2,NodoAdiacente},...]
run_initiators(NodesL, InitiatorL, AdjList) ->
    lists:foreach(
  fun({Nodo1, _, _, _, _}) -> % fa lo spawn per poter fare un'esecuzione concorrente, quindi più processi eseguono in contemporanea 
          spawn(fun() -> communicate(NodesL, Nodo1, AdjList) end)
        end, InitiatorL).

%% funzione che gestisce lo scambio di messaggi fra due nodi: nodo->nodo_adiacente
%% Nodo1 alias del nodo che invia la richiesta
%% NodesL lista che contiene [{IdN,Color,Stato,Target},...]
%% AdjList lista di adiacenza del nodo Nodo1
communicate(NodesL, Nodo1, AdjList) ->

  Pid1 = whereis(Nodo1),
  ColorePid1 = my_color(Pid1,NodesL),

  % NodiAdiacenti prende in considerazione tutti e due gli elementi della tupla, per trovare tutti gli adiacenti del nodo. 
  % Viene fatta un'unione delle due liste, per ottenere quella completa  
  NodiAdiacenti = [N1 || {N1,N2} <- AdjList, integer_to_list(N2) == atom_to_list(Nodo1)] ++ [N2 || {N1,N2} <- AdjList, integer_to_list(N1) == atom_to_list(Nodo1)],
  lists:foreach(fun(X) -> integer_to_list(X) end, NodiAdiacenti), % converto gli interi letti in list
  
  % io:format("Nodi adiacenti a ~p: ~p~n",[Nodo1,NodiAdiacenti]),
  % Invia un messaggio a ciascun nodo adiacente
  % {request_color,...} viene inviato a NFiglio
  lists:foreach(fun(NFiglio) ->
      Pid2 = whereis(list_to_atom(integer_to_list(NFiglio))),
      Pid2 ! {request_color, Pid1, ColorePid1, NodesL, AdjList},
      io:format("~n~p inviato da ~p che ha colore ~p verso ~p~n",[request_color, Nodo1, ColorePid1, list_to_atom(integer_to_list(NFiglio))])
      end, NodiAdiacenti).
       
% funzione che gestisce la comunicazione e il protocollo 3-way handshake
handle_mess() ->
        receive % il figlio riceve dal padre
          {request_color, From, ColoreNodo, NodesL, AdjList} ->
                    Color = my_color(self(), NodesL),
                    case ColoreNodo of
                        Color ->
                            io:format("I due colori sono uguali, e ~p ha ricevuto il messaggio inviato da ~p~n",[get_alias(self()), get_alias(From)]),
                            From ! {response_color, self(),NodesL,AdjList},
                            timer:sleep(1000),
                            handle_mess();
                        _ ->
                            io:format("I due colori sono diversi, e ~p ha ricevuto il messaggio inviato da ~p~n",[get_alias(self()), get_alias(From)]),
                            From ! {response_color, self()},
                            timer:sleep(1000),
                            handle_mess()
                    end;
          
          % il padre riceve dal figlio
          {response_color,From,NodesL,AdjList} ->
                    io:format("Il nodo ~p ha ricevuto il messaggio da ~p~n", [get_alias(self()),get_alias(From)]),
                    Result = write_result_to_file(?NODELIST_FILE, NodesL, From, self()), % scrive su file, chiamando il gen server
                    io:format("Il processo ~p ha scritto sul file ~n", [get_alias(self())]), 
                    % non serve mettere una receive per aspettare di ricevere la risposta dal gen_server
                    % il nodo/processo che lo ha contattato, aspetta automaticamente la sua risposta
                    % la chiamata al gen_server è bloccante, infatti viene servito un processo alla volta
                    % {reply, Reply, Status} -> l'oggetto che viene ritornato da write_result_to_file è esattamente Reply
                    From ! {ack1,self(), lists:reverse(element(2, Result)), AdjList},  % Result = {ok, MergedFinal}, quindi prendo il secondo elemento
                    timer:sleep(1000),  
                    handle_mess();

          %% caso nodi diversi
          {response_color,From} ->
                    io:format("I due colori sono diversi. Non serve fare l'aggiornamento.~n"),
                    From ! {ack1,self()},
                    timer:sleep(1000),
                    handle_mess();
      
          %% caso nodi uguali
          {ack1, From, Updated2,AdjList} ->
                    io:format("~p ha ricevuto ack1 da ~p~n", [get_alias(self()),get_alias(From)]),
                    From ! {ack2, self(), Updated2,AdjList},
                    io:format("~p ha inviato ack2 a ~p~n", [get_alias(self()),get_alias(From)]),
                    timer:sleep(1000),  
                    handle_mess();
          %% caso nodi diversi
          {ack1, From} ->
                    io:format("~p ha ricevuto ack1 da ~p~n", [get_alias(self()),get_alias(From)]),
                    From ! {ack2, self()},
                    io:format("~p ha inviato ack2 a ~p~n", [get_alias(self()),get_alias(From)]),
                    timer:sleep(1000),  
                    handle_mess();
          %% caso nodi uguali
          {ack2, From, Updated2,AdjList} ->
                    io:format("~p ha ricevuto ack2 da nodo adiacente ~p~n", [get_alias(self()),get_alias(From)]),
                    communicate(Updated2,get_alias(From),AdjList),
                    io:format("~n~n~n"),
                    timer:sleep(1000),  
                    handle_mess();

          %% caso nodi diversi
          {ack2, From} ->
                    io:format("~p ha ricevuto ack2 da nodo adiacente ~p~n", [get_alias(self()),get_alias(From)]),
                    io:format("~n~n~n"),
                    timer:sleep(1000),  
                    handle_mess()
        end.

% funzione per ottenere l'Alias dal Pid
get_alias(Pid) -> 
    RegisteredNames = registered(), % prendo una lista di tutti i processi registrati
    case lists:filter(fun(Name) -> whereis(Name) == Pid end, RegisteredNames) of
        [Alias] -> Alias; % se il processo è presente nella lista, ritorno il suo alias
        [] -> {error, not_found};
        _ -> {error, multiple_matches}
    end.

% funzione per ottenere il colore target del nodo
my_target(_, []) -> undefined;
%% NodesL: {id nodo, colore, stato, coloretarget}
my_target(Pid, [{IdNodo, _, _, Targ} | T]) ->
  Pid2 = whereis(list_to_atom(integer_to_list(IdNodo))), % necessaria la conversione altrimenti stampa un intero
  case Pid of 
    Pid2 -> 
      Targ;
    _ -> 
      my_target(Pid, T)
  end.

% funzione per assegnare il campo target ai nodi
%% N : numero di nodi, NodesL : {id nodo, colore, stato, Target}
assign_target([],_) -> [];
assign_target([{Nodo, Colore, Stato,_} | Rest], N) ->
    ColorList = ["Red","Green","Blue","Yellow"],
    Randnum = rand:uniform(N) rem length(ColorList),
    if 
      Stato == "iniziale" ->
        if
          Randnum == 0 ->
            NewTarget = lists:nth(rand:uniform(length(ColorList)), ColorList);
          Randnum > 0 andalso (Randnum < length(ColorList)+1) ->
            NewTarget = lists:nth(Randnum, ColorList)
        end;
      Stato == "non iniziale" ->
        NewTarget = undefined 
    end,
  [{Nodo, Colore, Stato, NewTarget} | assign_target(Rest, N)].

%% Funzione per estrarre tuple con stato "iniziale"
%% estraggo dalla lista FinalL le tuple che contengono solo nodi initiator
%% quei nodi avraNFiglioo una lista ciascuno (funzione sopra da scrivere) che contiene lo spaNFiglioing tree composto da nodi con colori tutti uguali
extract(List) ->
  extract_initiator(List, []).
extract_initiator([], Acc) -> Acc;
extract_initiator([{_,_,_,D,_} = Tuple | T], Acc) ->
  if 
      D == "iniziale" ->
          extract_initiator(T, [Tuple | Acc]);
      true ->
          extract_initiator(T, Acc)
  end.

%% ritorna il colore attuale del nodo con pid Pid
my_color(_, []) -> 
    undefined; % Caso base: se la lista è vuota, restituisce un valore predefinito
my_color(Pid, [{IdNodo, C, _, _} | T]) ->
  Pid2 = whereis(list_to_atom(integer_to_list(IdNodo))),
    case Pid of
      Pid2 -> 
        C; % Se Pid corrisponde a IdNodo, restituisce C
      _ -> 
        my_color(Pid, T) % Altrimenti continua a cercare nella lista
  end.

%% aggiorna il colore di Pid  
update_color(NewColor,Pid, NodesL) ->
    lists:map(fun({IdNodo,Color,S,Tar}) -> 
      Pid2 = whereis(list_to_atom(integer_to_list(IdNodo))),  
      case Pid of     
        Pid2 -> {IdNodo,NewColor,S,Tar};
        _ -> {IdNodo,Color,S,Tar}
      end
    end, NodesL).


%% aggiorna il target di Pid
update_target(NewTarget,Pid, NodesL) ->
    lists:map(fun({IdNodo,Color,S,Tar}) -> 
      Pid2 = whereis(list_to_atom(integer_to_list(IdNodo))),  
      case Pid of     
        Pid2 -> {IdNodo,Color,S,NewTarget};
        _ -> {IdNodo,Color,S,Tar}
      end
    end, NodesL).

%% funzione per unire due liste di tuple
%% in questo caso N1 è sempre uguale a N2
merge_list([],[]) -> [];
merge_list([{N1,P}| T1 ], [{_,C,S,T} | T2]) ->
  [{N1,P,C,S,T} | merge_list(T1,T2)].

% funzione per stampare la lista e tutti i suoi campi per ogni nodo
print_tuple_list_with_foreach(TupleList) ->
    lists:foreach(fun({NumeroNodo, Colore, Status, Target}) ->
                      io:format("Nodo: ~p, Colore: ~p, Stato: ~p, Target : ~p~n", [NumeroNodo, Colore, Status, Target])
                  end, TupleList).

%% funzione per l'assegnazione random del colore ad ogni nodo
rnd_color([],_,_) -> [];
rnd_color([H | T], ColorList, N) ->
    Randnum = rand:uniform(N),
    RandCol = lists:nth(Randnum,ColorList),
    [{ H,RandCol} | rnd_color(T,ColorList,N)].

%% Funzione che assegna lo stato ai nodi
%% Nodes: lista nodi, Limit: numero nodi initiator scelti dall'utente
assign_status(Nodes, Limit) ->
    % Inizializza il generatore di numeri casuali
    % Usa una funzione ausiliaria con un contatore per mappare lo stato
    assign_status_aux(Nodes, Limit, 0).

% Funzione ausiliaria che gestisce l'assegnazione dello stato
assign_status_aux([], _, _) -> [];
assign_status_aux([{Nodo, Colore} | Rest], Limit, N) ->
    % Genera un numero casuale (1 o 2)
    RandNum = rand:uniform(2),
    
    % Assegna "iniziale" o "non iniziale"
    Stato = case N < Limit of
        true -> case RandNum of
                  1 -> "non iniziale";
                  2 -> "iniziale"
                end;
        false -> "non iniziale"
    end,

    % Aggiorna il contatore solo se Stato è "iniziale"
    NewN = case Stato of
        "iniziale" -> N + 1;
        _ -> N
    end,

    % Restituisce la tupla {Nodo, Colore, Stato} e procede ricorsivamente
    [{Nodo, Colore, Stato} | assign_status_aux(Rest, Limit, NewN)].

% funzione usata all'inizio del programma per scrivere su file di testo
write_to_file(FileName, Result) ->
      % apre il file in scrittura, e dunque lo svuota
      {ok, File} = file:open(FileName, [write]),
    
      % scrive ogni tupla della lista Result nel file
      lists:foreach(fun({Vertex, Color}) -> 
                        io:format(File, "~w ~s~n", [Vertex, Color]) 
                    end, Result),
      
      % chiude il file
      file:close(File).

% uccide tutti i processi alla fine dell'esecuzione, così è possibile fare un'altra run
% altrimenti rimangono registrati e bisogna chiudere e riaprire
kill_all_registered_processes(MergedProc) ->
    Registered = erlang:registered(),
    lists:foreach(fun({Name, Pid}) ->
        case lists:member(Name, Registered) of
            true -> 
                Pid = whereis(Name),
                io:format("Process Killed : ~p~n", [Name]),
                exit(Pid, kill);
            false -> 
                ok
        end      
      end, MergedProc).

%% Crea un nuovo grafo
-spec from_file(file:name()) -> graph().

from_file(File) ->
  from_file(File, fun read_vertices/2, fun read_edge/2).

-spec from_file(file:name(), function(), function()) -> graph().

% ritorna una tupla con degli elementi utili all'inizio della demo
from_file(File, ReadVertices, ReadEdge) ->
  {ok, IO} = file:open(File, [read]),
  %% N = numero di vertici :: non_neg_integer()
  %% M = numero di archi :: non_neg_integer()
  %% T = tipo di grafo :: directed | undirected
  %% W = peso degli archi :: tipo di peso (d | f) | unweighted
  {ok, [N, M, T, W]} = io:fread(IO, ">", "~d ~d ~a ~a"),
  G = #graph{type=T, graph=digraph:new(), weightType=W},
  
  ok = init_vertices(IO, G, N, ReadVertices),
  {ok,Lista} = init_edges(IO, G, M, ReadEdge, T, W,[]),
  {ReadVertices, Lista, IO,N}.

%% Funzione predefinita per la lettura dei vertici da file
-spec read_vertices(file:io_device(), integer()) -> [integer()].

read_vertices(_IO, N) -> lists:seq(1, N).

%% Funzione predefinita per leggere gli archi da file
%% U inizio dell'arco (vertice di partenza)
%% V fine dell'arco (vertice di arrivo)
%% W numero che definisce il peso dell'arco
-spec read_edge(file:io_device(), weighttype()) -> {vertex(), vertex(), weight()}.

read_edge(IO, WT) ->
  read_edge(IO, WT, fun erlang:list_to_integer/1).

read_edge(IO, unweighted, MapVertex) ->
  {ok, [V1, V2]} = io:fread(IO, ">", "~s ~s"),
  {MapVertex(V1), MapVertex(V2), 1};
read_edge(IO, WT, MapVertex) ->
  Format = "~s ~s ~n" ++ atom_to_list(WT),
  {ok, [V1, V2, W]} = io:fread(IO, ">", Format),
  {MapVertex(V1), MapVertex(V2), W}.

%% Inizializza i vertici nel grafo
-spec init_vertices(file:io_device(), graph(), integer(), function()) -> ok.

init_vertices(IO, Graph, N, ReadVertices) ->
  Vs = ReadVertices(IO, N),
  lists:foreach(fun(V) -> add_vertex(Graph, V) end, Vs).

%% Inizializza gli archi nel grafo
-spec init_edges(file:io_device(), graph(), integer(), function(), graphtype(), weighttype(),edge_list()) -> ok.

init_edges(_IO, _G, 0, _ReadEdge, _T, _WT,Lista) -> {ok,Lista};
init_edges(IO, G, M, ReadEdge, T, WT,Lista) ->
  {V1, V2, W} = ReadEdge(IO, WT),

  _ = add_edge(G, V1, V2, W),
  init_edges(IO, G, M-1, ReadEdge, T, WT,[ {V1, V2, W} | Lista]).

%% Elimina un grafo
-spec del_graph(graph()) -> 'true'.
  
del_graph(G) ->
  digraph:delete(G#graph.graph).
  
%% Ritorna il tipo di grafo
-spec graph_type(graph()) -> graphtype().

graph_type(G) ->
  G#graph.type.

%% Ritorna il tipo di pesi
-spec weight_type(graph()) -> weighttype().

weight_type(G) ->
  G#graph.weightType.

%% Aggiunge un nodo al grafo
-spec add_vertex(graph(), vertex()) -> vertex().

add_vertex(G, V) ->
  digraph:add_vertex(G#graph.graph, V).

%% Ritorna una lista di vertici del grafo
-spec vertices(graph()) -> [vertex()].

vertices(G) ->
  digraph:vertices(G#graph.graph).

%% Ritorna il numero di vertici/nodi nel grafo
-spec num_of_vertices(graph()) -> non_neg_integer().

num_of_vertices(G) ->
  digraph:no_vertices(G#graph.graph).

%% Aggiunge un arco ad un grafo non pesato
-spec add_edge(graph(), vertex(), vertex()) -> edge().

add_edge(G, From, To) ->
  add_edge(G, From, To, 1).

%% Aggiunge un arco ad un grafo pesato
-spec add_edge(graph(), vertex(), vertex(), weight()) -> edge() | {error, not_numeric_weight}.

add_edge(#graph{type=directed, graph=G}, From, To, W) when is_number(W) ->
  digraph:add_edge(G, {From, To}, From, To, W);
add_edge(#graph{type=undirected, graph=G}, From, To, W) when is_number(W) ->
  digraph:add_edge(G, {From, To}, From, To, W),
  digraph:add_edge(G, {To, From}, To, From, W);
add_edge(_G, _From, _To, _W) ->
  {error, not_numeric_weight}.

%% Elimina un arco dal grafo
-spec del_edge(graph(), edge()) -> 'true'.

del_edge(G, E) ->
  digraph:del_edge(G#graph.graph, E).

%% Ritorna la lista degli archi di un grafo
-spec edges(graph()) -> [edge()].

edges(G) ->
  Es = digraph:edges(G#graph.graph),
  case G#graph.type of
    directed -> Es;
    undirected -> remove_duplicate_edges(Es, [])
  end.

%% Rimuove gli archi dupplicati di un grafo indiretto
remove_duplicate_edges([], Acc) ->
  Acc;
remove_duplicate_edges([{From, To}=E|Es], Acc) ->
  remove_duplicate_edges(Es -- [{To, From}], [E|Acc]).

%% Ritorna il numero di archi nel grafo
-spec num_of_edges(graph()) -> non_neg_integer().

num_of_edges(G) ->
  M = digraph:no_edges(G#graph.graph),
  case G#graph.type of
    directed -> M;
    undirected -> M div 2
  end.

%% Ritorna il peso di un arco
-spec edge_weight(graph(), edge()) -> weight() | 'false'.

edge_weight(G, E) ->
  case digraph:edge(G#graph.graph, E) of
    {E, _V1, _V2, W} -> W;
    false -> false
  end.

%% Ritorna una lista degli archi del grafo insieme ai loro pesi
-spec edges_with_weights(graph()) -> [{edge(), weight()}].

edges_with_weights(G) ->
  Es = edges(G),
  lists:map(fun(E) -> {E, edge_weight(G, E)} end, Es).

%% Per grafi diretti. Ritorna una lista di vicini per ogni vertice
-spec out_neighbours(graph(), vertex()) -> [vertex()].
  
out_neighbours(G, V) ->
  digraph:out_neighbours(G#graph.graph, V).

%% Pretty print è stata utilizzata inizialmente per verificare
%% la lettura da file di testo del grafo
-spec pprint(graph()) -> ok.

pprint(G) ->
  Vs = digraph:vertices(G#graph.graph),
  F = 
    fun(V) ->
      Es = digraph:out_edges(G#graph.graph, V),
      Ns = lists:map(
        fun(E) -> 
          {E, _V1, V2, W} = digraph:edge(G#graph.graph, E),
          {V2, W}
        end, 
        Es),
      {V, Ns}
    end,
  N = lists:sort(fun erlang:'<'/2, lists:map(F, Vs)),
  io:format("[{From, [{To, Weight}]}]~n"),
  io:format("========================~n"),
  io:format("~p~n", [N]).

%% Esporta un grafo in un file.
%% DumpVertex prende in input un vertice e ritorna la sua rappresentazione testuale
-spec export(graph(), file:name(), fun((vertex()) -> string())) -> ok.

export(Graph, Filename, DumpVertex) ->
  {ok, IO} = file:open(Filename, [write]),
  export_graph_info(IO, Graph),
  export_vertices(IO, Graph, DumpVertex),
  export_edges(IO, Graph, DumpVertex),
  file:close(IO).

export_graph_info(IO, Graph) ->
  N = num_of_vertices(Graph),
  M = num_of_edges(Graph),
  GT = graph_type(Graph),
  WT = weight_type(Graph),
  io:fwrite(IO, "~w ~w ~w ~w~n", [N, M, GT, WT]).

export_vertices(IO, Graph, DumpVertex) ->
  Vs = vertices(Graph),
  Rs = [io_lib:format("~s", [DumpVertex(V)]) || V <- Vs],
  io:fwrite(IO, "~s~n", [string:join(Rs, " ")]).

export_edges(IO, Graph, DumpVertex) ->
  Es = edges(Graph),
  lists:foreach(
    fun({V1, V2}=E) ->
        W = edge_weight(Graph, E),
        io:fwrite(IO, "~s ~s ~w~n", [DumpVertex(V1), DumpVertex(V2), W])
      end,
    Es).

%% importa un grafo esportato.
%% MapVertex prende la rappresentazione testuale di un vertice e ritorna il vertice stesso
-spec import(file:name(), fun((string()) -> vertex())) -> graph().
import(File, MapVertex) ->
  ImportVs = fun(IO, _N) -> import_vertices(IO, MapVertex) end,
  ImportEs = fun(IO, WT) -> read_edge(IO, WT, MapVertex) end,
  from_file(File, ImportVs, ImportEs).

import_vertices(IO, MapVertex) ->
  Line = io:get_line(IO, ""),
  Strip1 = string:strip(Line, right, $\n),
  Strip2 = string:strip(Strip1, right, $\n),
  Vs = string:tokens(Strip2, " "),
  [MapVertex(V) || V <- Vs].

%% controlla se due grafi sono uguali
-spec equal(graph(), graph()) -> boolean().
equal(G1, G2) ->
  graph_type(G1) =:= graph_type(G2)
    andalso weight_type(G1) =:= weight_type(G2)
    andalso num_of_vertices(G1) =:= num_of_vertices(G2)
    andalso num_of_edges(G1) =:= num_of_edges(G2)
    andalso lists:sort(vertices(G1)) =:= lists:sort(vertices(G2))
    andalso equal_edges(G1, G2).

equal_edges(G1, G2) ->
  Es1 = lists:sort(edges(G1)),
  Es2 = lists:sort(edges(G2)),
  case Es1 =:= Es2 of
    false -> false;
    true ->
      [edge_weight(G1, E) || E <- Es1] =:= [edge_weight(G2, E) || E <- Es2]
  end.
