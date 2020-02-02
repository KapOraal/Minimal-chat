%%% @author Djibril Dieng <dieng.a.djibril@gmail.com>
%%% @doc : https://learnyousomeerlang.com/content
%%% @doc : https://learnyousomeerlang.com/buckets-of-sockets#tcp-sockets
%%%-------------------------------------------------------------------
-module(server).
-record(state, {listen_socket,
                clients = [],
                message_history = []}).

-export([start/1, stop/0, init/1, worker/2, client_init/3, client_listener/3]).


start(Port) ->
  io:format("Starting chat server...~n"),
  register(?MODULE, Pid = spawn(?MODULE, init, [Port])),
  io:format("Chat server started with pid ~p~n", [Pid]).


stop() ->
  io:format("Server is shutting down...~n"),
  exit(whereis(?MODULE), normal).


init(Port) ->
  {ok, LSocket} = gen_tcp:listen(Port, [{reuseaddr, true}, {packet, 0}, {active, false}]),
  spawn(?MODULE, worker, [?MODULE, LSocket]),
  loop(#state{listen_socket = LSocket, clients = orddict:new(), message_history = []}).


worker(Server, LSocket) ->
  case gen_tcp:accept(LSocket) of
    {ok, CSocket} ->
      io:format("Incoming user from ~p~n", [CSocket]),
      Server ! new_worker,
      Ref = monitor(process, whereis(Server)),
      client_init(Server, CSocket, Ref);
    _ ->
      error
  end,
  io:format("~p worker died~n", [self()]).


loop(S) ->
  receive
    {ready, From, _Ref, Username} ->
      io:format("~p has joined the chat room~n", [Username]),
      JoinMessage = Username ++ " a rejoint le chat!\n",
      broadcast_message(JoinMessage, From, S#state.clients),

      UpdatedClients = orddict:store(Username, From, S#state.clients),
      loop(S#state{clients = UpdatedClients});

    new_worker ->
      Pid = spawn(?MODULE, worker, [?MODULE, S#state.listen_socket]),
      io:format("Starting new worker ~p for listening to new connection~n", [Pid]),
      loop(S);

    {check_username, From, Username} ->
      case orddict:find(Username, S#state.clients) of
        {ok, _} ->
          From ! existed;

        error ->
          From ! ok
      end,
      loop(S);

    {broadcast, From, _Ref, Message} ->
      io:format("Starting new worker  for listening to new connection~n"),
      broadcast_message(Message, From, S#state.clients),
      UpdatedHistory = [Message | S#state.message_history],
      loop(S#state{message_history = UpdatedHistory});

    _ ->
      oops,
      loop(S)
  end.


%% Client sends message to Clients (except himself)
broadcast_message(Message, Client, Clients) ->
  BroadList = lists:filter(fun({_Username, UserPid}) -> UserPid =/= Client
                           end, Clients),
  lists:map(fun({_Username, UserPid}) -> UserPid ! {receive_message, Message} end, BroadList).

%% initialize client
client_init(Server, Socket, Ref) ->
  ok = gen_tcp:send(Socket, "Bonjour\n"),
  ok = gen_tcp:send(Socket, "Choisissez votre nom d'utilisateur: "),
  Username = get_username(Server, Socket),

  ok = gen_tcp:send(Socket, Username ++ ": "),
  Server ! {ready, self(), Ref, Username},
  spawn_link(?MODULE, client_listener, [self(), Socket, Ref]),
  client_loop(Server, Username, Socket).

%% receive message from the listener, forward it to the server
%% send message received from server back to client via Socket
client_loop(Server, Username, Socket) ->
  receive
    % message from listener
    {send_message, Ref, Message} ->
      Server ! {broadcast, self(), Ref, Username ++ ": " ++ Message};

    {tcp_closed, Ref, Reason} ->
      Server ! {disconnected, self(), Ref, Username, Reason},
      gen_tcp:close(Socket),
      erlang:exit(normal);

    % message from server
    {receive_message, Message} ->
      ok = gen_tcp:send(Socket, "\r" ++ Message)

  end,

  ok = gen_tcp:send(Socket, Username ++ ": "),
  client_loop(Server, Username, Socket).


%% waiting for input for the client, then send the input message to the client himself
client_listener(Client, Socket, Ref) ->
  case gen_tcp:recv(Socket, 0) of
    {ok, Pack} ->
      Client ! {send_message, Ref, Pack},
      client_listener(Client, Socket, Ref);
    {error, Reason} ->
      Client ! {tcp_closed, Ref, Reason}
  end.

%% get username and check if already exist
get_username(Server, Socket) ->
  case gen_tcp:recv(Socket, 0) of
    {ok, Pack} ->
      {Username, _} = lists:splitwith(fun(X) -> X =/= $\r end, Pack),
      Server ! {check_username, self(), Username},
      receive
        existed ->
          ok = gen_tcp:send(Socket, "Nom d'utilisateur dÃ©jÃ  pris, chosissez un autre: "),
          get_username(Server, Socket);

        ok ->
          Username
      end;

    {error, Reason} ->
      exit(Reason)
  end.