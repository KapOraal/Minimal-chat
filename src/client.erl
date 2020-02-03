%%% @author Djibril Dieng <dieng.a.djibril@gmail.com>
%%% @doc : https://learnyousomeerlang.com/content
%%% @doc : https://learnyousomeerlang.com/buckets-of-sockets#tcp-sockets
%%%-------------------------------------------------------------------
-module(client).

%% API
-export([client_init/3, client_listener/3]).

%% initialize client with username
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
      ok = gen_tcp:send(Socket, "\r" ++ Message);

    {'DOWN', _Ref, process, _Pid, _Reason} ->
      ok = gen_tcp:send(Socket, "Chat serveur est hors service, au revoir!\n"),
      gen_tcp:close(Socket),
      exit(normal)
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

%% check username exists or not
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