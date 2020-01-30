%%% @author Djibril Dieng <dieng.a.djibril@gmail.com>
%%% @doc : https://learnyousomeerlang.com/content
%%% @doc : https://learnyousomeerlang.com/buckets-of-sockets#tcp-sockets
%%%-------------------------------------------------------------------
-module(server).
-record(state, {listen_socket,
                clients = [],
                message_history = []}).

-export([start/1, init/1, worker/2]).


start(Port) ->
  io:format("Starting chat server...~n"),
  register(?MODULE, Pid = spawn(?MODULE, init, [Port])),
  io:format("Chat server started with pid ~p~n", [Pid]).


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
      init(Server, CSocket, Ref);
    _ ->
      error
  end,
  io:format("~p worker died~n", [self()]).


loop(S) ->
  receive
    new_worker ->
      Pid = spawn(?MODULE, worker, [?MODULE, S#state.listen_socket]),
      io:format("Starting new worker ~p for listening to new connection~n", [Pid]),
      loop(S);

    _ ->
      oops,
      loop(S)
  end.

%% initialize client
init(Server, Socket, Ref) ->
	io:format("Bonjour sur le serveur de chat : ~p avec le pid ~p~n", [Server, Ref]),
  ok = gen_tcp:send(Socket, "Bonjour\n"),
  ok = gen_tcp:send(Socket, "Choisissez votre nom d'utilisateur: "),
  timer:sleep(infinity).
