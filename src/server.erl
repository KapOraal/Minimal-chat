%%% @author Djibril Dieng <dieng.a.djibril@gmail.com>
%%% @doc : https://learnyousomeerlang.com/content
%%% @doc : https://learnyousomeerlang.com/buckets-of-sockets#tcp-sockets
%%%-------------------------------------------------------------------
-module(server).
-record(state, {listen_socket,
                clients = [],
                message_history = []}).

-export([start/1, stop/0, init/1, worker/2]).


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
      client:client_init(Server, CSocket, Ref);
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