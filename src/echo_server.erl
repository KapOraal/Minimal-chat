%%%-------------------------------------------------------------------
%%% @author Djibril Dieng <dieng.a.djibril@gmail.com>
%%% @copyright (C) 2015, Ali Yakout
%%% @doc : https://learnyousomeerlang.com/content
%%% @doc : https://learnyousomeerlang.com/buckets-of-sockets#tcp-sockets
%%%-------------------------------------------------------------------
-module(echo_server).
-author("Djibril").
-compile(export_all).

start_server(Port) ->
    Pid = spawn_link(fun() ->
        {ok, Listen} = gen_tcp:listen(Port, [binary, {active, false}]),
        spawn(fun() -> acceptor(Listen) end),
        timer:sleep(infinity)
    end),
    {ok, Pid}.

acceptor(ListenSocket) ->
    {ok, Socket} = gen_tcp:accept(ListenSocket),
    spawn(fun() -> acceptor(ListenSocket) end),
    handle(Socket).

%% Echoing back whatever was obtained
handle(Socket) ->
    inet:setopts(Socket, [{active, once}]),
    receive
        {tcp, Socket, <<"quit", _/binary>>} ->
            gen_tcp:close(Socket);
        {tcp, Socket, Msg} ->
            gen_tcp:send(Socket, Msg),
            handle(Socket)
    end.