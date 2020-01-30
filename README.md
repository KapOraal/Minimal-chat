Simple Chat room Erlang
=============
Documentation Erlang
https://learnyousomeerlang.com/buckets-of-sockets#tcp-sockets

Running
-----------
### Server
```
erl
c(echo_server).
echo_server:start_server(4000).
```

### Client
```
telnet 127.0.0.1 6667
```
