%%%-------------------------------------------------------------------
%%% @author Wojtek
%%% @copyright (C) 2015, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 10. Dec 2015 22:41
%%%-------------------------------------------------------------------
-module(db_server).
-author("Wojtek").

%% API
-export([start/0, stop/0, lock/0, unlock/0, write/2, delete/1, read/1, match/1, db_loop/2]).
-import(db, [new/0, destroy/0, write/3, delete/2, read/2, match/2]).

start() ->
  DbRef = db:new(),
  register(server, spawn(db_server, db_loop, [DbRef, server])).

stop() ->
  server ! {self(), stop},
  receive
    Response ->
      Response
  after 5000 ->
    {error, timeout}
  end.

lock() ->
  server ! {self(), lock},
  receive
    Response ->
      Response
  after 5000 ->
    {error, timeout}
  end.

unlock() ->
  server ! {self(), unlock},
  receive
    Response ->
      Response
  after 5000 ->
    {error, timeout}
  end.

write(Key, Element) ->
  server ! {self(), {write, Key, Element}},
  receive
    Response ->
      Response
  after 5000 ->
    {error, timeout}
  end.

delete(Key) ->
  server ! {self(), {delete, Key}},
  receive
    Response ->
      Response
  after 5000 ->
    {error, timeout}
  end.

read(Key) ->
  server ! {self(), {read, Key}},
  receive
    Response ->
      Response
  after 5000 ->
    {error, timeout}
  end.

match(Element) ->
  server ! {self(), {match, Element}},
  receive
    Response ->
      Response
  after 5000 ->
    {error, timeout}
  end.

db_loop(DbRef, server) ->
  receive
    {Client, lock} ->
      Client ! locked,
      db_loop(DbRef, Client);
    {Client, stop} ->
      unregister(server),
      Client ! stopped;
    {Client, _} ->
      Client ! {error, unlock_command_error},
      db_loop(DbRef, server)
  end;
db_loop(DbRef, LockingClient) ->
  receive
    {LockingClient, {write, K, E}} ->
      NewDbRef = db:write(K, E, DbRef),
      LockingClient ! written,
      db_loop(NewDbRef, LockingClient);
    {LockingClient, {delete, K}} ->
      NewDbRef = db:delete(K, DbRef),
      LockingClient ! deleted,
      db_loop(NewDbRef, LockingClient);
    {LockingClient, {read, K}} ->
      LockingClient ! db:read(K, DbRef),
      db_loop(DbRef, LockingClient);
    {LockingClient, {match, E}} ->
      LockingClient ! db:match(E, DbRef),
      db_loop(DbRef, LockingClient);
    {LockingClient, unlock} ->
      LockingClient ! unlocked,
      db_loop(DbRef, server);
    {LockingClient, stop} ->
      LockingClient ! stopped
  after 5000 ->
    db_loop(DbRef, server)
  end.
