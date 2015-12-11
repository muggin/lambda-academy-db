%%%-------------------------------------------------------------------
%%% @author Wojtek
%%% @copyright (C) 2015, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 10. Dec 2015 21:39
%%%-------------------------------------------------------------------
-module(db).
-author("Wojtek").

%% API
-export([new/0, destroy/0, write/3, delete/2, read/2, match/2]).
-compile(export_all).

new() ->
  [].

destroy() ->
  ok.

write(Key, Element, DbRef) ->
  write_element(Key, Element, DbRef, []).

delete(Key, DbRef) ->
  delete_element(Key, DbRef, []).

read(Key, DbRef) ->
  read_element(Key, DbRef).

match(Element, DbRef) ->
  match_element(Element, DbRef, []).

%% Part 1 methods
write_element(Key, Element, [], NewDbRef) ->
  [{Key, Element}|NewDbRef];
write_element(Key, Element, [{HKey, HElement}|T], NewDbRef) ->
  case Key == HKey of
    true ->
      write_element(Key, Element, T, NewDbRef);
    false ->
      write_element(Key, Element, T, [{HKey, HElement}|NewDbRef])
  end.

delete_element(_, [], NewDbRef) ->
  NewDbRef;
delete_element(Key, [{HKey, HElement}|T], NewDbRef) ->
  case Key == HKey of
    true ->
      delete_element(Key, T, NewDbRef);
    false ->
      delete_element(Key, T, [{HKey, HElement}|NewDbRef])
  end.

read_element(_, []) ->
  {error, instance};
read_element(Key, [{HKey, HElement}|T]) ->
  case Key == HKey of
    true ->
      {ok, HElement};
    false ->
      read_element(Key, T)
  end.

match_element(_, [], AccList) ->
  AccList;
match_element(Element, [{HKey, HElement}| T], AccList) ->
  case Element == HElement of
    true ->
      match_element(Element, T, [HKey|AccList]);
    false ->
      match_element(Element, T, AccList)
  end.

