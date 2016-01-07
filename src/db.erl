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
write_element(Key, NewElement, [{Key, _}| T], NewDbRef) ->
  [{Key, NewElement}| NewDbRef] ++ T;
write_element(Key, Element, [H| T], NewDbRef) ->
  write_element(Key, Element, T, [H| NewDbRef]);
write_element(Key, Element, [], NewDbRef) ->
  [{Key, Element}| NewDbRef].


delete_element(Key, [{Key, _}| T], NewDbRef) ->
  NewDbRef ++ T;
delete_element(Key, [H| T], NewDbRef) ->
  delete_element(Key, T, [H | NewDbRef]);
delete_element(_, [], NewDbRef) ->
  NewDbRef.

read_element(Key, [{Key, Element}| _]) ->
  {ok, Element};
read_element(Key, [_| T]) ->
  read_element(Key, T);
read_element(_, []) ->
  {error, instance}.


match_element(Element, [{Key, Element}| T], AccList) ->
  match_element(Element, T, [Key| AccList]);
match_element(Element, [_| T], AccList) ->
  match_element(Element, T, AccList);
match_element(_, [], AccList) ->
  {ok, AccList}.