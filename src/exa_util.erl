%%%-------------------------------------------------------------------
%%% @author Edward Tate <edward.tate@erlang-solutions.com>
%%% @copyright (C) 2010, Erlang Solutions Ltd.
%%% @doc
%%%
%%% @end
%%% Created : 15 Oct 2010 by Edward Tate <edward.tate@erlang-solutions.com>
%%%-------------------------------------------------------------------
%%% Copyright (c) 2009,2010 Erlang Solutions formerly Erlang Training & Consulting
%%% All rights reserved.
%%%
%%% Redistribution and use in source and binary forms, with or without
%%% modification, are permitted provided that the following conditions are met:
%%% * Redistributions of source code must retain the above copyright
%%%   notice, this list of conditions and the following disclaimer. 
%%% * Redistributions in binary form must reproduce the above copyright
%%%   notice, this list of conditions and the following disclaimer in the
%%%   documentation and/or other materials provided with the distribution.
%%% * Neither the name of the Erlang Solutions nor the names of its
%%%   contributors may be used to endorse or promote products
%%%   derived from this software without specific prior written permission.
%%%
%%% THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS "AS IS"
%%% AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE
%%% IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE
%%% ARE DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT HOLDER OR CONTRIBUTORS
%%% BE  LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR
%%% CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF
%%% SUBSTITUTE GOODS OR SERVICES; LOSS OF USE, DATA, OR PROFITS; OR
%%% BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON ANY THEORY OF LIABILITY,
%%% WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT (INCLUDING NEGLIGENCE OR
%%% OTHERWISE) ARISING IN ANY WAY OUT OF THE USE OF THIS SOFTWARE, EVEN IF
%%% ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.
%%%-------------------------------------------------------------------
-module(exa_util).

-export([with_file/3, drop_last/1, collect_file_by_line/2]).

%% @doc utility to cleanly collect files
-spec(with_file (Path::string(), Collector::tuple(), Modifier::tuple())
      -> CollectedResults::list()).	     
with_file(Path, {Module1, Collect}, {Module2, Function}) ->
    case file:open(Path, [read]) of
	{ok, IoDevice} ->
	    Result = Module1:Collect(IoDevice, {Module2, Function}),
	    file:close(IoDevice),
	    Result;
	Error          ->
	    {file_error, Error}
    end.

%% @doc Drop the last element in a list
-spec(drop_last (List::list())
      -> ListButLast::list()).
drop_last(L) ->
    drop_last(L, []).

drop_last([[]|[]], R) ->
    lists:reverse(R);
drop_last([E|[]], R) ->
    lists:reverse([E|R]);
drop_last([E|Elts], R) ->
    drop_last(Elts, [E|R]).

%% @doc Collect a file by line
collect_file_by_line(IoDevice, {Module, Function}) ->
    collect_file_by_line(IoDevice, {Module, Function}, start, []).

collect_file_by_line(IoDevice, {Module, Function}, start, Result) ->
    collect_file_by_line(IoDevice, {Module, Function}, file:read_line(IoDevice), Result);
collect_file_by_line(_IoDevice, {_Module, _Function}, eof, Result)   -> 
    lists:reverse(Result);
collect_file_by_line(IoDevice, {Module, Function}, {ok, Line}, Result) ->
    collect_file_by_line(IoDevice, {Module, Function}, file:read_line(IoDevice), [drop_last(Module:Function(Line))|Result]).

