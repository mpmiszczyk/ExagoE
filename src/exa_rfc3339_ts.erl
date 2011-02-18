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
-module(exa_rfc3339_ts).

-export([parse/1]).

parse(TimestampString) ->
    parse_timestamp_rfc3339(TimestampString, 0, [], []).

%% @doc rfc3339 parser
parse_timestamp_rfc3339(Timestamp, N, A, R) ->
    parse_timestamp_rfc3339(Timestamp, N, A, R, date_fullyear).

%% date_fullyear date_month date_mday time_hour time_minute time_second time_secfrac 
%% time_numoffset_hour time_numoffset_minute
parse_timestamp_rfc3339([$-|Cs], 4, A, R, date_fullyear) ->
    parse_timestamp_rfc3339(Cs, 0, [], [{date_fullyear, lists:reverse(A)}|R], date_month);
parse_timestamp_rfc3339([C|Cs], N, A, R, date_fullyear) when N < 4  ->
    parse_timestamp_rfc3339(Cs, N+1, [C|A], R, date_fullyear);
parse_timestamp_rfc3339([_C|_Cs], N, _A, _R, date_fullyear) when N >= 4 ->
    {parse_error, "date_fullyear contains too many digits"};

parse_timestamp_rfc3339([$-|Cs], 2, A, R, date_month) ->
    parse_timestamp_rfc3339(Cs, 0, [], [{date_month, lists:reverse(A)}|R], date_mday);
parse_timestamp_rfc3339([C|Cs], N, A, R, date_month) when N < 2  ->
    parse_timestamp_rfc3339(Cs, N+1, [C|A], R, date_month);
parse_timestamp_rfc3339([_C|_Cs], N, _A, _R, date_month) when N >= 2 ->
    {parse_error, "date_month contains too many digits"};

parse_timestamp_rfc3339([$T|Cs], 2, A, R, date_mday) ->
    parse_timestamp_rfc3339(Cs, 0, [], [{date_mday, lists:reverse(A)}|R], time_hour);
parse_timestamp_rfc3339([C|Cs], N, A, R, date_mday) when N < 2  ->
    parse_timestamp_rfc3339(Cs, N+1, [C|A], R, date_mday);
parse_timestamp_rfc3339([_C|_Cs], N, _A, _R, date_mday) when N >= 2 ->
    {parse_error, "date_mday contains too many digits"};

parse_timestamp_rfc3339([$:|Cs], 2, A, R, time_hour) ->
    parse_timestamp_rfc3339(Cs, 0, [], [{time_hour, lists:reverse(A)}|R], time_minute);
parse_timestamp_rfc3339([C|Cs], N, A, R, time_hour) when N < 2  ->
    parse_timestamp_rfc3339(Cs, N+1, [C|A], R, time_hour);
parse_timestamp_rfc3339([_C|_Cs], N, _A, _R, time_hour) when N >= 2 ->
    {parse_error, "time_hour contains too many digits"};

parse_timestamp_rfc3339([$:|Cs], 2, A, R, time_minute) ->
    parse_timestamp_rfc3339(Cs, 0, [], [{time_minute, lists:reverse(A)}|R], time_second);
parse_timestamp_rfc3339([C|Cs], N, A, R, time_minute) when N < 2  ->
    parse_timestamp_rfc3339(Cs, N+1, [C|A], R, time_minute);
parse_timestamp_rfc3339([_C|_Cs], N, _A, _R, time_minute) when N >= 2 ->
    {parse_error, "time_minute contains too many digits"};

parse_timestamp_rfc3339([$.|Cs], 2, A, R, time_second) ->
    parse_timestamp_rfc3339(Cs, 0, [], [{time_second, lists:reverse(A)}|R], time_secfrac);
parse_timestamp_rfc3339([$+|Cs], 2, A, R, time_second) ->
    parse_timestamp_rfc3339(Cs, 0, [], [{time_second, lists:reverse(A)}|R], time_numoffset_hour);
parse_timestamp_rfc3339([$-|Cs], 2, A, R, time_second) ->
    parse_timestamp_rfc3339(Cs, 0, [], [{time_second, lists:reverse(A)}|R], time_numoffset_hour);
parse_timestamp_rfc3339([$Z|_Cs], 2, A, R, time_second) ->
    lists:reverse([{time_second, lists:reverse(A)}|R]);
parse_timestamp_rfc3339([C|Cs], N, A, R, time_second)     when N < 2  ->
    parse_timestamp_rfc3339(Cs, N+1, [C|A], R, time_second);
parse_timestamp_rfc3339([_C|_Cs], N, _A, _R, time_second) when N >= 2 ->
    {parse_error, "time_second contains too many digits"};

parse_timestamp_rfc3339([$+|Cs], _N, A, R, time_secfrac) ->
    parse_timestamp_rfc3339(Cs, 0, [], [{time_secfrac, lists:reverse(A)}|R], time_numoffset_hour);
parse_timestamp_rfc3339([$-|Cs], _N, A, R, time_secfrac) ->
    parse_timestamp_rfc3339(Cs, 0, [], [{time_secfrac, lists:reverse(A)}|R], time_numoffset_hour);
parse_timestamp_rfc3339([$Z|_Cs], _N, A, R, time_secfrac) ->
    lists:reverse([{time_secfrac, lists:reverse(A)}|R]);
parse_timestamp_rfc3339([C|Cs], N, A, R, time_secfrac) ->
    parse_timestamp_rfc3339(Cs, N+1, [C|A], R, time_secfrac);

parse_timestamp_rfc3339([$:|Cs], 2, A, R, time_numoffset_hour) ->
    parse_timestamp_rfc3339(Cs, 0, [], [{time_numoffset_hour, lists:reverse(A)}|R], time_numoffset_minute);
parse_timestamp_rfc3339([C|Cs], N, A, R, time_numoffset_hour) when N < 2  ->
    parse_timestamp_rfc3339(Cs, N+1, [C|A], R, time_numoffset_hour);
parse_timestamp_rfc3339([_C|_Cs], N, _A, _R, time_numoffset_hour) when N >= 2 ->
    {parse_error, "time_numoffset_hour contains too many digits"};

parse_timestamp_rfc3339([], 2, A, R, time_numoffset_minute) ->
    lists:reverse([{time_numoffset_minute, lists:reverse(A)}|R]);
parse_timestamp_rfc3339([$Z|_Cs], 2, A, R, time_numoffset_minute) ->
    lists:reverse([{time_numoffset_minute, lists:reverse(A)}|R]);
parse_timestamp_rfc3339([C|Cs], N, A, R, time_numoffset_minute) when N < 2  ->
    parse_timestamp_rfc3339(Cs, N+1, [C|A], R, time_numoffset_minute);
parse_timestamp_rfc3339([_C|_Cs], N, _A, _R, time_numoffset_minute) when N >= 2 ->
    {parse_error, "time_numoffset_minute contains too many digits"};

parse_timestamp_rfc3339([$:|Cs], 2, A, R, time_offset_hour) ->
    parse_timestamp_rfc3339(Cs, 0, [], [{time_offset_hour, lists:reverse(A)}|R], time_offset_minute);
parse_timestamp_rfc3339([C|Cs], N, A, R, time_offset_hour) when N < 2 ->
    parse_timestamp_rfc3339(Cs, N+1, [C|A], R, time_offset_hour);
parse_timestamp_rfc3339([_C|_Cs], N, _A, _R, time_offset_hour) when N >= 2 ->
    {parse_error, "time_offset_hour contains too many digits"};

parse_timestamp_rfc3339([], 2, _A, R, time_offset_minute) ->
    lists:reverse(R);
parse_timestamp_rfc3339([C|Cs], N, A, R, time_offset_minute) when N < 2  ->
    parse_timestamp_rfc3339(Cs, N+1, [C|A], R, time_offset_minute);
parse_timestamp_rfc3339([_C|_Cs], N, _A, _R, time_offset_minute) when N >= 2 ->
    {parse_error, "time_offset_minute contains too many digits"}.

