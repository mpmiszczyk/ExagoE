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
-module(process_driven).

-compile(export_all).

-export([init_state/3, process1/1, process2/1]).
-export([start/0]).

init_state(N, {SPid1, S}, SPid2) ->
    case S of
	true  ->
	    SPid1 ! {N, init_state, self(), ping, time_now()};
	false -> 
	    ok
    end,
    receive
	{N, _, _, pong, _} ->
	    SPid2 ! {N, init_state, self(), pang, time_now()},
	    init_state(N, {SPid1, false}, SPid2)
    end.

process1(N) ->
    receive
	{N, _, Pid, ping, _} ->
	    Pid ! {N, process1, self(), pong, time_now()}
    end.

process2(N) ->
    receive
	{N, _, _, pang, _} ->
	    ok
    end.

default_match_spec() ->
    [{'_', [], [{return_trace}, {set_seq_token, timestamp, true}]}].

enable_tracing() ->
    ttb:tracer(node(), [{handler, 
			 {fun (_Fd, Trace, _TraceInfo, _State) ->
				  case Trace of
				      {trace_ts, {_Pid1, {_M, P1, _A}, _N}, 'receive', 
				       {N, _P2, _Pid2, Message, _Ts2}, Ts1} ->
					  write_line("/Users/etate/ExagoE/src/examples/ttb_example/", N, P1, time_now(Ts1),
						     atom_to_list(Message));
				      end_of_trace ->
					  ok;
				      _ ->
					  io:format("~p\n", [Trace])
				  end
			  end, state_0}}]).

disable_tracing() ->
    ttb:stop([format]).

time_now(Now) ->
    {MegaS, S, MicroS} = Now,
    {calendar:now_to_universal_time({MegaS, S, MicroS}), MicroS}.
time_now() ->
    {MegaS, S, MicroS} = erlang:now(),
    {calendar:now_to_universal_time({MegaS, S, MicroS}), MicroS}.

format_timestamp(TimeNow) ->
    {{{YY,MM,DD},{H,M,S}},MicroS}  = TimeNow,
    [YYs,MMs,DDs,Hs,Ms,Ss,MicroSs] = 
	lists:map(fun integer_to_list/1, [YY,MM,DD,H,M,S,MicroS]),
    YYs ++ "-" ++ MMs ++ "-" ++ DDs ++ " " ++ Hs ++ ":" ++ Ms ++ ":" ++ Ss ++ ":" ++ MicroSs.

format_pid(Pid) ->
    pid_to_list(Pid).

write_line(BasePath, N, Filename, Timestamp, Transition) ->
    {ok, IoDevice} = file:open(BasePath ++ atom_to_list(Filename) ++ ".log", [append]),
    file:write(IoDevice, integer_to_list(N) ++ "," ++ format_timestamp(Timestamp) ++ "," ++ Transition ++ "\n"),
    file:close(IoDevice).

start() ->
    enable_tracing(),

    ttb:tpl(process_driven, process1, default_match_spec()),
    ttb:tpl(process_driven, process2, default_match_spec()),
    ttb:tpl(process_driven, process3, default_match_spec()),

    lists:map(fun (N) ->
		      ttb:p(Pid1 = spawn(process_driven, process1, [N]), ['receive',timestamp]),
		      ttb:p(Pid2 = spawn(process_driven, process2, [N]), ['receive',timestamp]),
		      ttb:p(spawn(process_driven, init_state, [N, {Pid1, true}, Pid2]), ['receive',timestamp])
	      end, [1,2,3,4,5]),
		      
    disable_tracing().
