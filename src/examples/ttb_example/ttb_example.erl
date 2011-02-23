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
-module(ttb_example).

-export([generate_model/0]).
-export([event_source/0]).

timestamp_format() ->
    [date_fullyear, date_month, date_mday,
     time_hour, time_minute, time_second,
     time_secfrac].

field_format() ->
    [exa_field:instance_key("pid", string),
     exa_field:transaction_type("transaction_type", atom),
     exa_field:timestamp("timestamp", partial, timestamp_format()),
     exa_field:transition("transition", atom),
     exa_field:transaction_key("transaction_key", integer)].

init_state_send() ->    
    {"init_state", [{csv, absolute, "./log_files/init_state_send.log"}], field_format()}.
init_state_receive() -> 
    {"init_state", [{csv, absolute, "./log_files/init_state_receive.log"}], field_format()}.
process1_send() ->
    {"process1", [{csv, absolute, "./log_files/process1_send.log"}], field_format()}.
process1_receive() ->
    {"process1", [{csv, absolute, "./log_files/process1_receive.log"}], field_format()}.
process2_receive() ->
    {"process2", [{csv, absolute, "./log_files/process2_receive.log"}], field_format()}.
process3_send() ->
    {"process3", [{csv, absolute, "./log_files/process3_send.log"}], field_format()}.
process3_receive() ->
    {"process3", [{csv, absolute, "./log_files/process3_receive.log"}], field_format()}.

event_source() ->
    {"event_source", exa_es:collect(
			 [init_state_send(), init_state_receive(), 
			  process1_send(), process1_receive(),
			  process2_receive(),
			  process3_send(), process3_receive()], 
		       append, source_state)}.

generate_model() ->
    exa_sm:generate_visualizations([exa_sm:generate_state_machine(event_source(), [])], 0).
