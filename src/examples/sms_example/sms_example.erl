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
-module(sms_example).

-export([generate_model/0]).

timestamp_format() ->
    [date_fullyear, date_month, date_day,
     time_hour, time_minute, time_second,
     time_secfrac].

ack_sms_row_format() ->
    [exa_field:transition("transition", string),
     exa_field:timestamp("timestamp", partial, timestamp_format()),
     exa_field:foreign_key("instance_key", integer, "req_sms_es", "instance_key", ["instance_key"]),
     exa_field:annotation("unknownAckSMS", string)].

req_ack_row_format() ->
    [exa_field:transition("transition", string),
     exa_field:timestamp("timestamp", partial, timestamp_format()),
     exa_field:instance_key("instance_key", integer),
     exa_field:annotation("message", string)].

req_err_row_format() ->
    [exa_field:transition("transition", string),
     exa_field:timestamp("timestamp", partial, timestamp_format()),
     exa_field:instance_key("instance_key", integer),
     exa_field:annotation("message", string)].

req_row_format() ->
    [exa_field:transition("transition", string),
     exa_field:timestamp("timestamp", partial, timestamp_format()),
     exa_field:instance_key("instance_key", integer)].

req_sms_row_format() ->
    [exa_field:transition("transition", string),
     exa_field:timestamp("timestamp", partial, timestamp_format()),
     exa_field:instance_key("instance_key", integer),
     exa_field:annotation("ack_sms_key", integer),
     exa_field:annotation("unknown_req_sms", integer)].

ack_sms() -> {"ack_sms_es", [{csv, absolute, "./log_files/etc_ex_AckSMS.log"}], ack_sms_row_format()}.
req_ack() -> {"req_ack_es", [{csv, absolute, "./log_files/etc_ex_ReqAck.log"}], req_ack_row_format()}.
req_err() -> {"req_err_es", [{csv, absolute, "./log_files/etc_ex_ReqErr.log"}], req_err_row_format()}.
req()     -> {"req_es", [{csv, absolute, "./log_files/etc_ex_Req.log"}], req_row_format()}.
req_sms() -> {"req_sms_es", [{csv, absolute, "./log_files/etc_ex_ReqSMS.log"}], req_sms_row_format()}.

combined() ->
    {"combined", exa_es:collect([ack_sms(), req_ack(), req_err(), req(), req_sms()], append, source_state)}.

generate_model() ->
    exa_sm:generate_visualizations([exa_sm:generate_state_machine(combined(), [])], 0).

