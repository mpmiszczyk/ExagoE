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
-module(exa_sm_sup).

-export([execute_fsm_against_source/2, execute_fsm_against_fsm/2]).

%% @doc 
execute_fsm_against_source(FSMModel, EventSources) ->
    ReducedModel = exa_sm_gen:reduce_fsms([FSMModel], []),
    lists:map(fun (Source) ->
		      execute_fsms(ReducedModel, Source)
	      end, exa_es:source_transitions(EventSources)).

%% @doc
execute_fsm_against_fsm(FSMModel0, FSMModel1) ->
    ReducedModel = exa_sm_gen:reduce_fsms([FSMModel0], []),
    execute_fsms(ReducedModel, exa_sm:state_machine_transitions(FSMModel1, [])).

%% @doc
execute_fsms(FSMModels, Transitions) ->
    execute_fsms(FSMModels, Transitions, 0, []).

execute_fsms([], _Transitions, _N, Acc) ->
    lists:reverse(Acc);
execute_fsms([FSMModel|FSMModels], Transitions, N, Acc) ->
    FSMName = start_fsm_process(N, FSMModel),
    R = transition_until_exit(FSMName, Transitions, []),
    execute_fsms(FSMModels, Transitions, N+1, [R|Acc]).

%% @doc
start_fsm_process(N, {StateTransitions, Exports}) ->
    NString = lists:flatten(io_lib:format("~p", [N])),
    FSMName = list_to_atom("fsm_instance_" ++ NString),
    exa_sm_gen:generate_sm_module(FSMName, StateTransitions, Exports),
    gen_fsm:start_link({local, FSMName}, FSMName, [init_state], []),
    FSMName.

%% @doc
transition(FSMName, Transition) ->
    gen_fsm:sync_send_event(FSMName, Transition).
end_of_input(FSMName) ->
    gen_fsm:sync_send_event(FSMName, end_of_input).

%% @doc
transition_until_exit(FSMName, [], Result) ->
    %% no more transition data available
    {FSMName, end_of_input(FSMName), lists:reverse(Result)};
transition_until_exit(FSMName, [Transition|Transitions], Result) ->
    case (catch transition(FSMName, Transition)) of
	{'EXIT', Reason} ->
	    {process_terminated, Result, Reason};
	X when is_atom(X) -> {FSMName, X, lists:reverse(Result)};
	X when is_list(X) ->
	    transition_until_exit(FSMName, Transitions, X)
    end.
	    
