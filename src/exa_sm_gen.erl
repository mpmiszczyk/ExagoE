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
-module(exa_sm_gen).
-behaviour(gen_fsm).

-compile(export_all).

%% compatize fsm definition
lookup_state(N, [], _) -> {error, no_state_found};
lookup_state(N, [{N, State, StateType}|States], annotate) -> 
    {State, StateType};
lookup_state(N, [{N, State, StateType}|States], raw) -> 
    State;
lookup_state(N, [{NFrom, State, StateType}|States], LookupType) ->
    lookup_state(N, States, LookupType);
lookup_state(N, [{N, State}|States], _) -> 
    State;
lookup_state(N, [{NFrom, State}|States], LookupType) ->
    lookup_state(N, States, LookupType).

compatize_fsm({States, Transitions}, CompatibleFSM) ->
    compatize_fsm({States, Transitions, {autogen_possible, true}}, CompatibleFSM);

compatize_fsm({States, [], _AutoGenFlag}, CompatibleFSM) ->
    ExportedStates = extract_exported_states(States, []),
    {lists:reverse(CompatibleFSM), ExportedStates};
compatize_fsm({States, [{NFrom, Transition, NTo}|Transitions], AutoGenFlag}, CompatibleFSM) ->
    compatize_fsm({States, Transitions, AutoGenFlag}, 
		  [{lookup_state(NFrom, States, annotate),
		    Transition,
		    lookup_state(NTo, States, raw)}|CompatibleFSM]).

compatize_fsms(FSMs) ->
    lists:map(fun (FSM) -> compatize_fsm(FSM, []) end, FSMs).

%% reduce fsm
missing_state(State, []) -> 
    true;
missing_state(State, [{State, _}|StateTransitions]) ->
    false;
missing_state(State0, [{State1, _}|StateTransitions]) ->
    missing_state(State0, StateTransitions).

add_missing_states(StateTransitions, []) ->
    lists:reverse(StateTransitions);
add_missing_states(StateTransitions, [State|StateList]) ->
    case missing_state(State, StateTransitions) of
	true  -> add_missing_states([{State, []}|StateTransitions], StateList);
	false -> add_missing_states(StateTransitions, StateList)
    end.

reduce_states([], {S, R}) ->
    {S, lists:reverse(R)};
reduce_states([{State, Transition, NextState}|STs], {S, R}) ->
    reduce_states(STs, {S, [{Transition, NextState}|R]}).

reduce_fsm([], StateTransitions, Result) ->
    ordsets:from_list(lists:reverse(Result));
reduce_fsm([{State, Transition, NextState}|STs], StateTransitions, Result) ->
    reduce_fsm(STs, StateTransitions, 
	       [reduce_states(lists:filter(fun ({S,T,N}) -> State == S end, StateTransitions), 
			      {State, []})|Result]).

reduce_fsm({StateTransitions, Exports}) ->
    {reduce_fsm(StateTransitions, StateTransitions, []), Exports}.

reduce_fsms([], Result) ->
    lists:reverse(Result);
reduce_fsms([FSMModel|FSMModels], Result) ->
    {StateTransitions, Exports} = reduce_fsm(compatize_fsm(FSMModel, [])),
    StateList = exa_sm:state_machine_states(FSMModel, []),
    reduce_fsms(FSMModels, [{add_missing_states(StateTransitions, StateList), Exports}|Result]).

%% extract_accept_states
extract_accept_states(States) ->
    extract_accept_states(States, []).

extract_accept_states([], AcceptStates) ->
    lists:reverse(AcceptStates);
extract_accept_states([{{S, accept}, Transitions}|States], AcceptStates) ->
    extract_accept_states(States, [{S, Transitions}|AcceptStates]);
extract_accept_states([{_S, _Transitions}|States], AcceptStates) ->
    extract_accept_states(States, AcceptStates).

%% normal states
extract_normal_states(States) ->
    extract_normal_states(States, []).

extract_normal_states([], NormalStates) ->
    lists:reverse(NormalStates);
extract_normal_states([{{S, normal}, Transitions}|States], NormalStates) ->
    extract_normal_states(States, [{S, Transitions}|NormalStates]);
extract_normal_states([{{_S, error}, _Transitions}|States], NormalStates) ->
    extract_normal_states(States, NormalStates);
extract_normal_states([{{_S, accept}, _Transition}|States], NormalStates) ->
    extract_normal_states(States, NormalStates);
extract_normal_states([{S, Transitions}|States], NormalStates) ->
    extract_normal_states(States, [{S, Transitions}|NormalStates]).

%% error states
extract_error_states(States) ->
    extract_error_states(States, []).

extract_error_states([], ErrorStates) ->
    lists:reverse(ErrorStates);
extract_error_states([{{S, error}, Transitions}|States], ErrorStates) ->
    extract_error_states(States, [{S, Transitions}|ErrorStates]);
extract_error_states([{_S, _Transitions}|States], ErrorStates) ->
    extract_error_states(States, ErrorStates).

%% exported states
generate_number_list(N, 0, Result) ->
    lists:reverse(Result);
generate_number_list(N, Lim, Result) ->
    generate_number_list(N, Lim-1, [N|Result]).

extract_exported_states(States) ->
    extract_exported_states(States, []).

extract_exported_states([], Result) ->
    lists:zip(lists:reverse(Result), generate_number_list(3, length(Result), []));
extract_exported_states([{StateN, StateName, StateType}|States], Result) ->
    extract_exported_states(States, [StateName|Result]);
extract_exported_states([{StateN, StateName}|States], Result) ->
    extract_exported_states(States, [StateName|Result]).

%% generate_sm_module
generate_sm_module(Module, States, ExportedStates) ->
    AcceptStates   = extract_accept_states(States),
    NormalStates   = extract_normal_states(States),
    ErrorStates    = extract_error_states(States),
    compile_code(lists:flatten([[generate_module_directive(Module)],
				[generate_exports_directive(ExportedStates
							   ++ [{init,1}, {model_error,3}, {terminate, 3}])],
				[generate_init()],
				[generate_terminate()],
				[generate_model_error()],
				[generate_accept_state(State, Transitions) ||
				    {State, Transitions} <- AcceptStates],
				[generate_normal_state(State, Transitions) ||
				    {State, Transitions} <- NormalStates],
				[generate_error_state(State, Transitions) ||
				    {State, Transitions} <- ErrorStates]
			       ])).

generate_module_directive(ModuleName) ->
    erl_syntax:attribute(erl_syntax:atom(module), [erl_syntax:atom(ModuleName)]).

generate_behaviour_directive(BehaviourName) ->
    erl_syntax:attribute(erl_syntax:atom(behaviour), [erl_syntax:atom(BehaviourName)]).

generate_exports_directive(ExportedFunctions) ->
    erl_syntax:attribute(
      erl_syntax:atom(export),
      [erl_syntax:list(
	 [erl_syntax:arity_qualifier(
	    erl_syntax:atom(FunctionName),
	    erl_syntax:integer(Arity)
	   ) || {FunctionName, Arity} <- ExportedFunctions]
	)]
     ).

generate_init() ->
    erl_syntax:function(
      erl_syntax:atom('init'),
      [erl_syntax:clause(
	 [erl_syntax:cons(erl_syntax:variable('InitialState'),
			  erl_syntax:variable('FSMModel'))],
	 none,
	 [erl_syntax:tuple(
	    [erl_syntax:atom('ok'), erl_syntax:variable('InitialState'),
	     erl_syntax:variable('FSMModel')]
	   )]
	)]
     ).

generate_terminate() ->
    erl_syntax:function(
      erl_syntax:atom('terminate'),
      [erl_syntax:clause(
	 [erl_syntax:variable('_'), erl_syntax:variable('_'), erl_syntax:variable('_')],
	 none,
	 [erl_syntax:atom('ok')]
	)]
     ).

generate_model_error() ->
    erl_syntax:function(
      erl_syntax:atom('model_error'),
      [erl_syntax:clause(
	 [erl_syntax:variable('_Input'), erl_syntax:variable('_From'), erl_syntax:variable('S')],
	 none,
	 [erl_syntax:tuple(
	    [erl_syntax:atom('stop'), erl_syntax:atom('normal'), erl_syntax:atom('model_error'),
	     erl_syntax:variable('S')]
	   )]
	)]
     ).

generate_transition(State, {Transition, NextState}) ->
    erl_syntax:clause(
      [erl_syntax:abstract(Transition), erl_syntax:variable('_From'),
       erl_syntax:variable('S')],
      none,
      [erl_syntax:tuple(
	 [erl_syntax:atom('reply'),
	  erl_syntax:cons(
	    erl_syntax:tuple(
	      [erl_syntax:atom(State),
	       erl_syntax:abstract(Transition)]
	     ), erl_syntax:variable('S')),
	  erl_syntax:atom(NextState),
	  erl_syntax:cons(
	    erl_syntax:tuple(
	      [erl_syntax:atom(State),
	       erl_syntax:abstract(Transition)]
	     ), erl_syntax:variable('S'))
	 ]
	)]
     ).

generate_state(State, Transitions, MainClause) ->
    erl_syntax:function(
      erl_syntax:atom(State),
      lists:flatten(
	%% transition clause
	[[generate_transition(State, {Transition, NextState}) ||
	     {Transition, NextState} <- Transitions],
	 [MainClause],
	 %% model error clause
	 [erl_syntax:clause(
	    [erl_syntax:variable('_Any'), erl_syntax:variable('_From'),
	     erl_syntax:variable('S')],
	    none,
	    [erl_syntax:tuple(
	       [erl_syntax:atom('reply'),
		erl_syntax:cons(
		  erl_syntax:tuple(
		    [erl_syntax:atom(State),
		     erl_syntax:atom('no_transition')]),
		  erl_syntax:variable('S')),
		erl_syntax:atom('model_error'),
		erl_syntax:cons(erl_syntax:atom('no_transition'),
				erl_syntax:cons(erl_syntax:atom(State), erl_syntax:variable('S')))]
	      )]
	   )]]
       )
     ).

generate_init_state(Transitions) ->
    generate_state('init_state', Transitions,
		   erl_syntax:clause(
		     [erl_syntax:atom('end_of_input'), erl_syntax:variable('_From'),
		      erl_syntax:variable('S')],
		     none,
		     [erl_syntax:tuple(
			[erl_syntax:atom('stop'), erl_syntax:atom('normal'),
			 erl_syntax:atom('accept_state'),
			 erl_syntax:cons(erl_syntax:atom('accept_state'), erl_syntax:variable('S'))]
		       )]
		    )).

generate_accept_state(State, Transitions) ->
    generate_state(State, Transitions,
		   %% accept clause
		   erl_syntax:clause(
		     [erl_syntax:atom('end_of_input'), erl_syntax:variable('_From'),
		      erl_syntax:variable('S')],
		     none,
		     [erl_syntax:tuple(
			[erl_syntax:atom('stop'), erl_syntax:atom('normal'),
			 erl_syntax:atom('accept_state'),
			 erl_syntax:cons(erl_syntax:atom('accept_state'), erl_syntax:variable('S'))]
		       )]
		    )).

generate_error_state(State, Transitions) ->
    generate_state(State, Transitions,
		   %% error clause
		   erl_syntax:clause(
		     [erl_syntax:atom('end_of_input'), erl_syntax:variable('_From'),
		      erl_syntax:variable('S')],
		     none,
		     [erl_syntax:tuple(
			[erl_syntax:atom('stop'), erl_syntax:atom('normal'),
			 erl_syntax:atom('error_state'),
			 erl_syntax:cons(erl_syntax:atom('error_state'), erl_syntax:variable('S'))]
		       )]
		    )).

generate_normal_state(State, Transitions) ->
    generate_state(State, Transitions,
		   %% normal clause
		   erl_syntax:clause(
		     [erl_syntax:atom('end_of_input'), erl_syntax:variable('_From'),
		      erl_syntax:variable('S')],
		     none,
		     [erl_syntax:tuple(
			[erl_syntax:atom('stop'), erl_syntax:atom('normal'),
			 erl_syntax:atom('normal_state'),
			 erl_syntax:cons(erl_syntax:atom('error_state'), erl_syntax:variable('S'))]
		       )]
		    )).

compile_code(AST) -> 
    case compile:forms([erl_syntax:revert(Code) || Code <- AST]) of
	{ok, ModuleName, Binary} ->
	    code:load_binary(ModuleName, "exa_sm_gen", Binary);
	{ok, ModuleName, Binary, _Warnings} ->
	    code:load_binary(ModuleName, "exa_sm_gen", Binary);
	Other ->
	    Other
    end.

