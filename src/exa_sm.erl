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
-module(exa_sm).

-export([strip_results/1, split_by_instance_key/3, unique_instance_keys/1]).
-export([extract_states_and_transitions/1, extract_flat_states_and_transitions/1]).
-export([generate_visualizations/3]).

-export([generate_state_machine/2]).

-export([lookup_field_by_type/2]).

-compile(export_all).

%% @doc Extracts the field sets from the internal result format
-spec(strip_results (Source::list())
      -> FieldSets::list()).	     
strip_results(Sources) ->
    [FieldSet || {_ResultType, FieldSet, _FormatResult} <- 
		     lists:flatten([ResultSet || {_SourceName, ResultSet} <- Sources])].

%% @doc Looks up a field by type in the field sets
-spec(lookup_field_by_type (Type::atom(), FieldSet::list())
      -> Field::any()).	     
lookup_field_by_type(_Type, []) ->
    undefined;
lookup_field_by_type(Type, [{FieldType, FieldIdentifier, FieldValue}|Fields]) ->
    case Type =:= FieldType of
	true  -> {FieldType, FieldIdentifier, FieldValue};
	false -> lookup_field_by_type(Type, Fields)
    end;
lookup_field_by_type(Type, [_|Fields]) ->
    lookup_field_by_type(Type, Fields).

%% @doc Finds all unique instance_keys in a set of events
unique_instance_keys(EventList) ->
    ordsets:from_list([FieldValue 
		       || {instance_key, _FieldIdentifier, {field_value, FieldValue}} 
			      <- [lookup_field_by_type(instance_key, Event) || Event <- EventList]]).

%% @doc Splits a list of events by its instance key
split_by_instance_key(_EventList, [], SplitList) ->
    lists:reverse(SplitList);
split_by_instance_key(EventList, [Key|UniqueInstanceKeys], SplitList) ->
    split_by_instance_key(EventList, UniqueInstanceKeys, 
			  [{instance, Key, 
			    lists:filter(fun (Event) -> 
						 case lookup_field_by_type(instance_key, Event) of
						     {instance_key, _FieldIdentifier, {field_value, FieldValue}} ->
							 FieldValue =:= Key;
						     undefined ->
							 false
						 end
					 end, EventList)}
			   |SplitList]).

fold_flat_instances([], _Fun, OuterResult) ->
    lists:reverse(OuterResult);
fold_flat_instances([{instance, _Key, EventList}|Instances], Fun, OuterResult) ->
    fold_flat_instances(Instances, Fun, 
			[lists:reverse(lists:foldl(Fun, [], EventList))
			 |OuterResult]).

fold_instances([], _Fun, OuterResult) ->
    lists:reverse(OuterResult);
fold_instances([{instance, Key, EventList}|Instances], Fun, OuterResult) ->
    fold_instances(Instances, Fun,
		   [{instance, Key, lists:reverse(lists:foldl(Fun, [], EventList))}
		    |OuterResult]).

extract_flat_states_and_transitions(Instances) ->
    fold_flat_instances(Instances,
			fun (Event, Rest) ->
				[{exa_es_util:extract_field_value_by_type(timestamp, Event),
				  exa_es_util:extract_field_value_by_type(transition, Event),
				  exa_es_util:extract_field_value_by_type(state, Event)}|Rest]
			end, []).

extract_resolved_states_and_transitions(Instances) ->
    fold_flat_instances(Instances,
			fun (Event, Rest) ->
				{state, _, {field_value, Sender}}   = proplists:get_value(state_from, Event),
				{state, _, {field_value, Receiver}} = proplists:get_value(state_to, Event),
				Transition = exa_es_util:extract_field_value_by_type(transition, Event),
				[{exa_es_util:extract_field_value_by_type(timestamp, Event),
				  Sender, Transition, Receiver}|Rest]
			end, []).

extract_states_and_transitions(Instances) ->
    fold_instances(Instances,
		   fun (Event, Rest) ->
			   case lookup_field_by_type(state, Event) of
			       {state, _FieldIdentifierA, {field_value, StateValue}} ->
				   case lookup_field_by_type(transition, Event) of
				       {transition, _FieldIdentifierB, {field_value, TransitionValue}} ->
					   [{TransitionValue, StateValue}|Rest];
				       undefined ->
					   [{undefined, StateValue}|Rest]
				   end;
			       undefined ->
				   case lookup_field_by_type(transition, Event) of
				       {transition, _FieldIdentifierC, {field_value, TransitionValue}} ->
					   [{TransitionValue, undefined}|Rest];
				       undefined ->
					   [{undefined, undefined}|Rest]
				   end
			   end
		   end, []).

%% augment the fsm
augment_state(_StateAtom, [], _StateModifier, Result) ->
    Result;
augment_state(Atom, [{N, Atom}|States], StateModifier, Result) ->
    augment_state(Atom, States, StateModifier, [StateModifier({N, Atom})|Result]);
augment_state(Atom, [{N, Atom, StateType}|States], StateModifier, _Result) ->
    augment_state(Atom, States, StateModifier, [StateModifier({N, Atom, StateType})]);
augment_state(Atom, [{N, StateAtom}|States], StateModifier, Result) ->    
    augment_state(Atom, States, StateModifier, [{N, StateAtom}|Result]);
augment_state(Atom, [{N, StateAtom, StateType}|States], StateModifier, Result) -> 
    augment_state(Atom, States, StateModifier, [{N, StateAtom, StateType}|Result]).

augment_model({States, Transitions}, []) ->
    {lists:reverse(States), Transitions};
augment_model({States, Transitions, AutoGenFlag}, []) ->
    {lists:reverse(States), Transitions, AutoGenFlag};
augment_model({States, Transitions, AutoGenFlag}, [{Atom, StateModifier}|StateFormat]) ->
    augment_model({augment_state(Atom, States, StateModifier, []), Transitions, AutoGenFlag},
		  StateFormat);
augment_model({States, Transitions}, [{Atom, StateModifier}|StateFormat]) ->
    augment_model({augment_state(Atom, States, StateModifier, []), Transitions}, StateFormat).

augment_models(Models, StateFormats) ->
    augment_models(Models, StateFormats, []).

augment_models([], [], AugmentedModels) ->
    lists:reverse(AugmentedModels);
augment_models([FSMModel|FSMModels], [StateFormat|StateFormats], AugmentedModels) ->
    augment_models(FSMModels, StateFormats, [augment_model(FSMModel, StateFormat)|AugmentedModels]).

%% generate fsm
map2(_Fun, [], [], Result) ->
    lists:reverse(Result);
map2(Fun, [H1|List1], [H2|List2], Result) ->
    map2(Fun, List1, List2, [Fun(H1, H2)|Result]).

generate_state_machine(EventSource, Opt) ->
    SortedEventSource = strip_results([EventSource]),
    %%
    LabelledFSMs = 
	case proplists:get_value(gen_state, Opt, false) of
	    true      -> label_previous_states(generate_states(sort_fsms(SortedEventSource)));
	    false     -> label_previous_states(sort_fsms(SortedEventSource))
	end,
    %%
    case proplists:get_value(uniques, Opt, false) of
	true  ->
	    case proplists:get_value(transactional, Opt, false) of
		true  ->
		    LFSMs = rsort_fsms(SortedEventSource),
		    Nodes = lists:map(fun labelled_fsms_to_nodes/1, LFSMs),
		    UniqueTs =
			lists:map(fun (X) -> collect_uniques(X, []) end,
				  map2(fun (Node, LabelledFSM) ->
					       combine(Node, LabelledFSM)
				       end, Nodes, LFSMs, [])),
		    Uniques = ordsets:from_list(UniqueTs),
		    CombinedFSMs =
			lists:map(fun (UniqueFSM) ->
					  lists:map(fun to_node/1,
						    [extract_missing_states(UniqueFSM, existing_states(UniqueFSM, []), [])])
					      ++ UniqueFSM
				  end, Uniques),
		    NumberedStates =
			lists:map(fun (CombinedFSM) ->
					  extract_numbered_states(0, CombinedFSM, [])
				  end, CombinedFSMs),
		    NumberedFSMs =
			map2(fun (FSM, NStates) ->
				     number_fsm(FSM, NStates, [])
			     end, CombinedFSMs, NumberedStates, []),
		    NumberedTransitions =
			lists:map(fun (FSM) ->
					  extract_numbered_transitions(FSM, [])
				  end, NumberedFSMs),
		    CleanStates =
			lists:map(fun (NStates) ->
					  clean_states(NStates, [])
				  end, NumberedStates),
		    lists:map(fun ({CStates, NTransitions}) ->
				      {CStates, NTransitions, 
				       {autogen_possible, is_deterministic(NTransitions)}}
			      end, lists:zip(CleanStates, NumberedTransitions));
		false ->
		    Nodes    = lists:map(fun labelled_fsms_to_nodes/1, LabelledFSMs),
		    UniqueTs = 
			lists:map(fun (X) -> collect_uniques(X, []) end,
				  map2(fun (Node, LabelledFSM) ->
					       combine(Node, LabelledFSM)
				       end, Nodes, LabelledFSMs, [])),
		    Uniques  = ordsets:from_list(UniqueTs),
		    CombinedFSMs = 
			lists:map(fun (UniqueFSM) ->
					  lists:map(fun to_node/1, 
						    [extract_missing_states(UniqueFSM, existing_states(UniqueFSM, []), [])])
					      ++ UniqueFSM
				  end, Uniques),
		    NumberedStates = 
			lists:map(fun (CombinedFSM) ->
					  extract_numbered_states(0, CombinedFSM, [])
				  end, CombinedFSMs),
		    NumberedFSMs = 
			map2(fun (FSM, NStates) ->
				     number_fsm(FSM, NStates, [])
			     end, CombinedFSMs, NumberedStates, []),
		    NumberedTransitions =
			lists:map(fun (FSM) ->
					  extract_numbered_transitions(FSM, [])
				  end, NumberedFSMs),
		    CleanStates =
			lists:map(fun (NStates) ->
					  clean_states(NStates, [])
				  end, NumberedStates),
		    lists:map(fun ({CStates, NTransitions}) ->
				      {CStates, NTransitions, 
				       {autogen_possible, is_deterministic(NTransitions)}}
			      end, lists:zip(CleanStates, NumberedTransitions))
	    end;
	false ->
	    Nodes   = labelled_fsms_to_nodes(LabelledFSMs),
	    Uniques = collect_uniques(combine(Nodes, LabelledFSMs), []),
	    CombinedFSM  = lists:map(fun to_node/1, [extract_missing_states(Uniques, existing_states(Uniques, []), [])])
		++ Uniques,
	    NumberedStates      = extract_numbered_states(0, CombinedFSM, []),
	    NumberedFSM         = number_fsm(CombinedFSM, NumberedStates, []),
	    NumberedTransitions = extract_numbered_transitions(NumberedFSM, []),
	    CleanStates         = clean_states(NumberedStates, []),
	    {CleanStates, NumberedTransitions, {autogen_possible, is_deterministic(NumberedTransitions)}}
    end.

%% state_machine_states
state_machine_states({[], _Transitions, _AutoGenFlag}, Result) ->
    lists:reverse(Result);
state_machine_states({[{_N, State, StateType}|States], Transitions, AutoGenFlag}, Result) ->
    state_machine_states({States, Transitions, AutoGenFlag}, [{State, StateType}|Result]);
state_machine_states({[{_N, State}|States], Transitions, AutoGenFlag}, Result) ->
    state_machine_states({States, Transitions, AutoGenFlag}, [State|Result]).

%% state_machine_transitions
state_machine_transitions({_States, [], _AutoGenFlag}, Result) ->
    lists:reverse(Result);
state_machine_transitions({States, [{_FromN, Transition, _ToN}|Transitions], AutoGenFlag}, Result) ->
    state_machine_transitions({States, Transitions, AutoGenFlag}, [Transition|Result]);

state_machine_transitions([], Result) ->
    lists:reverse(Result);
state_machine_transitions([SM|StateMachines], Result) ->
    state_machine_transitions(StateMachines, [state_machine_transitions(SM, [])|Result]).

%% check that the state machine is deterministic
start_member({_NFrom, _TValue}, []) ->
    true;
start_member({NFrom, TValue}, [{NFrom, TValue, _NTo0}|_Transitions]) ->
    false;
start_member({NFrom, TValue}, [{_NFrom0, _TValue0, _NTo0}|Transitions]) ->
    start_member({NFrom, TValue}, Transitions).

is_deterministic(Transitions) ->
    lists:foldl(
      fun (Transition, _Rest) ->
	      {NFrom, TValue, _NTo} = Transition,
	      start_member({NFrom, TValue}, lists:delete(Transition, Transitions))
      end, true, Transitions
     ).

%% 
clean_states([], Result) ->
    lists:reverse(Result);
clean_states([{N, State}|States], Result) when is_list(State) ->
    clean_states(States, [{N, list_to_atom(State)}|Result]);
clean_states([{N, State}|States], Result) when is_atom(State) ->
    clean_states(States, [{N, State}|Result]);
clean_states([{N, undefined}|States], Result) ->
    clean_states(States, [{N, no_state}|Result]);
clean_states([{_N, _Other}|_States], _Result) ->
    {error, invalid_state}.

%%
pretty_state(0) -> "start";
pretty_state(N) -> "state_" ++ lists:flatten(io_lib:fwrite("~p", [N])).

generate_numbered_states(_N, [], NumberedStates) ->
    lists:reverse(NumberedStates);
generate_numbered_states(N, [{Transition, _NoState}|States], NumberedStates) ->
    generate_numbered_states(N+1, States, [{Transition, pretty_state(N)}|NumberedStates]).

generate_states(SortedFSMs) ->
    lists:map(fun (FSM) -> 
		      generate_numbered_states(0, FSM, [])
	      end, SortedFSMs).

%%
rremove_timestamp({_TS, StateFrom, Transition, StateTo}) ->
    {StateFrom, Transition, StateTo}.

rremove_timestamps(FSMs) ->
    lists:map(fun (FSM) -> lists:map(fun rremove_timestamp/1, FSM) end, FSMs).

%%
remove_timestamp({_TS, Transition, State}) -> 
    {Transition, State}.

remove_timestamps(FSMs) ->
    lists:map(fun (FSM) -> lists:map(fun remove_timestamp/1, FSM) end, FSMs).

%% from previous-state -> transition -> next-state
label_previous([], _PreviousState, Result) ->
    lists:reverse(Result);
label_previous([{_Transition, NextState}|FSM], no_state, Result) ->
    label_previous(FSM, NextState, Result);
label_previous([{Transition, NextState}|FSM], PreviousState, Result) ->
    label_previous(FSM, NextState, [{PreviousState, Transition, NextState}|Result]).

to_node({P,_,_}) -> {P, []};
to_node(P)       -> {P, []}.

combine_node(Node, FSM) ->
    combine_node_(Node, FSM, []). 

combine_node_({_PX, _L0}, [], R) ->
    R;
combine_node_({PX, L0}, [{PX, T1, N1}|FSM], []) ->
    combine_node_({PX, L0}, FSM, {PX, [{T1,N1}]});
combine_node_({PX, L0}, [{PX, T1, N1}|FSM], {PX, LR}) ->
    combine_node_({PX, L0}, FSM, {PX, [{T1,N1}|LR]});
combine_node_({P0, L0}, [{_P1, _T1, _N1}|FSM], Result) ->
    combine_node_({P0, L0}, FSM, Result).

combine_nodes(Nodes, FSM) ->
    lists:map(fun (N) -> combine_node(N, FSM) end, Nodes).

combine(Nodes, FSMs) ->
    ordsets:from_list(combine_nodes(Nodes, lists:flatten(FSMs))).

collect_uniques([], R) ->
    lists:reverse(R);
collect_uniques([{P,L}|Nodes], R) ->
    collect_uniques(Nodes, [{P,ordsets:from_list(L)}|R]).

%% @doc Resolved instances -> FSMs
extract_rtimed_fsms(SortedEventSource) ->
    exa_sm:extract_resolved_states_and_transitions(
      exa_sm:split_by_instance_key(SortedEventSource, exa_sm:unique_instance_keys(SortedEventSource), [])
     ).

uorder_rfsms(SortedEventSource) ->
    lists:map(fun (FSM) ->
		      lists:usort(
			fun ({TS1, _, _, _}, {TS2, _, _, _}) ->
				Timestamp1 = lists:map(fun ({V, _K}) -> V end, TS1),
				Timestamp2 = lists:map(fun ({V, _K}) -> V end, TS2),
				Timestamp1 < Timestamp2
			end, FSM)
	      end, extract_rtimed_fsms(SortedEventSource)).

rsort_fsms(SortedEventSource) ->
    rremove_timestamps(uorder_rfsms(SortedEventSource)).

%% @doc Flat instances -> FSMs
extract_timed_fsms(SortedEventSource) ->
    exa_sm:extract_flat_states_and_transitions(
      exa_sm:split_by_instance_key(SortedEventSource, exa_sm:unique_instance_keys(SortedEventSource), [])
     ).

uorder_fsms(SortedEventSource) ->
    lists:map(fun (FSM) ->
		      lists:usort(
			fun ({TS1, _, _}, {TS2, _, _}) ->
				Timestamp1 = lists:map(fun ({V, _K}) -> V end, TS1),
				Timestamp2 = lists:map(fun ({V, _K}) -> V end, TS2),
				Timestamp1 < Timestamp2
			end, FSM)
	      end, extract_timed_fsms(SortedEventSource)).

sort_fsms(SortedEventSource) ->
    remove_timestamps(uorder_fsms(SortedEventSource)).

label_previous_states(FSMs) ->
    lists:map(fun (FSM) -> label_previous(FSM, "init_state", []) end, FSMs).

labelled_fsms_to_nodes(LabelledFSMs) ->
    lists:map(fun to_node/1, lists:usort(lists:flatten(LabelledFSMs))).

existing_states([], Result) ->
    lists:reverse(Result);
existing_states([{S,_StateList}|Nodes], Result) ->
    existing_states(Nodes, [S|Result]).

exists(_, []) ->
    false;
exists(S, [S|_ExistingStates]) ->
    true;
exists(S, [_State|ExistingStates]) ->
    exists(S, ExistingStates).

find_missing_states([], _ExistingStates, Result) ->
    lists:reverse(Result);
find_missing_states([{_T, S}|StateList], ExistingStates, Result) ->
    case exists(S, ExistingStates) of
	true  -> find_missing_states(StateList, ExistingStates, Result);
	false -> find_missing_states(StateList, ExistingStates, [S|Result])
    end.

extract_missing_states([], _, Result) ->
    lists:filter(fun (X) -> X =/= [] end, lists:flatten(ordsets:from_list(lists:reverse(Result))));
extract_missing_states([{_S, StateList}|Nodes], ExistingStates, Result) ->
    extract_missing_states(Nodes, ExistingStates, [find_missing_states(StateList, ExistingStates, [])|Result]).

extract_numbered_states(_N, [], R) ->
    lists:reverse(R);
extract_numbered_states(N, [{A,_L}|Nodes], R) ->
    extract_numbered_states(N+1, Nodes, [{N,A}|R]).

lookup_state_number({T, S}, [{N, S}|_NumberedStates]) ->
    {T, {N, S}};
lookup_state_number({T, S0}, [{_N, _S1}|NumberedStates]) ->
    lookup_state_number({T, S0}, NumberedStates);
lookup_state_number(_S, [{N, _S}|_NumberedStates]) ->
    N;
lookup_state_number(S0, [{_N, _S1}|NumberedStates]) ->
    lookup_state_number(S0, NumberedStates).

number_node({S, L}, NumberedStates) ->
    {{lookup_state_number(S, NumberedStates), S},
     lists:map(fun (S1) -> lookup_state_number(S1, NumberedStates) end, L)}.

number_fsm([], _NumberedStates, Result) ->
    lists:reverse(Result);
number_fsm([State|AllStates], NumberedStates, Result) ->
    number_fsm(AllStates, NumberedStates, [number_node(State, NumberedStates)|Result]).

extract_transitions(_NPrevious, [], Result) ->
    lists:reverse(Result);
extract_transitions(NPrevious, [{Transition, {NNext, _NextState}}|TransitionList], Result) ->
    extract_transitions(NPrevious, TransitionList, [{NPrevious, Transition, NNext}|Result]).
	
extract_numbered_transitions([], Result) ->
    lists:flatten(lists:reverse(Result));
extract_numbered_transitions([{{NPrevious, _S0}, TransitionList}|NumberedFSM], Result) ->
    lists:filter(fun (X) -> X =/= [] end, 
		 extract_numbered_transitions(NumberedFSM, [extract_transitions(NPrevious, TransitionList, [])|Result])).    
%% end generate FSM

%% execute FSM
execute_state_machine(FSM, ResultData) ->
    {FSM, ResultData}.
%% end execute FSM

%% visualize FSM
write_state(IoDevice, {Type, N, Label}) ->
    case Type of
	normal ->
	    io:fwrite(IoDevice, "st~p [shape=~s,color=\"~s\",style=filled,fontsize=10,"
		      "label=\"~s\"];\n", [N, "circle", "#aaaaee", Label]);
	accept ->
	    io:fwrite(IoDevice, "st~p [shape=~s,color=\"~s\",style=filled,fontsize=10,"
		      "label=\"~s\"];\n", [N, "circle", "#aaeeaa", Label]);
	error  ->
	    io:fwrite(IoDevice, "st~p [shape=~s,color=\"~s\",style=filled,fontsize=10,"
		      "label=\"~s\"];\n", [N, "circle", "#eeaaaa", Label])
    end.

write_states(_IoDevice, []) -> ok;
write_states(IoDevice, [{N, undefined}|States]) ->
    StateLabel = io_lib:fwrite("State ~p", [N]),
    write_state(IoDevice, {normal, N, StateLabel}),
    write_states(IoDevice, States);
write_states(IoDevice, [{N, StateLabel}|States]) ->
    write_state(IoDevice, {normal, N, StateLabel}),
    write_states(IoDevice, States);
write_states(IoDevice, [{N, StateLabel, StateType}|States]) ->
    write_state(IoDevice, {StateType, N, StateLabel}),
    write_states(IoDevice, States).

write_states(_IoDevice, [], _N) -> ok;
write_states(IoDevice, [{_Transition, undefined}|Data], N) ->
    [H|_] = io_lib:fwrite("~p", [N]),
    io:format("Writing state: " ++ H, []),
    write_state(IoDevice, {normal, N, "State " ++ H}),
    write_states(IoDevice, Data, N+1);
write_states(IoDevice, [{_Transition, {_StateType, StateLabel}}|Data], N) -> 
    io:format("Writing state: ~p\n", [StateLabel]),
    write_state(IoDevice, {normal, N, StateLabel}),
    write_states(IoDevice, Data, N+1).

write_transition(IoDevice, {FromN, ToN, Label}) when is_list(Label) ->
    io:fwrite(IoDevice, "st~p -> st~p [fontsize=10,label=\"~s\"];\n",
	      [FromN, ToN, Label]).

write_transitions(_IoDevice, []) -> ok;
write_transitions(IoDevice, [{FromN, Transition, ToN}|Transitions]) when is_list(Transition) -> 
    write_transition(IoDevice, {FromN, ToN, Transition}),
    write_transitions(IoDevice, Transitions);
write_transitions(IoDevice, [{FromN, Transition, ToN}|Transitions]) when is_atom(Transition) -> 
    write_transition(IoDevice, {FromN, ToN, atom_to_list(Transition)}),
    write_transitions(IoDevice, Transitions).

write_transitions(_IoDevice, [], _FromN, _ToN) -> ok;
write_transitions(IoDevice, [{Transition, _State}|FSM], FromN, ToN) when is_list(Transition) -> 
    write_transition(IoDevice, {FromN, ToN, Transition}),
    write_transitions(IoDevice, FSM, FromN+1, ToN+1);
write_transitions(IoDevice, [{Transition, _State}|FSM], FromN, ToN) when is_atom(Transition) ->
    write_transition(IoDevice, {FromN, ToN, atom_to_list(Transition)}),
    write_transitions(IoDevice, FSM, FromN+1, ToN+1).

write_state_machine(IoDevice, _Key, FlatInstance) ->
    io:fwrite(IoDevice, "digraph G {\n", []),
    io:fwrite(IoDevice, "#size=\"6,6\"\n", []),
    write_states(IoDevice, FlatInstance, 0),
    write_transitions(IoDevice, FlatInstance, 0, 1),
    io:fwrite(IoDevice, "}\n", []).

generate_flat_visualization(Path, Key, FlatInstance) ->
    case file:open(Path ++ io_lib:fwrite("~p", [Key]) ++ ".dot", [write]) of
	{ok, IoDevice} ->
	    Result = write_state_machine(IoDevice, Key, FlatInstance),
	    file:close(IoDevice),
	    Result;
	Error          -> 
	    Error
    end.

write_state_and_transitions(_IoDevice, []) -> ok;
write_state_and_transitions(IoDevice, [{N, State, _StateTransitions}|Tuples]) ->
    write_state(IoDevice, {normal, N, State}),
    write_state_and_transitions(IoDevice, Tuples).

write_fsm(IoDevice, Key, {States, Transitions, _AutoGenFlag}) ->
    write_fsm(IoDevice, Key, {States, Transitions});
write_fsm(IoDevice, _Key, {States, Transitions}) ->
    io:fwrite(IoDevice, "digraph G {\n", []),
    io:fwrite(IoDevice, "#size=\"6,6\"\n", []),
    write_states(IoDevice, States),
    write_transitions(IoDevice, Transitions),
    io:fwrite(IoDevice, "}\n", []).

generate_fsm_visualization(Path, Key, IdealFSM) ->
    case file:open(Path ++ io_lib:fwrite("~p", [Key]) ++ ".dot", [write]) of
	{ok, IoDevice} ->
	    Result = write_fsm(IoDevice, Key, IdealFSM),
	    file:close(IoDevice),
	    Result;
	Error ->
	    Error
    end.
    
generate_visualizations(_Path, [], _N) ->
    ok;
generate_visualizations(Path, [FlatInstance|FlatInstances], N) ->
    generate_fsm_visualization(Path, N, FlatInstance),
    [H|_] = io_lib:fwrite("~p", [N]),
    os:cmd("dot -T png " ++ Path ++ H ++ ".dot -o " ++ H ++ ".png"),
    generate_visualizations(Path, FlatInstances, N+1).
%% end visualize FSM    
