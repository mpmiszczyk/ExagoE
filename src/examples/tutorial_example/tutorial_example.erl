-module(tutorial_example).

-export([combined_event_source/0]).
-export([generate_model/0]).

-compile(export_all).

timestamp_format() ->
    [date_fullyear, date_month, date_mday,
     time_hour, time_minute, time_second,
     time_secfrac].

row_format_1() ->
    [exa_field:instance_key("id", integer),
     exa_field:foreign_key("foreignKey", integer, "sample_2", "linkKey", ["state"]),
     exa_field:timestamp("timestamp", partial, timestamp_format()),
     exa_field:transition("move", atom)].

row_format_2() ->
    [exa_field:annotation("linkKey", integer),
     exa_field:state("state", string)].

include_files_1() ->
    [{csv, absolute, "./log_files/sample_1.log"}].

include_files_2() ->
    [{csv, absolute, "./log_files/sample_2.log"}].

event_source_1() ->
    {"sample_1", include_files_1(), row_format_1()}.

event_source_2() ->
    {"sample_2", include_files_2(), row_format_2()}.

combined_event_source() ->
    %% from here you can:
    %% 1. collect event sources by absorption
    %% 2. collect event sources by appending
    %% 3. use implicit state
    %% 4. use source_state (state as the filename)
    {"sample_combined", exa_es:collect([event_source_1(), event_source_2()], absorb, implicit_state)}.

state_format() ->
    [exa_state:state(start, error),    %% start is an erroneous state to end in
     exa_state:state(state_1, normal), %% state_1 is an erroneous state
     exa_state:state(state_2, accept)].

generate_model() ->
    %% from here you can:
    %% 1. generate the state machine behavioural model found in the event source:
    %%    > Model  = exa_sm:generate_state_machine(combined_event_source(), [])
    %% 2. generate the unique state machine models found in the event source:
    %%    > Models = exa_sm:generate_state_machine(combined_event_source(), [{uniques, true}])
    %% 3. debug a state machine model that has no way of providing states:
    %%    > Models = exa_sm:generate_state_machine(combined_event_source(), [{uniques, true}, {gen_state, true}])
    Uniques  = exa_sm:augment_models(exa_sm:generate_state_machine(combined_event_source(), [{uniques, true}]),
				    [state_format(), state_format()]),
    Combined = exa_sm:augment_model(exa_sm:generate_state_machine(combined_event_source(), []), state_format()),
    Combined.
%%    exa_sm_sup:execute_fsms(exa_sm_gen:reduce_fsms(Uniques, []), 
%%			    [{atom, forward},{atom,forward},{atom,forward},{atom,forward},{atom,forward},{atom,forward}]),
%%    exa_sm:generate_visualizations(Uniques, 5),
%%    Transitions = exa_sm:state_machine_transitions(Combined, []),
%%    exa_sm_sup:execute_fsms(exa_sm_gen:reduce_fsms(Uniques, []), Transitions).

    %% 4. augment a generated model with a state format
    %%    > exa_sm:augment_model(Model, StateFormat)
%% 5. augment a series of generated models with a series of state formats
%%    > exa_sm:augment_models(Models, StateFormats)

    
