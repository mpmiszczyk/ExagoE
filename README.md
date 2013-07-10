Exago
======

Introduction
------------

This version of Exago is a remake of the old Exago tool. The idea is to create a version that works, and to bring it forward in terms of functionality. 

What is Exago?
--------------

Exago is a tool which was created to perform data mining on event logs. The vision back in the good old days was to connect a state machine model to a set of events produced by a program, and execute these events in the state machine, thereby giving us insight into how consistent the behavior of a program is. 

In certain types of applications this is great, such as those that generate a limited set of events and whose behaviour closely resembles a simple state machine. Telephony applications may benefit heavily from this approach, since the behaviour of these kinds of systems does closely map to an easily definable state machine.

Exago is now capable of analysing logs and generating a state machine representation of the behaviour found therein. The way this is done is that the logs are related by a unique identifier which defines the boundary of the states found within the state machine. 

Status
------

So far the tool works if you compile it and run the examples, however the reporting facilities are quite bare. There is a standard API being developed in exa.erl, but for the moment the API shown in the examples should be used instead (this will be detailed below).

Okay, fine, but how do I run this thing?
----------------------------------------

At the moment you can run it by cloning the repository, then doing:
```bash
make
./start-exago.sh
```       

To run an example, it is recommended you look at the short code that specifies the format of the files. 

For the sms_example:
```erlang
cd("/path/to/ExagoE/src/examples/sms_example/").
c(sms_example).
sms_example:generate_uniques().    %% shows the unique finite state machine behaviours found in the logs
sms_example:generate_combined().   %% shows the combined finit state machine behaviour from the uniques
sms_example:generate_model(Path).  %% generates FSM visualizations at current pwd, Path is a temp dir to store the .dot files 
```

Tutorial 1 - Defining and testing a basic log
=============================================

Introduction
------------

Okay, so you've decided that you would like to use Exago to examine your logs. This tutorial will run through how you would go about using Exago to analyse a simple CSV based log. This log can be interleaved, so messages can come from any different number of sources, but each log message must contain the three required fields "instance_key", "timestamp" and "transition" - either directly in the message or as a foreign key. Foreign keys will be explained in a later tutorial, so for now you must include the three required fields directly in the log. The field "state" must also be included - in the first tutorial we will absorb state from a second log linked in via a foreign key value.

Required fields
---------------

The "instance_key" is an identifier which represents the instance of the program that the message came from. For example, if you have two separate programs (program 1, and program 2) that write to this log, each message must contain the unique identifier for each "session". The "timestamp" is a timestamp in some format of your choosing. We have some basic timestamp parsers at your disposal if you wish to use them, but if you wish to use a different format, you can do this too by specifying a custom field parser in exa_field and exa_field_parser respectively.

For now, unless you are using rfc3339 timestamps, you may use the "partial" timestamp parser which we make use of throughout the tutorials. The way it works is that you specify the ordering of various date time values found within a string, and the rest works out of the box. 

As an example, the following timestamp format:

```erlang
timestamp_format() ->
    [date_fullyear, date_month, date_mday, time_hour, time_minute,
     time_second, time_secfrac].
```

Parses any of the following timestamps: "2010-10-09 12:10:01:00", "2010~10~09T12~10~01~001234" ...

"transition" is the input to the state machine which causes a transition to occur from one state into another.

All of the files for this tutorial may be found under src/examples/tutorial_example. The main log file that we will use as an example is described below:

A simple log, and the one that I will use for this tutorial (`sample_1.log`) is:

```
1,1,2010-10-12 16:00:00:0000000,forward
1,2,2010-10-12 16:00:01:0000000,forward
1,3,2010-10-12 16:00:02:0000000,forward
1,3,2010-10-12 16:00:03:0000000,stop
2,1,2010-10-12 16:01:00:0000000,forward
2,2,2010-10-12 16:01:01:0000000,forward
2,3,2010-10-12 16:01:02:0000000,stop
```

In this log file the first field represents the instance_key, the second field is a foreign_key which is to augment the row with a state value, the third field is a timestamp, and the last field is the value of the state machine transition.

The log whose state values are absorbed is also provided (`sample_2.log`):

```     
    1,start
    2,state_1
    3,state_2
    4,state_3
    5,state_4
```

In this log file the first field represents an integer annotation which will augment the rows in sample_1 with the second field, which represents a state.

Parsing Logs
------------

The first thing to do is to run the system using ./start-exago.sh. Erlang will then run and you will have access to a shell that contains the functionality required to proceed with the tutorial.

To test that your log file parses adequately as a CSV file, to generate the next representation of the data, or to see what the data format should be before proceeding, run:

```erlang
cd("/path/to/code/").
exa_parse:parse({csv, absolute, "/path/to/csv_file.csv"}).
```

Using the tutorial log, the resulting data looks like this:

```erlang
exa_parse:parse({csv, absolute, "./log_files/sample_1.log"}).
[["1","1","2010-10-12 16:00:00:0000000","forward"],
 ["1","2","2010-10-12 16:00:01:0000000","forward"],
 ["1","3","2010-10-12 16:00:02:0000000","forward"],
 ["1","3","2010-10-12 16:00:03:0000000","stop"],
 ["2","1","2010-10-12 16:01:00:0000000","forward"],
 ["2","2","2010-10-12 16:01:01:0000000","forward"],
 ["2","3","2010-10-12 16:01:02:0000000","stop"]]

exa_parse:parse({csv, absolute, "./log_files/sample_2.log"}).
[["1","start"],
 ["2","state_1"],
 ["3","state_2"],
 ["4","state_3"],
 ["5","state_4"]]
```

Custom log format
-----------------

The resulting data format shown above is the 'internal data format'. Any other parser for a different format than CSV will work just as well, as long as it generates data in the internal data format, which is a list of rows (lists). 

Next, Exago needs to know what each field is meant to represent in the log. For this purpose there are "field parsers" available, which take care of turning the rows into an internal format which denote certain types of fields.

A row format is an ordered list of field parser functions. The location in the list where the parser functions are placed is important as this is the order that each field row is parsed. You can choose to use the default parsers provided by Exago, or you can replace them with your own. If you do decide to create your own, you should take a look at how the parsers that currently exist in exa_field_parser work. 

```erlang
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
```

Creating an Event Source
------------------------

Once you are happy that the file parses adequately, and that the row format matches your fields, you may specify it as an event source. 

To do so, you may specify it as follows:
```erlang   
include_files_1() ->
    [{csv, absolute, "./log_files/sample_1.log"}].

include_files_2() ->
    [{csv, absolute, "./log_files/sample_2.log"}].

event_source_1() ->
    {"sample_1", include_files_1(), row_format_1()}.

event_source_2() ->
    {"sample_2", include_files_2(), row_format_2()}.
```

The options here are currently 'absolute' and 'wildcard', depending on whether you wish your event source to be one or a combination of different files.

Once you are happy with your event source, you may collect it into a combined event source, which takes care of resolving foreign keys, generating state and other features. In our example we are absorbing a state field from an external log which contains no relevant data to be kept so we pass the option "absorb", and we give the "implicit_state" option as the second option since we don't want to generate some state automatically. 

```erlang
combined_event_source() ->
    %% from here you can:
    %% 1. collect event sources by absorption
    %% 2. collect event sources by appending
    %% 3. use implicit state
    %% 4. use source_state (state as the filename)
    {"sample_combined", exa_es:collect([ event_source_1(),
                                         event_source_2()],
                                       absorb,
                                       implicit_state)}.
```

The details for absorb and implicit state, and additional features such as filters and modifiers will be described in a later tutorial.

Generating some behaviour
-------------------------

To generate behavioural state machines, one may do so as follows:
```erlang
C = exa_sm:generate_state_machine(combined_event_source(), Options).
```
One may also wish to generate a visualization of a combined state machine, as follows:
```erlang
exa_sm:generate_visualizations(TempPath, [C], 0).
```

Or, if you wish to generate many visualizations of e.g the unique behaviours, that is also a possibility:
```erlang
U = exa_sm:generate_state_machine(combined_event_source(), [{uniques, true}]).
exa_sm:generate_visualizations(TempPath, [U], 0).
```

In some cases it is useful to generate the states so that one may gain insight into how the unique behaviours behave without state. One can do this in the following manner:
```erlang
U = exa_sm:generate_state_machine(combined(), [{uniques, true}, {gen_state, true}]).
```

There are a few things you need to know whilst creating your state machine. First, if you wish to generate a state machine module, it must be deterministic.

** example state machine visualization here **

The Field Parsers
-----------------

The list of default parsers are:

```erlang
exa_field:timestamp(Name, partial, timestamp_format())
exa_field:timestamp(Name, rfc3339)
```

This represents the location in the row where the timestamp can be found.

```erlang
exa_field:annotation(Name, Type)
```

An annotation is any arbitrary field that you wish to label in some way. The label should be a string. Annotations are useful in connection with input modifiers, as we'll see later.

```erlang
exa_field:instance_key(Name, Type)
```

The instance_key parser expects an key value representing the instance_key of some group (as explained above). 

```erlang
exa_field:transition(Name, Type)
```

Represents where the transition_input is, can be any kind of data, but parses as a string.

```erlang
exa_field:foreign_key(EventSourceName, FieldKey, FieldList, Parser)
```

This is perhaps the most complicated field parser, and represents a parser for an Id referencing a value found in another log. Even required fields may be specified as foreign keys. For example, if you wish for a group_id to come from a different log named "GroupIdLog", where the field in the "GroupIdLog" is an annotation with the label "ForeignGroupId", you would use:
```erlang
exago_field:foreign_key("GroupIdLog", "ForeignGroupId", [group_id], exago_field:parser(group_id)).
```

The inclusion of this type of parser means that logs can be seen from the perspective of a relational database. This field will be exemplified in a later tutorial.

```erlang
exa_field:transaction_key(Name, Type)
exa_field:transaction_type(Name, Type)
```

These are advanced fields that should be used when checking the consistency of the transactions in a process driven program. They will be detailed in a more advanced tutorial, but if you wish to see how they work you may check the ttb_example in the examples sub-directory.

Ensuring the Field Parsers work correctly
-----------------------------------------

Where the field format is a list of field parsers, the log data is the list data by using 
```erlang
exa_parse:parse earlier.
```
We can see that a set of results has been produced and that there are no undefined or erroneous values here.

Running the State Machine
-------------------------

* TODO
