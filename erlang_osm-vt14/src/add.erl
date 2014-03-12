%% @doc Erlang mini project.
-module(add).
-export([start/3, start/4]).

%% @doc Creates a new process and returns pid frm the created process.
-spec start(A,B,Base) -> ok when 
      A::integer(),
      B::integer(), 
      Base::integer().

start(A,B, Base) ->
   Result_pid = spawn(?module, result,num_length(A,Base)).

num_length(A,Base) ->
    if A div Base =/= 0 -> 
	    num_length()
    

%% @doc TODO: add documentation
-spec start(A,B,Base, Options) -> ok when 
      A::integer(),
      B::integer(), 
      Base::integer(),
      Option::atom() | tuple(),
      Options::[Option].

start(A,B,Base, Options) ->
    tbi.






%% @doc Testcases. 

start_test() ->
    ?assertMatch(true, Result_pid(start(10,10,10))).
