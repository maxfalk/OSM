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


%%@doc Handels partial results, waits for every process to finish and adds them up to one result.
%%Base: the base for the possible carry in.
-spec result(Base::integer()) -> integer().

result(A,B,Base)->
    result_handler(A,B,Base,[],0).

%%@doc Waits to receive results and a possible carry. 
%% Terminates after some process has asked for the result.
%%
%%
-spec result_handler(Base::integer(),Sum_list::list()) -> integer() or ok.

result_handler(A,B,Base,Sum_carry_list)->
    receive
	(result,Result_carry)->
	    result_handler(Base,[Result_carry|Sum_carry_list]);
	(get_result,Pid) ->
	    Pid ! get_result(Sum_carry_list);
	(print_text,Pid) ->
	    print_text(A,B,Sum_carry_list,get_result(Sum_carry_list))
    end.

%%@doc Calculates the result from a list tuple of
%% [{Result,Carry}, ...]
%%
%%
-spec get_result(Sum_carry_list::list())-> integer().
    
get_result(Sum_carry_list)->    
    lists:sum([Result || {Result,_Carry} <- Sum_carry_list]).

%%@doc prints text representing the addition made
%%
%%
%%
-spec print_text(A,B,Sum_carry_list,Result)-> ok.

print_text(A,B,Sum_carry_list,Result)->
    print_list([Carry || {Carry,_} <- Sum_carry_list]),
    print_line(length(Sum_carry_list)+1),
    print_number(A),
    print_number(B),
    print_line(length(Sum_carry_list)+1),
    print_number(Result).
    


%%@doc prints all elements of a list, then prints a new line.
%%
%%
%%
-spec print_list(List::list())-> ok.

print_list(List)->
    [io:format("~p",[H]) || H <- List],
    io:format("~n",[]).
    
%%@doc Prints a line "------" with n "-", then a newline.
%%
%%
%%
-spec print_line(N:integer())-> ok.

print_line(N)->
    [io:format("-",[]) || lists:seq(0,N)],
    io:format("~n",[]).
%%@doc Prints the Number, then a newline.
%%
%%
%%
-spec print_number(Number:integer())-> ok.

print_number(Number)->
    io:format("~p~n",[Number]).



%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%                     Testcases. 
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%


start_test() ->
    ?assertMatch(true, Result_pid(start(10,10,10))).
