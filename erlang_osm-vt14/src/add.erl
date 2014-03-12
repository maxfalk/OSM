%% @doc Erlang mini project.
-module(add).
-export([start/3, start/4,result/3]).
%%import utils for utilit functions
-import(utils,[print_text/4]).
%% To use EUnit we must include this.
-include_lib("eunit/include/eunit.hrl").


%% @doc Creates a new process and returns pid frm the created process.
-spec start(A,B,Base) -> ok when 
      A::integer(),
      B::integer(), 
      Base::integer().

start(A,B, Base) ->
   Result_pid = spawn(?MODULE, result,num_length(A,Base)).

num_length(A,Base) -> tbi.
    %%if A div Base =/= 0 -> 
%%	    num_length()
    

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
-spec result(A::integer(),B::integer(),Base::integer()) -> integer().

result(A,B,Base)->
    result_handler(A,B,Base,[]).

%%@doc Waits to receive results and a possible carry. 
%% Terminates after some process has asked for the result.
%%
%%
-spec result_handler(Base::integer(),Sum_carry_list::list()) -> integer() | ok.

result_handler(Base,Sum_carry_pos_list)->
    receive
	({result,Result_carry_pos})->
	    result_handler(Base,[Result_carry_pos|Sum_carry_pos_list]);
	({get_result,Pid}) ->
	    Pid ! get_result(Sum_carry_pos_list,Base),
	    result_handler(Base,Sum_carry_pos_list);
	(print_text) ->
	    utils:print_text(Sum_carry_pos_list,get_result(Sum_carry_pos_list,Base)),
	    result_handler(Base,Sum_carry_pos_list);
        exit ->
            exit
    end.

%%@doc Calculates the result from a list tuple of
%% [{Result,Carry}, ...]
%%
%%
-spec get_result(Sum_carry_list::list(),Base::integer())-> integer().
    
get_result(Sum_carry_pos_list,Base)->    
    lists:sum([calc_number_from_part(Result,Part,Base) 
               || {Result,Carry,Part} <- Sum_carry_pos_list]).


%%@doc calculates the given number form the base and the pos/part of the current number.
%%
%%
%%
-spec calc_number_from_part(Number::integer(),Part::integer(),Base::integer())-> integer().

calc_number_from_part(Number,Part,Base)-> Number * round(math:pow(Base,Part)).
    

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%                     Testcases. 
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%


%%start_test() ->
   %% ?assertMatch(true, Result_pid(start(10,10,10))).


%%Test that the resul call will give the right vaule
result_result_test()->
    Pid = spawn(?MODULE,result,[13,14,10]),
    Pid ! {result,{7,0,0}},
    Pid ! {result,{5,0,0}},
    Pid ! {result,{3,1,1}},
    Pid ! {get_result,self()},
    receive
        Result -> Result
    end,
    Pid ! exit,
    ?assert(Result =:= 42).

%%Just test that the function doesn't crash
result_print_test()->
    Pid = spawn(?MODULE,result,[13,14,10]),
    Pid ! {result,{7,0,0}},
    Pid ! {result,{5,0,0}},
    Pid ! {result,{3,1,1}},
    Pid ! print_text.
