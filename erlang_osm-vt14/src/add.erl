%% @doc Erlang mini project.
-module(add).
-export([start/3, start/4,result/3]).
%%import utils for utilit functions
-import(utils,[print_text/3,get_result/1]).
%% To use EUnit we must include this.
-include_lib("eunit/include/eunit.hrl").


%% @doc Creates a new process and returns pid frm the created process.
-spec start(A,B,Base) -> ok when 
      A::integer(),
      B::integer(), 
      Base::integer().

start(A,B, Base) ->
   Result_pid = spawn(?MODULE, result,[A,B,Base]).


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
%%[{[{Carry,Result},...], pos}, ..]
-spec result_handler(A,B,Base,Sum_carry_pos_list) -> integer() | ok when
      A :: integer(),
      B :: integer(),
      Base :: integer(),
      Sum_carry_pos_list :: list().

result_handler(A,B,Base,Sum_carry_pos_list)->
    receive
	({result,Result_carry_pos})->
	    result_handler(A,B,Base,[Result_carry_pos|Sum_carry_pos_list]);
	({get_result,Pid}) ->
	    Result_list = utils:get_result(Sum_carry_pos_list),
	    Pid ! convert_list_to_integer(Result_list);
	(print_text) ->
	    utils:print_text(A,B,Sum_carry_pos_list),
	    result_handler(A,B,Base,Sum_carry_pos_list);
        exit ->
            exit
    end.

    
%%
%%
%%
%%
convert_list_to_integer(List)-> 
    Length = length(List),
    AddList = lists:seq(1,Length).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%                     Testcases. 
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%


%%start_test() ->
   %% ?assertMatch(true, Result_pid(start(10,10,10))).


result_test_()->
    result_result_test_1(),
    result_result_test_2().

%%Test that the resul call will give the right vaule
result_result_test_1()->
    Pid = spawn(?MODULE,result,[13,14,10]),
    Pid ! {result,{[{0,7}],0}},
    Pid ! {result,{[{0,2}],1}},
    Pid ! {get_result,self()},
    receive
        Result -> Result
    end,
    Pid ! exit,
    ?assert(Result =:= 27).

result_result_test_2()->
    Pid = spawn(?MODULE,result,[113,114,10]),
    Pid ! {result,{[{0,2},{0,7}],0}},
    Pid ! {result,{[{0,2}],1}},
    Pid ! {get_result,self()},
    receive
        Result -> Result
    end,
    Pid ! exit,
    ?assert(Result =:= 227).


%%Just test that the function doesn't crash
result_print_test()->
    Pid = spawn(?MODULE,result,[13,14,10]),
    Pid ! {print_text,{[{0,7}],0}},
    Pid ! {print_text,{[{0,2}],1}},
    Pid ! print_text.
