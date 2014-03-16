
-module(calc). 

-export([start_calc/4]).

%% To use EUnit we must include this.
-include_lib("eunit/include/eunit.hrl").

-export_type([result_list/0]).
-opaque result_list() :: [{[{integer(),integer()}, ...],integer()}, ...]. 


-compile(export_all). 

%% CALC


%% @doc Splits A and B into Split number of similar length lists and 
%% spawns the same number of processes that will add each corresponding
%% sublist of A and B in parts.
%%
%% Each subprocess will message the ParentPid process with {N, Result},
%% where N is its sublist number and Result is the sum of the addition 
%% including carry bits, and the tuple {carry, Carry} where Carry is the
%% highest order carry out of the addition.


-spec start_calc(ParentPid, LeftSplit, RightSplit, Options) -> ok when
      ParentPid :: pid(),
      LeftSplit :: [[integer()]],
      RightSplit :: [[integer()]],
      Min :: integer(),
      Max :: integer(),
      Base :: integer(),
      Split :: integer(),
      Spawn :: boolean(),
      Sleep :: false | {Min, Max},
      Options :: {Base, Split, Spawn, Sleep}.


start_calc(ParentPid, LeftSplit, RightSplit, {Base, Split, Spawn, Sleep}) when Base >= 1 ->
    spawn_calculators(LeftSplit, RightSplit, Split, {Base, Spawn, Sleep}, ParentPid).




%% @doc Spawns N number of processes that will add each corresponding
%% sublist of A and B in parts
%%
%% Each subprocess will message the ParentPid process with {N, Result},
%% where N is its sublist number and Result is the sum of the addition 
%% including carry bits, and the tuple {carry, Carry} where Carry is the
%% highest order carry out of the last addition process to finish. 


-spec spawn_calculators(Left, Right, N, Options, ParentPid) -> ok when
      Left :: list(),
      Right :: list(),
      N :: integer(),
      ParentPid :: pid(),
      Min :: integer(),
      Max :: integer(),
      Base :: integer(),
      Spawn :: boolean(),
      Sleep :: false | {Min, Max},
      Options :: {Base, Spawn, Sleep}.


spawn_calculators(Left, Right, N, Options, ParentPid) when N =:= length(Left) ->
    spawn_calculators_rec(Left, Right, N-1, Options, ParentPid, ParentPid),
    ok.
    
spawn_calculators_rec([A|[]], [B|[]], 0, Options, ParentPid, PreviousPid) -> 
    PidNew = spawn(?MODULE, calculator, [A, B, 0, Options, ParentPid, PreviousPid]),
    PidNew ! {carry, 0};

spawn_calculators_rec([A|Left], [B|Right], N, Options, ParentPid, PreviousPid)  ->
    PidNew = spawn(?MODULE, calculator, [A, B, N, Options, ParentPid, PreviousPid]),
    spawn_calculators_rec(Left, Right, N-1, Options, ParentPid, PidNew).

	    


%% @doc Adds A and B after receiving a carry in then messages the PreviousPid 
%% process with the carry out of the addition and ParentPid with the result
%% of the addition.

-spec calculator(A, B, N, Options, ParentPid, PreviousPid) -> ok when
      A :: list(),
      B :: list(),
      N :: integer(),
      ParentPid :: pid(),
      PreviousPid :: pid(),
      Min :: integer(),
      Max :: integer(),
      Base :: integer(),
      Spawn :: boolean(),
      Sleep :: false | {Min, Max},
      Options :: {Base, Spawn, Sleep}.


calculator(A, B, N, {Base, false, Sleep}, ParentPid, PreviousPid) ->
    receive 
	{carry, CarryIn} -> Result = add(A, B, Base, CarryIn, Sleep)
    end,
    [{CarryOut, _} | _] = Result,
    PreviousPid ! {carry, CarryOut}, 
    ParentPid ! {result, {Result, N}},
    ok;

calculator(A, B, N, {Base, true, Sleep}, ParentPid, PreviousPid) ->
    PidZero = spawn(?MODULE, add_proc, [self(), A, B, Base, 0, Sleep]),
    PidOne = spawn(?MODULE, add_proc, [self(), A, B, Base, 1, Sleep]),
    receive
	{carry, CarryIn} ->
	    case CarryIn of 
		0 -> exit(PidOne, kill);
		1 -> exit(PidZero, kill)
	    end,
	    receive
		{result, CarryIn, Result} -> [H | _] = Result,
					     {CarryOut, _} = H
	    end
    end,
    PreviousPid ! {carry, CarryOut},
    ParentPid ! {result, {Result, N}},
    ok.




%% @doc Adds List1, List2 and the carry in C in base Base and messages
%% the ParentPid process with the result.

-spec add_proc(ParentPid, List1, List2, Base, Carry, Sleep) -> {Carry, List3} when
      ParentPid :: pid(),
      N :: integer(),
      Min :: integer(),
      Max :: integer(),
      T :: {N, N},
      Carry :: integer(),
      List1 :: [N],
      List2 :: [N],
      Base :: integer(),
      List3 :: [T],
      Sleep :: false | {Min, Max}.


add_proc(ParentPid, A, B, Base, Carry, Sleep) ->
    Result = add(A, B, Base, Carry, Sleep),
    ParentPid ! {result, Carry, Result}.




%% @doc Adds two numbers, represented as equally long lists of digits, of any but equal base 
%% plus a carry in bit and returns the sum as a list of digits and remainders.
%% 
%% === Example ===
%% 
%% <div class="example">```
%% 1> utils:add([1,2,3,4,5],[1,2,3,4,5], 10, 1).
%% [{0,2},{0,4},{0,6},{0,9},{1,1}]
%% 2> utils:add([1,2,3,4,5],[1,2,3,4,5], 10, 0).
%% [{0,2},{0,4},{0,6},{0,9},{1,0}]'''
%% </div>

-spec add(List1, List2, Base, C, Sleep) -> List3 when
      N :: integer(),
      Min :: integer(),
      Max :: integer(),
      T :: {N, N},
      C :: integer(),
      List1 :: [N],
      List2 :: [N],
      Base :: integer(),
      List3 :: [T],
      Sleep :: false | {Min, Max}.


add([A | []], [B | []], Base, C, Sleep) when Base >= 1, A < Base, B < Base, C =< 1, C >= 0 ->
    case Sleep of
	{Min, Max} -> timer:sleep(Min + random:uniform(Max - Min));
	false -> ok
    end,
    Sum = A + B + C,
    Quot = Sum div Base,
    Rem = Sum rem Base,
    [{Quot, Rem}];

add([A | Left], [B | Right], Base, C, Sleep) when Base >= 1, A < Base, B < Base ->
    case Sleep of
	{Min, Max} -> timer:sleep(Min + random:uniform(Max - Min));
	false -> ok
    end,
    [{Carry, Result} | List] = add(Left, Right, Base, C, Sleep),
    Sum = A + B + Carry,
    Quot = Sum div Base,
    Rem = Sum rem Base,
    [{Quot, Rem} | [{Carry, Result} | List]].




%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%                                                                          %%
%%			   EUnit Test Cases                                 %%
%%                                                                          %%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

    
add_test() ->
    ?assert(add([1,2,3,4,5],[5,4,3,2,1], 10, 0, false) =:= [{0,6},{0,6},{0,6},{0,6},{0,6}]),
    ?assert(add([1,1,1],[1,1,1], 2, 0, false) =:= [{1,1},{1,1},{1,0}]),
    ?assert(add([1,1,1],[1,1,1], 2, 0, {5, 10}) =:= [{1,1},{1,1},{1,0}]).


add_proc_test() ->
    Pid = self(),
    ?assert(add_proc(Pid, [1,1,1],[1,1,1], 2, 0, {5, 10}) =:= {result, 0, [{1,1},{1,1},{1,0}]}),
    receive
	{result, 0, Result1} -> ?assert(Result1 =:= [{1,1},{1,1},{1,0}])
    end,
    ?assert(add_proc(Pid, [1,1,1],[1,1,1], 2, 1, false) =:= {result, 1, [{1,1},{1,1},{1,1}]}),
    receive
	{result, Carry, Result2} -> ?assert(Result2 =:= [{1,1},{1,1},{1,1}]),
			    ?assert(Carry =:= 1)
    end.


calculator_test() ->
    Pid = self(),
    CalcPid1 = spawn(?MODULE, calculator, [[1,1,1], [1,1,1], 1, {2, false, false}, Pid, Pid]),
    CalcPid1 ! {carry, 1},
    receive
	{carry, Carry1} -> ?assert(Carry1 =:= 1)
    end,
    receive
	{result, {Result1, N1}} -> ?assert(Result1 =:= [{1,1},{1,1},{1,1}]),
			 ?assert(N1 =:= 1)
    end,
    CalcPid2 = spawn(?MODULE, calculator, [[1,1,1], [1,1,1], 1, {2, true, false}, Pid, Pid]),
    CalcPid2 ! {carry, 1},
    receive
	{carry, Carry2} -> ?assert(Carry2 =:= 1)
    end,
    receive
	{result, {Result2, N2}} -> ?assert(Result2 =:= [{1,1},{1,1},{1,1}]),
			 ?assert(N2 =:= 1)
    end.
    

spawn_calculators_test() ->
    {A, B} = {utils:split([1,2,3,4,5,6,7,8,9], 3), utils:split([9,8,7,6,5,4,3,2,1], 3)},
    Pid = self(),
    spawn_calculators(A, B, 3, {10, false, false}, Pid),
    receive
	{carry, Carry1} -> ?assert(Carry1 =:= 1)
    end,
    receive
	{result, {Result1, 0}} ->  ?assert(Result1 =:= [{1,1},{1,1},{1,0}]) 
    end,
    receive
	{result, {Result2, 1}} ->  ?assert(Result2 =:= [{1,1},{1,1},{1,1}]) 
    end,
    receive
	{result, {Result3, 2}} ->  ?assert(Result3 =:= [{1,1},{1,1},{1,1}]) 
    end,
    spawn_calculators(A, B, 3, {10, true, {100, 1000}}, Pid),
    receive
	{carry, Carry2} -> ?assert(Carry2 =:= 1)
    end,
    receive
	{result, {Result12, 0}} ->  ?assert(Result12 =:= [{1,1},{1,1},{1,0}]) 
    end,
    receive
	{result, {Result22, 1}} ->  ?assert(Result22 =:= [{1,1},{1,1},{1,1}]) 
    end,
    receive
	{result, {Result32, 2}} ->  ?assert(Result32 =:= [{1,1},{1,1},{1,1}]) 
    end.
    

start_calc_test() ->
    Pid = self(),
    start_calc(Pid, [[1],[1],[1]], [[1],[0],[1]], {2, 3, true, {100, 1000}}),
    receive
	{carry, Carry} -> ?assert(Carry =:= 1)
    end,
    receive
	{result, {Result1, 0}} -> ?assert(Result1 =:= [{1,0}])
    end,
    receive
	{result, {Result2, 1}} -> ?assert(Result2 =:= [{1,0}])
    end,
    receive
	{result, {Result3, 2}} -> ?assert(Result3 =:= [{1,1}])
    end.
