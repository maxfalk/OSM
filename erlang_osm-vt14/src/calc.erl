
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
%% including carry bits.

-spec start_calc(ParentPid, LeftSplit, RightSplit, Options) -> integer() when
      ParentPid :: pid(),
      LeftSplit :: list(),
      RightSplit :: list(),
      Min :: integer(),
      Max :: integer(),
      Base :: integer(),
      Split :: integer(),
      Spawn :: boolean(),
      Sleep :: false | {Min, Max},
      Options :: {Base, Split, Spawn, Sleep}.


start_calc(ParentPid, LeftSplit, RightSplit, {Base, Split, Spawn, Sleep}) ->
    spawn_calculators(LeftSplit, RightSplit, Split, {Base, Spawn, Sleep}, ParentPid).




%% @doc Spawns N number of processes that will add each corresponding
%% sublist of A and B in parts
%%
%% Each subprocess will message the ParentPid process with {N, Result},
%% where N is its sublist number and Result is the sum of the addition 
%% including carry bits.

-spec spawn_calculators(Left, Right, N, Options, ParentPid) -> pid() when
      Left :: integer(),
      Right :: integer(),
      N :: integer(),
      ParentPid :: pid(),
      Min :: integer(),
      Max :: integer(),
      Base :: integer(),
      Spawn :: boolean(),
      Sleep :: false | {Min, Max},
      Options :: {Base, Spawn, Sleep}.


spawn_calculators(Left, Right, N, Options, ParentPid) when N =:= length(Left) ->
    spawn_calculators_rec(Left, Right, N, Options, ParentPid, ParentPid).
    
spawn_calculators_rec([A|[]], [B|[]], 1, Options, ParentPid, PreviousPid) -> 
    spawn(?MODULE, calculator, [A, B, 1, Options, ParentPid, PreviousPid]);

spawn_calculators_rec([A|Left], [B|Right], N, Options, ParentPid, PreviousPid)  ->
    PidNew = spawn(?MODULE, calculator, [A, B, N, Options, ParentPid, PreviousPid]),
    spawn_calculators_rec(Left, Right, N-1, Options, ParentPid, PidNew).

	    


%% @doc Adds A and B after receiving a carry in then messages the PreviousPid 
%% process with the carry out of the addition and ParentPid with the result
%% of the addition.

-spec calculator(A, B, N, Options, ParentPid, PreviousPid) -> ok when
      A :: integer(),
      B :: integer(),
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
    ParentPid ! {N, Result};

calculator(A, B, N, {Base, true, Sleep}, ParentPid, PreviousPid) ->
    PidZero = spawn(?MODULE, add, [self(), A, B, Base, 0, Sleep]),
    PidOne = spawn(?MODULE, add, [self(), A, B, Base, 1, Sleep]),
    receive
	{carry, CarryIn} ->
	    case CarryIn of 
		0 -> exit(PidOne, kill);
		1 -> exit(PidZero, kill)
	    end,
	    receive
		{CarryIn, Result} -> Result 
	    end
    end,
    [{CarryOut, _}, _] = Result,
    PreviousPid ! {carry, CarryOut},
    ParentPid ! {N, Result}.




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
    ParentPid ! {Carry, Result}.




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

    


