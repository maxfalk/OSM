
-module(calc). 

-export([start/4]).

%% To use EUnit we must include this.
-include_lib("eunit/include/eunit.hrl").

-compile(export_all). 

%% CALC


%% @doc Splits A and B into Split number of similar length lists and 
%% spawns the same number of processes that will add each corresponding
%% sublist of A and B in parts.
%%
%% Each subprocess will message the ParentPid process with {N, Result},
%% where N is its sublist number and Result is the sum of the addition 
%% including carry bits.

-spec start(ParentPid, A, B, Options) -> ok when
      ParentPid :: pid(),
      A :: integer(),
      B :: integer(),
      Min :: integer(),
      Max :: integer(),
      Base :: integer(),
      Split :: integer(),
      Spawn :: boolean(),
      Sleep :: false | {Min, Max},
      Options :: {Base, Split, Spawn, Sleep}.


start(ParentPid, A, B, {Base, Split, Spawn, Sleep}) ->
    {Left, Right} = utils:pad_two(integer_to_list(A), integer_to_list(B), 0),
    {LeftSplit, RightSplit} = {utils:split(Left, Split), utils:split(Right, Split)},
    spawn_calculators(LeftSplit, RightSplit, Split, {Base, Spawn, Sleep}, ParentPid).




%% @doc Spawns N number of processes that will add each corresponding
%% sublist of A and B in parts
%%
%% Each subprocess will message the ParentPid process with {N, Result},
%% where N is its sublist number and Result is the sum of the addition 
%% including carry bits.

-spec spawn_calculators(Left, Right, N, Options, ParentPid) -> ok when
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


calculator(A, B, N, {Base, false, false}, ParentPid, PreviousPid) ->
    receive 
	{carry, CarryIn} -> Result = add(A, B, Base, CarryIn)
    end,
    [{CarryOut, _} | _] = Result,
    PreviousPid ! {carry, CarryOut}, 
    ParentPid ! {N, Result};

calculator(A, B, N, {Base, true, false}, ParentPid, PreviousPid) ->
    spawn(?MODULE, add, [self(), A, B, Base, 0]),
    spawn(?MODULE, add, [self(), A, B, Base, 1]),
    receive
	{carry, Carry} ->
	    receive
		{Carry, Result} -> [{CarryOut, _} | _] = Result
	    end
	    %% Kill the other process
    end,
    PreviousPid ! {carry, CarryOut},
    ParentPid ! {N, Result}.




%% @doc Adds List1, List2 and the carry in C in base Base and messages
%% the ParentPid process with the result.

-spec add_proc(ParentPid, List1, List2, Base, C) -> {C, List3} when
      ParentPid :: pid(),
      N :: integer(),
      T :: {N, N},
      C :: integer(),
      List1 :: [N],
      List2 :: [N],
      Base :: integer(),
      List3 :: [T].


add_proc(ParentPid, A, B, Base, Carry) ->
    Result = add(A, B, Base, Carry),
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

-spec add(List1, List2, Base, C) -> List3 when
      N :: integer(),
      T :: {N, N},
      C :: integer(),
      List1 :: [N],
      List2 :: [N],
      Base :: integer(),
      List3 :: [T].

add([A | []], [B | []], Base, C) when Base >= 1, A < Base, B < Base, C =< 1, C >= 0 ->
    Sum = A + B + C,
    Quot = Sum div Base,
    Rem = Sum rem Base,
    [{Quot, Rem}];

add([A | Left], [B | Right], Base, C) when Base >= 1, A < Base, B < Base ->
    [{Carry, Result} | List] = add(Left, Right, Base, C),
    Sum = A + B + Carry,
    Quot = Sum div Base,
    Rem = Sum rem Base,
    [{Quot, Rem} | [{Carry, Result} | List]].


    


