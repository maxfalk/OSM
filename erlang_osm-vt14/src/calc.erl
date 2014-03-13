
-module(calc). 

-export([add/4]).

%% To use EUnit we must include this.
-include_lib("eunit/include/eunit.hrl").

-compile(export_all). 

%% CALC

-spec start(PID, A, B, Options) -> ok when
      PID :: pid(),
      A :: integer(),
      B :: integer(),
      Min :: integer(),
      Max :: integer(),
      Base :: integer(),
      Split :: integer(),
      Spawn :: boolean(),
      Sleep :: false | {Min, Max},
      Options :: {Base, Split, Spawn, Sleep}.


start(PID, A, B, {Base, Split, Spawn, Sleep}) ->
    {Left, Right} = utils:pad(int_to_list(A), int_to_list(B), 0),
    {LeftSplit, RightSplit} = {utils:split(Left, Split), utils:split(Right, Split)},
    Processes = spawn_calculators(LeftSplit, RightSplit, Split, {Base, Spawn, Sleep}, todo),
    .
    


spawn_calculators(Left, Right, N, Options, ParentPid) ->
    spawn_calculators(Left, Right, N, Options, ParentPid, ParentPid).
    
spawn_calculators([], [], 0, Options, ParentPid, PreviousPid) ->
    [].

spawn_calculators([A|Left], [B|Right], N, Options, ParentPid, PreviousPid) ->
    PidNew = spawn(?Module, calculator, [A, B, N, Options, ParentPid, PreviousPid]),
    [PidNew | spawn_calculators(Left, Right, N-1, Options, ParentPid, PidNew)].
	    



-spec calculator(A, B, N, Options, ParentPid, PreviousPid) -> ok when
      A :: integer(),
      B :: integer(),
      N :: integer(),
      Min :: integer(),
      Max :: integer(),
      Base :: integer(),
      Spawn :: boolean(),
      Sleep :: false | {Min, Max},
      Options :: {Base, Spawn, Sleep}.




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


    


