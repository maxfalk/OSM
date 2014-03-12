
-module(calc). 

-export([]).

%% To use EUnit we must include this.
-include_lib("eunit/include/eunit.hrl").

-compile(export_all). 

%% CALC

-spec calculator(A,B,N) -> ok when 
      A::list(),
      B::list(),
      N::integer().       

calculator(A,B,N) ->
    %% A = [1,2,6]
    %% B = [1,2,6]
    %% 6+6 = 
    LenA = length(A),
    LenB = length(B),
    addition(lists:nth(LenA,A),lists:nth(LenB,B),Cin)


-spec addition(A,B, Cin) -> ok when
      A::integer(),
      B::integer(),
      Cin::integer().

addition(A,B,Cin) ->
    N = 10,
    Result = ((A+B) rem 10),
    if ((A+B) > N) ->
	    Cin = 1,
	
