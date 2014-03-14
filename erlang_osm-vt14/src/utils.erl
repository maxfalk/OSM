%% @author Karl Marklund <karl.marklund@it.uu.se>

%% @doc A small collection of utility functions. 


-module(utils). 

-export([seqs/1, filter/2, split/2, pad/3, pad_two/3, print_text/3, get_result/1]).
-export_type([result_list/0]).
-opaque result_list() :: [{[{integer(),integer()}, ...],integer()}, ...]. 

%% To use EUnit we must include this.
-include_lib("eunit/include/eunit.hrl").

-compile(export_all). 


%% @doc Generates a list of lists of increasing sequences of integers
%% starting with the empty list and ending with [1,2, ..., N].
%% === Example ===
%% <div class="example">```
%% > utils:seqs(5).
%% [[],[1],[1,2],[1,2,3],[1,2,3,4],[1,2,3,4,5]]'''
%% </div>
-spec seqs(N::integer()) -> [[integer()]].

seqs(N) ->
    %% NOTE: Simply using a list comprehension such as [[]] ++
    %% [lists:seq(1,M) || M <- lists:seq(1,N)] will be quite slow
    %% since each sequence is generated from scratch. Hence, lets
    %% re-use the last sequnece and add a new element when
    %% constructing the next sequence.
    
    F = fun(X,[H|T]) -> [[X|H],H|T] end,
    lists:foldl(F, [[]], lists:seq(1,N)),
    lists:reverse([lists:reverse(L) || L <- lists:foldl(F, [[]], lists:seq(1,N))]).

		
%% @doc Each list in List2 contains the elements Elem in List1 for
%% which one of the Pred(Elem) returns true. The order of the lists in
%% List2 is the same as the order of the predicates. In each list in
%% List2, the relative order of the elements are the same as in the
%% original List1. 
%% 
%% === Example ===
%% <div class="example">```
%% 1> L = [1,2,3,4,5,6,7,8,9,10].
%% [1,2,3,4,5,6,7,8,9,10]
%% 2> P1 = fun(X) -> X rem 2 == 1 end.
%% #Fun<erl_eval.6.111823515>  
%% 3> P2 = fun(X) -> not P1(X) end. 
%% #Fun<erl_eval.6.111823515>
%% 4> P3 = fun(X) -> X > 3 andalso X < 7 end. 
%% #Fun<erl_eval.6.111823515>
%% 5> utils:filter([P1,P2,P3], L).
%% [[1,3,5,7,9],[2,4,6,8,10],[4,5,6]]'''
%% </div>
-spec filter(Preds, List1) -> List2 when
      Preds :: [Pred],
      Pred :: fun((Elem :: T) -> boolean()),
      List1 :: [T],
      List2 :: [[T]],
      T :: term().

filter(Predicates, List) ->
    Collect = self(),
    [spawn(fun() -> Collect!{I,lists:filter(P,List)} end) ||
	{I, P} <- lists:zip(lists:seq(1, length(Predicates)), Predicates)],
    
    filter_collect(length(Predicates), []).

filter_collect(0,R) ->
    [L || {_,L} <- lists:sort(R)];
filter_collect(N,R) ->
    receive
	{I, L} -> filter_collect(N-1, [{I,L}|R])
    end.



lqr(L, N) ->
    Len = length(L),

    %% Quotient
    Q = Len div N, 
    
    %% Reminder
    R = Len rem N, 
    
    {Len, Q, R}. 





%% @doc Split List into N Lists such that all Lists have approximately the same number of elements. 
%% 
%% Let Len = length(List), Q = Len div N and R = Len rem N. 
%% 
%% If R = 0, then all of the lists in Lists will be of length Q. 
%% 
%% If R =/= 0, then R of the lists in Lists will have
%% lenght Q + 1. 
%% 
%% === Example ===
%% 
%% <div class="example">```
%% 1> L = [1,2,3,4,5,6,7,8,9,10].
%% [1,2,3,4,5,6,7,8,9,10]
%% 2> utils:split(L, 4).
%% [[1,2],[3,4],[5,6,7],[8,9,10]]
%% 3> lists:concat(utils:split(L,3)).
%% [1,2,3,4,5,6,7,8,9,10]'''
%% </div>
-spec split(List, N) -> Lists when
      List :: [T],
      Lists :: [List],
      T :: term(),
      N :: integer().

split(List, N) ->
    {_, Q, R} = lqr(List, N),
    split(List, Q, R).

split([], _, _) ->
    [];
split(List, Q, 0) ->
    {Left, Right} = split_acc([], List, Q), 
    [Left | split(Right, Q, 0)];
split(List, Q, R) ->
    {Left, Right} = split_acc([], List, Q+1), 
    [Left | split(Right, Q, R-1)].

split_acc(Acc, List, 0) ->
    {lists:reverse(Acc), List};
split_acc(Acc, [H|List], N) ->    
    split_acc([H|Acc], List, N-1).






%% @doc Add N number of padding elements to the beginning of a list.
%% 
%% === Example ===
%% 
%% <div class="example">```
%% 1> utils:pad([1,2,3], 3, 0).
%% [0,0,0,1,2,3]'''
%% </div>

-spec pad(List1, N, P) -> List2 when
      List1 :: list(),
      N :: integer(),
      P :: term(),
      List2 :: list().

pad(List, N, _) when N =< 0 ->
    List;
pad(List, N, P) when N > 0 ->
    pad([P|List], N-1, P).





%% @doc Pads the shortest of two lists with P so that the lengths become equal.
%% 
%% === Example ===
%% 
%% <div class="example">```
%% 1> utils:pad([1,2,3], [3], 0).
%% {[1,2,3], [0,0,3]}'''
%% </div>

-spec pad_two(List1, List2, P) -> {List3, List4} when
      P :: term(),
      List1 :: list(),
      List2 :: list(),
      List3 :: list(),
      List4 :: list().

pad_two(A, B, P) ->
    LenA = length(A),
    LenB = length(B),
    Pad = LenA - LenB,
    if
	Pad < 0 -> NewA = pad(A, -Pad, P), {NewA, B}; 
	Pad > 0 -> {A, pad(B, Pad, P)};
	Pad =:= 0 -> {A, B}
    end.
	    



%%@doc get inner list length of a list 
%%[{List,_}|_] a list containing tuples with the first element as a list. 
-spec inner_list_length(List) -> integer() when
      List :: result_list().

inner_list_length([]) -> 0;
inner_list_length([{List,_}|_])-> length(List).
    

%%@doc prints text representing the addition made.
%%Example
%%        00 
%%   -------
%%        12
%%        13
%%  +-------
%%        25
-spec print_text(A,B,Sum_carry_list)-> ok when
      A :: integer(),
      B :: integer(),
      Sum_carry_list :: result_list().
	   
print_text(_,_,[])-> ok;
print_text(A,B,Sum_carry_pos_list)->
    Sorted_list = sort(Sum_carry_pos_list),
    Length = (inner_list_length(Sorted_list)*length(Sorted_list)),
    fun()->
	    print_list(get_carrys(Sorted_list)), %%print carrys in right order
	    print_line(Length), %% print line of right length
	    print_blankspace(1),
	    print_number(A), %% print Number A in right order
	    print_blankspace(1),
	    print_number(B), %% print Number B in right order
	    print_plus_line(Length), %% print line with + sign in the front
	    print_blankspace(1),
	    print_list(get_result(Sorted_list))
	    
    end. %% print the result

%%@doc prints a line and a plus sign "+--------"
%%
%%
%%
-spec print_plus_line(N::integer())-> ok.

print_plus_line(N)->
    io:format("+",[]),
    print_line(N-1).


%%@doc sort a sum_carry_pos_list with quick sort, with
%%biggest part first.
%%
%%
-spec sort(List) -> list() when
      List :: result_list().

sort([{Result,Pivot}|T]) ->
    sort([{XResult,X} || {XResult,X} <- T, X > Pivot]) ++
    [{Result,Pivot}] ++
    sort([{XResult,X} || {XResult,X} <- T, X =< Pivot]);
sort([]) -> [].

%%@doc prints all elements of a list, then prints a new line.
-spec print_list(List::list())-> ok.

print_list(List) when length(List) > 0 ->
    [io:format("~p",[H]) || H <- List],
    io:format("~n").
    
%%@doc Prints a line "------" with n "-", then a newline.
-spec print_line(N::integer())-> ok.


print_line(N) when N >=0 ->
    [io:format("-") || _ <- lists:seq(0,N)],
    io:format("~n");
print_line(_) -> ok.

%%@doc Prints the Number, then a newline.
-spec print_number(Number::integer())-> ok.

print_number(Number)->
    io:format("~p~n",[Number]).

%%@doc get carrys from our list 
%%
%%
%%
-spec get_carrys(List)-> list() when
      List :: result_list().

get_carrys([])-> [];
get_carrys([{List,_}|T])->
   [Carry || {Carry,_} <- List] ++ get_carrys(T).

%%@doc get all the result numbers, aswell as the highest carry if it is one
%% as that carry will be needed in the result.
-spec get_result(List)-> list() when
      List :: result_list().

get_result([]) -> [];
get_result([{[{Carry,_}],_}|_] = List) when Carry =:= 1->
    [Carry|get_result_help(List)];
get_result(List) ->
    get_result_help(List).

get_result_help([]) -> [];
get_result_help([{List,_}|T])->
   [Number || {_,Number} <- List] ++ get_result_help(T).
    
    
%%@doc print N blank spaces.
-spec print_blankspace(N)-> ok when
      N :: integer().

print_blankspace(N)->
    [io:format(" ") || _ <- lists:seq(1,N)].

%%@doc add a new element with the highest position in a result_list,
%% if the carry is 1 else just return the list unmodified.
-spec add_carry_first(Element,List) -> result_list() when
      Element :: tuple(),
      List :: result_list().

add_carry_first({0,1} = Element, List) ->
    add_first_element(Element,List);
add_carry_first(_,List) ->
    List.



%%@doc add a new element with the highest position in a result_list.
-spec add_first_element(Element,List) -> result_list() when
      Element :: tuple(),
      List :: result_list().

add_first_element(Element,List)->
    [{[Element],length(List)}| List].
    


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%                                                                          %%
%%			   EUnit Test Cases                                 %%
%%                                                                          %%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%


add_carry_first_test()->
    ?assert(add_carry_first({1,0},[{[{1,2}],0}]) =:= [{[{1,2}],0}]),
    ?assert(add_carry_first({0,1},[{[{1,2}],0}]) =:= [{[{0,1}],1},{[{1,2}],0}]).

add_first_element_test()->
    ?assert(add_first_element({1,1},[{[{1,2}],0}]) =:= [{[{1,1}],1},{[{1,2}],0}]),
    ?assert(add_first_element({0,1},[{[{1,2}],0}]) =:= [{[{0,1}],1},{[{1,2}],0}]).
    

get_result_test()->
    ?assert(get_result([{[{1,2}],0}]) =:= [1,2]),
    ?assert(get_result([{[{0,0},{1,1}],1},{[{0,1},{1,0}],1}]) =:=[0,1,1,0]).

get_carrys_test()->
    ?assert(get_carrys([{[{1,2}],0}]) =:= [1]),
    ?assert(get_carrys([{[{0,0},{1,1}],1},{[{0,1},{1,0}],1}]) =:=[0,1,0,1]).

%%Just check that it doesn't crash, tests all print functions
print_text_test()->
    print_text(10,10,[{[0,2],1},{[0,2],1}]),
    print_text(12345,12345,
	       [{[{0,4}],3},{[{0,2}],4},{[{0,9}],1},{[{0,6}],2},{[{1,0}],0}]).


sort_test()->
    ?assert(sort([{[{0,0}],1},{[{0,1}],0},{[{0,1}],6}]) =:= 
	   [{[{0,1}],6},{[{0,0}],1},{[{0,1}],0}]).
    
inner_list_lengt_test()->
    ?assert(inner_list_length([{[{0,0},{0,0},{0,0}],1},{[{0,7}],0}]) =:= 3),
    ?assert(inner_list_length([{[{0,7}],0}]) =:= 1).

seqs_length_test_() ->
    %% The list [[], [1], [1,2], ..., [1,2, ..., N]] will allways have
    %% length N+1.

    [?_assertEqual(N+1, length(seqs(N))) || N <- lists:seq(1, 55)].

seqs_test_() ->
    %% A small collection of expected results {N, seqs(N)}.
    
    Data = [{0, [[]]}, {1, [[], [1]]}, {2, [[], [1], [1,2]]}, 
	    {7, [[],
		 [1],
		 [1,2],
		 [1,2,3],
		 [1,2,3,4],
		 [1,2,3,4,5],
		 [1,2,3,4,5,6],
		 [1,2,3,4,5,6,7]]}
	   ],
    
    [?_assertEqual(L, seqs(N)) || {N, L} <- Data].
    
filter_test_() ->
    [?_assertEqual([], filter([], L)) || L <- seqs(10)].
    
filter_true_false_test_() ->
    P1 = fun(_) -> false end,
    P2 = fun(_) -> true end,
    P3 = fun(X) -> X rem 2 == 0 end,
    
    Expected = fun(L) -> [lists:filter(P,L) || P <- [P1,P2,P3]] end,

    [?_assertEqual(Expected(L), filter([P1,P2,P3], L) ) || L <- seqs(10) ].
				       
filter_test() ->
    L = lists:seq(1,10),

    P1 = fun(X) -> X rem 2 == 0 end,
    P2 = fun(X) -> X rem 2 == 1 end,
    P3 = fun(X) -> X > 3 end,

    %%E = [[2,4,6,8,10],[1,3,5,7,9],[4,5,6,7,8,9,10]],
    E = [lists:filter(P,L) || P <- [P1,P2,P3]],
    
    ?assertEqual(E, filter([P1,P2,P3], L)).
    
split_concat_test_() ->
    %% Make sure the result of concatenating the sublists equals the
    %% original list.
    
    L = lists:seq(1,99),
    [?_assertEqual(L, lists:concat(split(L,N))) || N <- lists:seq(1,133)].

split_n_test_() ->
    %% Make sure the correct number of sublists are generated. 
    
    M = 99,
    L = lists:seq(1,M),
    Num_of_lists = fun(List, N) when N =< length(List) ->
			   N;
		      (List, _) ->
			   length(List)
		   end,
    [?_assertEqual(Num_of_lists(L,N), length(split(L,N))) || N <- L].    


expected_stat(L, N) when N =< length(L) ->
    %% When spliting a list L into N sublists, we know there will only by two possible
    %% lengths of the sublists.

    
    %% Quotient and reminder when dividing length of L with N. 
    {_, Q, R} = lqr(L, N),

    %% There will allways be R sublists of length Q+1 and N-R sublists
    %% of length Q.
    
    {{R, Q+1}, {N-R, Q}};

expected_stat(L, _N) ->
    %% N greater than the length of L, hence all sublists will have
    %% length 1.

    {{length(L), 1}, {0,0}}.

stat(N, M, LL) ->
    %% Return a tuple {{Num_N, N}, {Num_M, M}} where Num_N is the
    %% number of lists of length N in LL and Num_M is the number of
    %% lists of length M in LL.
    
    S = filter([fun(X) -> X == N end, fun(X) -> X == M end], [length(L) || L <- LL]),

    [Num_N, Num_M] = [length(L) || L <- S],
    
    {{Num_N, N}, {Num_M, M}}.

split_stat_test_() ->
    %% Assure the list of sublists contains the correct number of
    %% lists of the two expected lengths.
	
    Assert = fun(L,N) ->
		     {_, Q, _} = lqr(L,N), 
		     ?_assertEqual(expected_stat(L,N), stat(Q+1, Q, split(L,N))) 
	     end,
	
    %% Generators can depend on other generator expressions, here N
    %% depends on the length of L.
    
    [Assert(L,N) ||  L <- seqs(33), N <- lists:seq(1,length(L)+5)].
    
%% Assure that we add N numbers of padding elements to the beginning of a list
%% A new list with the elements added.
pad_test()->
    ?assert(pad([0,1,2], 2, 5) =:= [5,5,0,1,2]),
    ?assert(pad([4,5,6], 0, 3) =:= [4,5,6]),
    ?assert(pad([1,2,3], -3, 1) =:= [1,2,3]).




pad_two_test() ->
    ?assert(pad_two([1,2,3], [1,2,3], 0) =:= {[1,2,3], [1,2,3]}),
    ?assert(pad_two([1,2,3], [1], 0) =:= {[1,2,3], [0,0,1]}),
    ?assert(pad_two([1,2,3], [1,2,3,4,5], 0) =:= {[0,0,1,2,3], [1,2,3,4,5]}). 
