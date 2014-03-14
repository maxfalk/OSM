%% @doc Erlang mini project.
-module(add).
-export([start/3, start/4,result/4]).
%%import utils for utilit functions
-import(utils,[print_text/3,get_result/1,add_carry_first/2]).
-import(calc,[start_calc/4]).
%% To use EUnit we must include this.
-include_lib("eunit/include/eunit.hrl").


%% @doc Creates a new process and returns pid frm the created process.
-spec start(A,B,Base) -> ok when 
      A::integer(),
      B::integer(), 
      Base::integer().

start(A, B, Base) ->
    {Left, Right} = utils:pad_two(integer_to_list(A), integer_to_list(B), 0),
    Split = length(Left),
    {LeftSplit, RightSplit} = {utils:split(Left, Split), utils:split(Right, Split)},
    Result_pid = result(A,B,Split,self()),
    _First_spawn_pid = calc:start(Result_pid,LeftSplit,RightSplit,{Base,Split,false,false}),
    receive_result().
   


%% @doc
%%
%%
%%
-spec start(A,B,Base, Options) -> ok when 
      A::integer(),
      B::integer(), 
      Base::integer(),
      Option::atom() | tuple(),
      Options::[Option].

start(A,B,Base,Options) ->
    {Spawn,Sleep,Split} = get_options(Options),
    {Left, Right} = utils:pad_two(integer_to_list(A), integer_to_list(B), 0),
    {LeftSplit, RightSplit} = {utils:split(Left, Split), utils:split(Right, Split)},
    Result_pid = result(A,B,Split,self()),
    _First_spawn_pid = calc:start(Result_pid,LeftSplit,RightSplit,{Base,Split,Spawn,Sleep}),
    receive_result().
      

%%@doc get options from option list.
-spec get_options(List) -> tuple() when
      List :: list().

get_options(List)-> get_options_help(List,false,false,0).

get_options_help([],Spawn,Sleep,Split)-> {Spawn,Sleep,Split};
get_options_help([Option|T],Spawn,Sleep,Split)->
    case Option of
	{sleep,Sleep_value}->
	    get_options_help(T,Spawn,Sleep_value,Split);
	{spawn,Spawn_value}->
	    get_options_help(T,Spawn_value,Sleep,Split);
	{split,Split_value} ->
	    get_options_help(T,Spawn,Sleep,Split_value);
	true ->
	    io:format("felfelfelfelf")
    end.
    

%%@doc waits to receive the result of the addition
-spec receive_result()-> integer().
receive_result()->
    receive
	Result ->
	    Result
    end.


%%@doc Handels partial results, waits for every process to finish and adds them up to one result.
%%Base: the base for the possible carry in.
-spec result(A,B,Num_proc,Result_to_pid) -> integer() when
      A :: integer(),
      B :: integer(),
      Num_proc :: integer(),
      Result_to_pid :: pid().

result(A,B,Num_proc,Result_to_pid)->
    result_handler(A,B,[],Num_proc,Result_to_pid).

%%@doc Waits to receive results and a possible carry. 
%% Terminates after some process has asked for the result.
%%
%%[{[{Carry,Result},...], pos}, ..]
-spec result_handler(A,B,Sum_carry_pos_list,Num_processes,Result_to_pid) -> integer() when
      A :: integer(),
      B :: integer(),
      Sum_carry_pos_list :: list(),
      Num_processes :: integer(),
      Result_to_pid :: pid().



result_handler(A,B,Sum_carry_pos_list,0,Result_to_pid)->
    receive
	{carry,Carry_in} ->
	    Carry_added_result_list = add_carry_first({0,Carry_in},Sum_carry_pos_list),
	    Result_list = utils:get_result(Carry_added_result_list),
	    utils:print_text(A,B,Carry_added_result_list),
	    Result_to_pid ! convert_list_to_integer(Result_list)
    end;
result_handler(A,B,Sum_carry_pos_list,Num_processes,Result_to_pid)->
    receive
	({result,Result_carry_pos})->
	    result_handler(A,B,[Result_carry_pos|Sum_carry_pos_list],Num_processes-1,Result_to_pid);
        exit ->
            exit
    end.

    
%%@doc converts a string (list) of numbers to an integer 
%%
%%
%%
-spec convert_list_to_integer(List)-> integer() when
      List :: list().
convert_list_to_integer(List)-> 
    convert_list_to_integer(List,length(List)-1,0).

convert_list_to_integer([],_,Acc)-> round(Acc); 
convert_list_to_integer([H|T],Mult,Acc)-> 
    convert_list_to_integer(T,Mult-1,Acc+(H*math:pow(10,Mult))).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%                     Testcases. 
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%


%%start_test() ->
   %% ?assertMatch(true, Result_pid(start(10,10,10))).


convert_list_to_integer_test()->
    ?assert(convert_list_to_integer([1,2,3,4,5,6]) =:= 123456).
    
%%Test that the result call will give the right vaule
result_nocarry_test()->
    Pid = spawn(?MODULE,result,[2,1,1,self()]),
    Pid ! {result,{[{0,3}],0}},
    Pid ! {carry,0},
    receive
	Result -> Got_result = Result
	    
    end,
    ?assert(Got_result =:= 3).

result_carry_test()->
    Pid = spawn(?MODULE,result,[8,8,1,self()]),
    Pid ! {result,{[{1,6}],0}},
    Pid ! {carry,0},
    receive
	Result -> Got_result = Result
	    
    end,
    ?assert(Got_result =:= 16).

result_incarry_test()->
    Pid = spawn(?MODULE,result,[8,8,1,self()]),
    Pid ! {result,{[{1,6}],0}},
    Pid ! {carry,1},
    receive
	Result -> Got_result = Result
	    
    end,
    ?assert(Got_result =:= 16).
