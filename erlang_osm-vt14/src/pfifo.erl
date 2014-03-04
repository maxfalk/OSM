-module(pfifo). 

-export([new/0, size/1, push/2, pop/1, empty/1]).

%% To use EUnit we must include this:
-include_lib("eunit/include/eunit.hrl").

%% A common pattern is Erlang is to provide services as separate
%% processes. In this module we spawn a new process to keep the state
%% of a FIFO queue. 


%% @doc Creates a new fifo buffer.
-opaque pfifo()::pid().
-spec new() -> pfifo().

new() ->
    spawn(fun() -> loop(fifo:new()) end).

%% This is the process loop.

loop(Fifo) ->
    receive 
	{size, PID} ->
	    PID ! {size, fifo:size(Fifo)},
	    loop(Fifo); 
	{empty, PID} ->
	    PID ! fifo:empty(Fifo),
	    loop(Fifo);
	{pop, PID} ->
	    {Newvalue, Newfifo} = fpop(Fifo),
	    PID ! Newvalue,
	    loop(Newfifo);
	{push,Value, _} ->
	   loop(fpush(Fifo,Value))
    end.


%%  By hiding the message passing protocol inside a functional
%%  interface the user of the FIFO doesn't need to know whether or not
%%  the FIFO is implemented as a separate process.

%% @doc Returns the number of elements in Fifo. 
-spec size(Fifo) -> integer() when Fifo::pfifo().

size(Fifo) ->
    Fifo ! {size, self()},
    receive 
	{size, Size} ->
	    Size
    end.

%% @doc Returns true if Fifo is empty, otherwise returns false. 
-spec empty(Fifo) -> true|false when Fifo::pfifo().

empty(Fifo) ->
    Fifo ! {empty, self()},
    receive 
	true ->
	    true;
	false  ->
	    false
    end.

%% @doc Pushes an item on the stack
-spec fpush({fifo,list(),list()}, atom()) -> {fifo,list(),list()}.

%% To make it fast to push new values, add a new value to the head of
%% In.

fpush({fifo, In, Out}, X) -> 
    {fifo,[X|In],Out}.

%% @doc Pops an item from the stack.
%% @throws 'empty fifo'
-spec fpop({fifo,list(),list()}) -> {fifo,list(),list()}.

%% pop should return {Value, NewFifo}

fpop({fifo, [], []}) -> 
    {{error,empty_fifo},{fifo, [], []}};

%% To make pop fast we want to pop of the head of the Out list.
    
fpop({fifo, In, [H|T]}) -> 
  {H, {fifo, In, T}};

%% When Out is empty, we must take a performance penalty. Use the
%% reverse of In s the new Out and an empty lists as the new In, then
%% pop as usual.

fpop({fifo, In, []}) ->
    fpop({fifo,[],lists:reverse(In)}).


%% @doc sends signal to fifo, to pop.
%%
%%
-spec pop({fifo,list(),list()}) -> {fifo,list(),list()}.
pop(Fifo)->
    Fifo ! {pop,self()},
    receive	
	Value ->
	    Value
    end.
    


%%@doc push one item to the fifo.
%%
%%
-spec push(pid(),atom()) -> ok.
push(Fifo,Value)->
    {push,Value,_} = Fifo ! {push,Value,self()},
    ok.


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%                         EUnit Test Cases                                  %%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%% EUnit adds the fifo:test() function to this module. 

%% All functions with names ending wiht _test() or _test_() will be
%% called automatically by pfifo:test()

start_test_() ->
    [?_assertMatch(true, is_pid(new())),
     ?_assertMatch(0, pfifo:size(new())),
     ?_assertMatch(true, empty(new())),
     ?_assertMatch({error, empty_fifo}, pop(new()))].

empty_test() ->
    F =  new(),
    ?assertMatch(true, empty(F)),
    push(F, foo),
    ?assertMatch(false, empty(F)),
    pop(F),
    ?assertMatch(true, empty(F)).
		  
push_pop_test() ->
    F = new(),
    push(F, foo),
    push(F, bar),
    push(F, luz),
    ?assertMatch(false, empty(F)),
    ?assertMatch(foo, pop(F)),
    ?assertMatch(bar, pop(F)),
    ?assertMatch(luz, pop(F)),
    ?assertMatch({error, empty_fifo}, pop(F)).

large_push_pop_test() ->
    F = new(),
    List = lists:seq(1, 999),
    [push(F, Value) || Value <- List],
    ?assertEqual([pop(F) || _ <- List], List).
   
		 
    
    
    
    
    
    
    
