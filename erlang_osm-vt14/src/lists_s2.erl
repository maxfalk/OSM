-module(lists_s2).
-export([max/1, split/2, pmax/2]).

%% To use EUnit we must include this:
-include_lib("eunit/include/eunit.hrl").


%% @doc Find the max value in a list using a sequential, recursive
%% solution.
-spec max(List) -> integer() when List::list().

max([]) ->
    {undefined, empty_list};
max([N]) ->
    N;
max([H|T]) ->
    rmax(T, H).

%% A recursive helper function. 

rmax([], Max) ->
    Max; %% Recursive base case
rmax([H|T], Max) when H > Max ->
    rmax(T, H);
rmax([_H|T], Max) ->
    rmax(T, Max).


%% @doc Find the max value in List by spliting up List in sub lists of
%% size N and use concurrent procces to process each sublists.
-spec pmax(List, N) -> integer() when List::list(),
				      N::integer().

pmax(List, N) ->
    process_flag(trap_exit, true),
    Death = death:start(60),
    pmax(List, N, Death).

%% @doc Split a list L into lists of lengt N. 
-spec split(List, N) -> [list()] when List::list(),
				      N::integer().

%% NOTE: This may result in one list having fewer than N elemnts. 
%% Example: When splitting a list of length 5 into list of length 2 we
%% get two lists of lenght 2 and one list of length 1.
%% Fixed so it works 0 and less.
%% Fixed now always returns a list of list(s).


%% Can' split something in 0 parts?
split(L, N) when N < 1 ->
    [L];


%% Can we stop splitting?
split(L, N) when length(L) < N ->
    [L];

%% Do the splitting
split(L, N) ->
    split(L, N, []).

%% An auxiliary recursive split function
split(L, N, Lists) ->
    {L1, L2} = lists:split(N, L),
    if length(L2) > N ->
	    split(L2, N, [L1|Lists]);
       length(L2) =:= 0 ->
            [L1];
       true ->
	    [L1, L2|Lists]
    end.


pmax(List, N, Death) when length(List) > N ->
    Lists = split(List, N),
    CollectPID = self(),
    List_worker = [{spawn_link(fun() -> worker(L, CollectPID, Death) end),L} || L <- Lists],
    Worker_handler_pid = spawn_link(fun()-> worker_handler_init(List_worker,CollectPID,Death) end),
    Maxes = collect(length(Lists), [],Worker_handler_pid),
    pmax(Maxes, N, Death);
pmax(List, _, _) ->
    list:max(List). 
    
%% Find the max value in List and send result to Collect. 

worker(List, Collect, Death) ->
    death:gamble(Death),
    Collect ! list:max(List).


%% get


%% Search for the pid in List, return that list and the rest of the list in a tuple
get_list_for_pid(List,Pid)->
    get_list_for_pid(List,Pid,[]).

get_list_for_pid([],_Pid,Restlist) ->
    {[],Restlist};
get_list_for_pid([{Dead_pid,L}|T],Pid,Restlist) when Dead_pid == Pid->
    {L,Restlist ++ T};
get_list_for_pid([H|T],Pid,Restlist) ->
    get_list_for_pid(T,Pid,[H|Restlist]).

   
          


%% Start the worker that died again
restart_worker(List,CollectPID, Death)->
    spawn_link(fun() -> worker(List, CollectPID, Death) end).



worker_handler_init(List_worker,CollectPid,Death)->
    %%we want to trap chuld prcesses.
    process_flag(trap_exit, true),
    worker_handler(List_worker,CollectPid,Death).

%%Handels dead workers, gets them working again!
%%Will not bother collect if the worker keeps dying.

worker_handler(List_worker,CollectPid,Death)->
    
    receive
	{'EXIT', PID, random_death} ->
            %%Check for the list that the dead process owned.
            {Dropped_list,Rest} = get_list_for_pid(List_worker,PID),
            %%Start the process again, and update our worker list.
            New_worker_list = [{restart_worker(Dropped_list,CollectPid,Death),Dropped_list}|Rest],
            %%loop back
            worker_handler(New_worker_list,CollectPid,Death);
	{'EXIT', _PID, normal} ->
            worker_handler(List_worker,CollectPid,Death);
        {start,Startpid}->    
            %%Check for the list that the dead process owned.
            {Dropped_list,Rest} = get_list_for_pid(List_worker,Startpid),
            %%Start the process again, and update our worker list.
            New_worker_list = [{restart_worker(Dropped_list,CollectPid,Death),Dropped_list}|Rest],
            %%loop back
            worker_handler(New_worker_list,CollectPid,Death)
     
    end.
            

   
%% Wait for results from all workers. 
collect(N, Maxes,Worker_handler_pid) when length(Maxes) < N ->
    receive 
	{'EXIT', PID, random_death} ->
            %%Process was killed, Start it again
            Worker_handler_pid ! {start,PID},
            collect(N, Maxes,Worker_handler_pid);
	{'EXIT', _PID, normal} ->
	    collect(N, Maxes,Worker_handler_pid);
	Max -> 
	    collect(N, [Max|Maxes],Worker_handler_pid) 
    end;

collect(_N, Maxes,_Worker_handler_pid) ->
    %%io:format("Collected Maxes = ~w~n", [Maxes]),
    Maxes.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%			   EUnit Test Cases                                  %%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%% All functions with names ending wiht _test() or _test_() will be
%% called automatically by list:test()



split_test_() ->
    List = lists:seq(1,10), 
    
    [?_assertEqual([List], split(List, length(List)+1)),
     ?_assertEqual(5, length(split(List, 2))),
     ?_assertEqual(4, length(split(List, 3))),
     ?_assertEqual([1,3,3,3], lists:sort([length(L) || L <- split(List, 3)])),
     ?_assertEqual([2,4,4], lists:sort([length(L) || L <- split(List, 4)]))
    ].
    
split_merge_test_() ->
    List = lists:seq(1, 100),
    [?_assertMatch(List, lists:merge(split(List, N))) || N <- lists:seq(2, 23)].
    

max_empty_list_test() ->
    ?assertEqual({undefined, empty_list}, max([])).

max_test() ->
    ?assertEqual(42, max([3, 7,-9, 42, 11, 7])).

random_list(N) ->
    [random:uniform(N) || _ <- lists:seq(1, N)].

max_random_lists_test_() ->
    %% A list [1, 10, 100, .... ]
    Lengths = [trunc(math:pow(10, N)) || N <- lists:seq(0, 5)],
    
    %% A list of random lists of increasing lengths
    RandomLists = [random_list(Length) || Length <- Lengths],
    
    [?_assertEqual(lists:max(L), max(L)) || L <- RandomLists].

pmax_random_plist_test() ->
    N = 10000,
    L = random_list(N),
    ?assertEqual(lists:max(L), pmax(L, 10)).
    
