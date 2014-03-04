
-module(test).
-export([double/1,four/0]).

double(A)->
    2*A.

four()->
    double(2).
