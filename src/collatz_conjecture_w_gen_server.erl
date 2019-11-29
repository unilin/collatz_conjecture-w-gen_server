-module(collatz_conjecture_w_gen_server).
-ifdef(TEST). % if define test, then compile all which export.
    -compile(export_all).
-endif.
-export([calculate/2]).


-spec   calculate(Num1, Num2) ->  Result when %test calculate
                 Num1         ::  pos_integer(),
                 Num2         ::  pos_integer(),
                 Result       ::  non_neg_integer().

calculate(From, To) -> calculate(From, To, [worker:for_calculate()|| _X <- lists:seq(1, 5)], 0).


calculate(From, To, Workers, LongestStep) when (From > To) andalso length(Workers) =:= 5 -> LongestStep; %when From>To,
%return to the LongsStep.
calculate(From, To, Workers, LongestStep) when From > To -> %when From>To and worker "not full"
    receive
        {WorkerPid, Result} ->
            calculate(From, To, [WorkerPid | Workers], max(Result, LongestStep))
    end;

calculate(From, To, [H|T], LongestStep) -> % put 5 pid to the worker list,return the LongestStep
          %H ! {self(), From},
          worker:calc_collatz_seq2(From, self(), H),
          calculate(From + 1, To, T, LongestStep);

calculate(From, To, Workers, LongestStep) -> %get result, put pid back to worker and compare result and LongestStep
    receive
        {WorkerPid, Result} ->
            calculate(From, To, [WorkerPid | Workers], max(Result, LongestStep))
    end.

