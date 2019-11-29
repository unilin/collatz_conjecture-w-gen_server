-module(worker_tests).
-include_lib("eunit/include/eunit.hrl").


steps_test() -> %test steps.
    ?assertEqual(18, worker:steps(30)),
    ?assertEqual(6, worker:steps(10)),
    ?assertEqual({error,"Only positive numbers are available"}, worker:steps(-3)),
    ?assertEqual({error, "Only positive numbers are available"}, worker:steps(0)).

gen_server_test() ->
    {ok, Pid} = gen_server:start_link(worker, [], []),
    ?assert(true, is_process_alive(Pid)),
    Result = worker:calc_collatz_seq(10, Pid),
    ?assertEqual(6, Result),
    Result1 = worker:calc_collatz_seq(2, Pid),
    ?assertEqual(1, Result1),
    Result2 = worker:calc_collatz_seq(30, Pid),
    ?assertEqual(18, Result2),

    case worker:get_all_tasks(Pid) of
        All_tasks when is_list(All_tasks) ->
            ?assertEqual([30,2,10], All_tasks);
        _Another ->
           ?assert(false)
    end,

    case worker:get_all_results(Pid) of
        All_results when is_list(All_results) ->
            ?assertEqual([18,1,6], All_results);
        _Another1 ->
            ?assert(false)
    end,

    case worker:get_last_result(Pid) of
        [] ->
            ?assert(undefined);
        LastResult ->
            ?assertEqual(18, LastResult)
    end,

    Result3 = worker:calc_collatz_seq2(10, self(), Pid),
    ?assert(true, Result3).

