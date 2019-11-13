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
    ?assertEqual(6, Result).
