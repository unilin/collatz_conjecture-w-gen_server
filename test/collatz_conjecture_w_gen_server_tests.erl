-module(collatz_conjecture_w_gen_server_tests).
-include_lib("eunit/include/eunit.hrl").

calculate_test() -> %test calculate function
    ?assertEqual(7, collatz_conjecture_w_gen_server:calculate(1, 3)),
    ?assertEqual(16, collatz_conjecture_w_gen_server:calculate(1, 7)),
    ?assertEqual(20, collatz_conjecture_w_gen_server:calculate(10, 20)),
    ?assertEqual(20, collatz_conjecture_w_gen_server:calculate(15, 18)).

