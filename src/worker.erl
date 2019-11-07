

-module(worker).
-behaviour(gen_server).
-export([steps/1]).
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).
-record(worker_state, {tasks = [], results = []}).

%--------------------------------Gen_server-----------------------------------------------------

init([]) -> {ok, #worker_state{}}.

handle_call(Task, From, #worker_state{tasks = Tasks, results = Results} = State) ->
       Reply =
             Result = steps(Task),
             From ! {self(), Result},
       State1 = State#worker_state{tasks = [Task | Tasks], results = [Result | Results]},
       {reply, Reply, State1};

handle_call(get_all_tasks, From, #worker_state{tasks = Tasks} = State) ->
       Reply =
          From ! Tasks,
       {reply, Reply, State};

handle_call(get_all_results, From, #worker_state{results = Results} = State) ->
       Reply =
           From ! Results,
       {reply, Reply, State};

handle_call(get_last_result, From, #worker_state{results = Results} = State) when Results =:= [] ->
       Reply =
           From ! undefined,
       {reply, Reply, State};

handle_call(get_last_result, From, #worker_state{results = Results} = State) ->
       Reply =
           From ! hd(Results),
       {reply, Reply, State};

handle_call(make_me_sia, _From, State) ->
       Reply =
           ok,
      {reply, Reply, State}.

handle_cast(_Msg, State) -> {noreply, State}.

handle_info(_Info, State) -> {noreply, State}.

terminate(_Reason, _State) -> ok.

code_change(_OldVsn, State, _Extra) -> {ok, State}.

%----------------------------Private Function------------------------------------------------------

steps(N) when N =< 0 -> {error, "Only positive numbers are available"}; %when N is negtive integer
steps(N) -> steps_helper(N, 0).  %when N is positive integer,return to steps_helper.

-spec steps_helper(Num1, Num2)  ->  Result when %test steps_helper.
                 Num1           ::  pos_integer(),
                 Num2           ::  non_neg_integer(),
                 Result         ::  pos_integer().

steps_helper(1, Steps) -> Steps; %when N=1, get result "Steps".
steps_helper(N, Steps) when (N rem 2 =:= 0) -> steps_helper(N div 2, Steps + 1);% when N rem 2 =:=0.
steps_helper(N, Steps) -> steps_helper(3 * N + 1, Steps + 1). %when N cannot be divisible by 2.

