

-module(worker).
-behaviour(gen_server).
-export([steps/1, calc_collatz_seq/2,get_all_tasks/1,get_all_results/1,get_last_result/1]).
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).
-record(worker_state, {tasks = [], results = []}).

calc_collatz_seq(N, ServerPid) ->
    gen_server:call(ServerPid, {task, N}). % send message to gen_server and wait for reply
    % ServerPid ! {{task, N}, self()}
    % receive
    % ....
    % end

get_all_tasks(ServerPid) ->
    gen_server:call(ServerPid, get_all_tasks).

get_all_results(ServerPid) ->
    gen_server:call(ServerPid, get_all_results).

get_last_result(ServerPid) ->
    gen_server:call(ServerPid, get_last_result).
%%calc_collatz_seq2(N, ReplyTo, ServerPid) -> %%just for study,not for this task
%%    gen_server:cast(ServerPid, {task, N, ReplyTo}). % send message to gen_server and don't wait for reply
    % ServerPid ! {task, N, ReplyTo}
    % caller don't enter receive routine

%--------------------------------Gen_server-----------------------------------------------------

init([]) -> {ok, #worker_state{}}.

handle_call({task, Task}, _From, #worker_state{tasks = Tasks, results = OldResults} = State) ->
       NewResult = steps(Task),
       State1 = State#worker_state{tasks = [Task | Tasks], results = [NewResult | OldResults]},
       {reply, NewResult, State1};

handle_call(get_all_tasks, _From, #worker_state{tasks = Tasks} = State) ->
       {reply, Tasks, State};

handle_call(get_all_results, _From, #worker_state{results = Results} = State) ->
       {reply, Results, State};

handle_call(get_last_result, _From, #worker_state{results = Results} = State) when Results =:= [] ->
       {reply, undefined, State};

handle_call(get_last_result, _From, #worker_state{results = [LastResult|_T]} = State) ->
       {reply, LastResult, State}.


%handle_cast({task, Task, ReplyTo}, State) ->
%    NewResult = steps(Task),
%    ReplyTo ! NewResult,
%    State1 = State#worker_state{tasks = [Task | Tasks], results = [NewResult | OldResults]},
%    {noreply, State1}.

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

