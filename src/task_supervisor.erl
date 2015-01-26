-module(task_supervisor).

-export([start_link/0, start_link/1]).
-export([async/2, async/4, start_child/2, start_child/4]).

-define(CHILD(I, Type, Restart, Shutdown), {I, {I, start_link, []}, Restart, Shutdown, Type, [I]}).

start_link() ->
    start_link([]).

start_link(Opts) ->
    Restart = proplists:get_value(restart, Opts, temporary),
    Shutdown = proplists:get_value(shutdown, Opts, 5000),
    Children = [
        ?CHILD(task_supervised, worker, Restart, Shutdown)
    ],
    supervisor:start_link({local, ?MODULE}, ?MODULE, []).

init([]) ->
    {ok, { {simple_one_for_one, 5, 10}, []} }.

async(Sup, Fun) ->
    async(Sup, erlang, apply, [Fun, []]).

async(Sup, M, F, A) ->
    Args = [self(), task:get_info(self()), {M,F,A}],
    {ok, Pid} = supervisor:start_child(Sup, Args),
    Ref = erlang:monitor(Pid),
    Pid ! {self(), Ref},
    {Pid, Ref}.

start_child(Sup, Fun) ->
    start_child(Sup, erlang, apply, [Fun, []]).

start_child(Sup, M, F, A) ->
    supervisor:start_child(Sup, [task:get_info(self()), {M,F,A}]).
