-module(task_supervised).
-export([
    start/2,
    start_link/2,
    start_link/3,
    async/3,
    reply/3,
    noreply/2
]).

start(Info, Fun) ->

    {ok, proc_lib:spawn(?MODULE, noreply, [Info, Fun])}.

start_link(Info, Fun) ->

    {ok, proc_lib:spawn_link(?MODULE, noreply, [Info, Fun])}.

start_link(TaskOwner, Info, Fun) ->

        {ok, proc_lib:spawn_link(?MODULE, reply, [TaskOwner, Info, Fun])}.

-spec async(pid(),
            {node(), pid() | atom()},
            {atom(), atom(), [term()]}) ->
                term().
async(TaskOwner, TaskOwnerInfo, MFA) ->
    initial_call(MFA),
        Ref =
        receive
            {TaskOwner, Ref1} ->

                Ref1
        after
            5000 ->
		 exit(timeout)
        end,
        erlang:send(TaskOwner, {Ref, do_apply(TaskOwnerInfo, MFA)}).

-spec reply(pid(),
               {node(), pid() | atom()},
               {atom(), atom(), [term()]}) ->
                      term().
reply(TaskOwner, TaskOwnerInfo, MFA) ->
    initial_call(MFA),
    erlang:link(TaskOwner),
    proc_lib:init_ack({ok, self()}),
        Ref =
        receive
            {TaskOwner, Ref1} ->
                Ref1
        after
            5000 ->
		 exit(timeout)
        end,
        erlang:send(TaskOwner, {Ref, do_apply(TaskOwnerInfo, MFA)}).

-spec noreply({node(), pid() | atom()},
              {atom(), atom(), [term()]}) ->
                    term().
noreply(TaskOwnerInfo, MFA) ->
    initial_call(MFA),
        do_apply(TaskOwnerInfo, MFA).

initial_call(MFA) ->
    erlang:put('$initial_call', get_initial_call(MFA)).

get_initial_call({erlang, apply, [Fun, []]}) when erlang:is_function(Fun, 0) ->
    {module, Mod} = erlang:fun_info(Fun, module),
    {name, Name}  = erlang:fun_info(Fun, name),
    {Mod, Name, 0};
get_initial_call({Mod, Fun, Args}) ->
    {Mod, Fun, erlang:length(Args)}.

do_apply(TaskOwnerInfo, {Mod, Fun, Args} = MFA) ->
    try
        erlang:apply(Mod, Fun, Args)
    catch
        error: Value ->
            task_exit(TaskOwnerInfo, MFA,
                      {Value, erlang:get_stacktrace()});
        throw: Value ->
            task_exit(TaskOwnerInfo, MFA,
                      {{nocatch, Value}, erlang:get_stacktrace()});
        exit: Value ->
            task_exit(TaskOwnerInfo, MFA, Value)
    end.

task_exit(_, _, normal) ->
    erlang:exit(normal);
task_exit(_, _, shutdown) ->
    erlang:exit(shutdown);
task_exit(_, _, Reason) when erlang:tuple_size(Reason) =:= 2
                             andalso
                             erlang:element(2, Reason) =:= shutdown ->
    erlang:exit(Reason);
task_exit(TaskOwnerInfo, MFA, Reason) ->
    {Fun, Args} = get_running(MFA),
    error_logger:format(
      "** Task ~p terminating~n" ++
          "** Started from ~p~n" ++
          "** When function == ~p~n" ++
          "**      arguments == ~p~n" ++
          "** Reason for termination == ~n" ++
          "** ~p~n", [erlang:self(),
                      get_from(TaskOwnerInfo),
                      Fun, Args, Reason]),
    erlang:exit(Reason).

get_from({Node, PidOrName}) when Node =:= erlang:node() ->
    PidOrName;
get_from(Other) ->
    Other.

get_running({erlang, apply, [Fun, []]}) when erlang:is_function(Fun, 0) ->
    {Fun, []};
get_running({Mod, Fun, Args}) ->
    {erlang:make_fun(Mod, Fun, erlang:length(Args)), Args}.
