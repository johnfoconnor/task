-module(task).

-export([async/3,
         async/1,
         await/1,
         await/2]).
-export([safe_await/2,
         safe_await/3]).

-export([start/1, start/3,
         start_link/1, start_link/3]).

-export([get_info/1]).

start(Fun) when erlang:is_function(Fun) ->
    start(erlang, apply, [Fun, []]).

start(M, F, A) ->
    task_supervised:start(get_info(self()), {M, F, A}).

start_link(Fun) when erlang:is_function(Fun) ->
    start_link(erlang, apply, [Fun, []]).

start_link(M, F, A) ->
    task_supervised:start_link(get_info(self()), {M, F, A}).

-spec async(function()) -> {pid(), reference()}.
async(Fun) when erlang:is_function(Fun) ->
    async(erlang, apply, [Fun, []]).

-spec async(atom(), atom(), [term()]) -> {pid(), reference()}.
async(Mod, Fun, Args) ->
    Me  = erlang:self(),
    Pid = proc_lib:spawn_link(task_supervised, async,
                              [Me, get_info(Me), {Mod, Fun, Args}]),
    Ref = erlang:monitor(process, Pid),
    erlang:send(Pid, {Me, Ref}),
    {Pid, Ref}.

-spec await({pid(), reference()}) -> any() | no_return().
await({Pid, Ref}) ->
    await({Pid, Ref}, 5000).

-spec await({pid(), reference()}, non_neg_integer()) ->
                   any() | no_return().
await(TaskRef = {Pid, Ref}, TimeOut) ->
    receive
        {Ref, Reply} ->
            erlang:demonitor(Ref, [flush]),
            Reply;
        {'DOWN', Ref, _, _, noconnection} ->
            erlang:exit({nodedown, erlang:node(Pid),
                         {?MODULE, await, [TaskRef, TimeOut]}});
        {'DOWN', Ref, _, _, Reason} ->
            erlang:exit({Reason,
                         {?MODULE, await, [TaskRef, TimeOut]}})
    after TimeOut ->
            erlang:demonitor(Ref, [flush]),
            erlang:exit({timeout,
                         {?MODULE, await, [TaskRef, TimeOut]}})
    end.

-spec safe_await({pid(), reference()}, term()) -> any().
safe_await(TaskRef, DefaultResult) ->
    safe_await(TaskRef, DefaultResult, 5000).

-spec safe_await({pid(), reference()}, term(), non_neg_integer()) -> any().
safe_await(TaskRef, DefaultResult, TimeOut) ->
    case catch await(TaskRef, TimeOut) of
        {'EXIT', _} ->
            DefaultResult;
        Any ->
            Any
    end.

get_info(Pid) ->
    Name =
        case erlang:process_info(Pid, [registered_name]) of
            [{registered_name, []}] ->
                Pid;
            [{registered_name, RegisteredName}] ->
                RegisteredName
        end,
    {erlang:node(Pid), Name}.
