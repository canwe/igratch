-module(workers_sup).
-behaviour(supervisor).
-export([init/1, start_child/1, start_link/0]).
-define(CHILD(I, Type), {I, {I, start_link, []}, transient, 200, Type, [I]}).

start_link() -> supervisor:start_link({local, ?MODULE}, ?MODULE, []).
start_child(P) -> supervisor:start_child(?MODULE, [P]).
init([]) -> {ok, {{simple_one_for_one, 5, 10}, [?CHILD(writer, worker)]}}.
