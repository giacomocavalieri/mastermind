-module(prng@seed).
-compile([no_auto_import, nowarn_unused_vars, nowarn_unused_function]).

-export([new/1, random/0]).
-export_type([seed/0]).

-type seed() :: any().

-spec new(integer()) -> seed().
new(Int) ->
    ffi:new_seed(Int).

-spec random() -> seed().
random() ->
    ffi:new_seed(gleam@int:random(0, 4294967296)).
