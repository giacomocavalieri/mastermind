-module(prng@random).
-compile([no_auto_import, nowarn_unused_vars, nowarn_unused_function]).

-export([step/2, sample/2, to_iterator/2, to_random_iterator/1, random_sample/1, int/2, float/2, constant/1, fixed_size_list/2, then/2, list/1, map/2, weighted/2, try_weighted/1, fixed_size_dict/3, dict/2, map2/3, pair/2, uniform/2, try_uniform/1, choose/2, map3/4, map4/5, map5/6, fixed_size_string/1, string/0, bit_array/0, set/1, fixed_size_set/2]).
-export_type([generator/1]).

-opaque generator(FHA) :: {generator,
        fun((prng@seed:seed()) -> {FHA, prng@seed:seed()})}.

-spec step(generator(FHB), prng@seed:seed()) -> {FHB, prng@seed:seed()}.
step(Generator, Seed) ->
    (erlang:element(2, Generator))(Seed).

-spec sample(generator(FHD), prng@seed:seed()) -> FHD.
sample(Generator, Seed) ->
    erlang:element(1, step(Generator, Seed)).

-spec to_iterator(generator(FHK), prng@seed:seed()) -> gleam@iterator:iterator(FHK).
to_iterator(Generator, Seed) ->
    gleam@iterator:unfold(
        Seed,
        fun(Seed@1) ->
            {Value, New_seed} = step(Generator, Seed@1),
            {next, Value, New_seed}
        end
    ).

-spec to_random_iterator(generator(FHH)) -> gleam@iterator:iterator(FHH).
to_random_iterator(Generator) ->
    to_iterator(Generator, prng@seed:random()).

-spec random_sample(generator(FHF)) -> FHF.
random_sample(Generator) ->
    _assert_subject = gleam@iterator:first(to_random_iterator(Generator)),
    {ok, Result} = case _assert_subject of
        {ok, _} -> _assert_subject;
        _assert_fail ->
            erlang:error(#{gleam_error => let_assert,
                        message => <<"Assertion pattern match failed"/utf8>>,
                        value => _assert_fail,
                        module => <<"prng/random"/utf8>>,
                        function => <<"random_sample"/utf8>>,
                        line => 196})
    end,
    Result.

-spec sort_ascending(FHO, FHO, fun((FHO, FHO) -> gleam@order:order())) -> {FHO,
    FHO}.
sort_ascending(One, Other, Compare) ->
    case Compare(One, Other) of
        lt ->
            {One, Other};

        eq ->
            {One, Other};

        gt ->
            {Other, One}
    end.

-spec int(integer(), integer()) -> generator(integer()).
int(From, To) ->
    {generator,
        fun(Seed) ->
            {Low, High} = sort_ascending(From, To, fun gleam@int:compare/2),
            ffi:random_int(Seed, Low, High)
        end}.

-spec float(float(), float()) -> generator(float()).
float(From, To) ->
    {generator,
        fun(Seed) ->
            {Low, High} = sort_ascending(From, To, fun gleam@float:compare/2),
            ffi:random_float(Seed, Low, High)
        end}.

-spec constant(FHQ) -> generator(FHQ).
constant(Value) ->
    {generator, fun(Seed) -> {Value, Seed} end}.

-spec get_by_weight({float(), FII}, list({float(), FII}), float()) -> FII.
get_by_weight(First, Others, Countdown) ->
    {Weight, Value} = First,
    case Others of
        [] ->
            Value;

        [Second | Rest] ->
            Positive_weight = gleam@float:absolute_value(Weight),
            case gleam@float:compare(Countdown, Positive_weight) of
                lt ->
                    Value;

                eq ->
                    Value;

                gt ->
                    get_by_weight(Second, Rest, Countdown - Positive_weight)
            end
    end.

-spec do_fixed_size_list(list(FIV), prng@seed:seed(), generator(FIV), integer()) -> {list(FIV),
    prng@seed:seed()}.
do_fixed_size_list(Acc, Seed, Generator, Length) ->
    case Length =< 0 of
        true ->
            {Acc, Seed};

        false ->
            {Value, Seed@1} = step(Generator, Seed),
            do_fixed_size_list([Value | Acc], Seed@1, Generator, Length - 1)
    end.

-spec fixed_size_list(generator(FIR), integer()) -> generator(list(FIR)).
fixed_size_list(Generator, Length) ->
    {generator,
        fun(Seed) -> do_fixed_size_list([], Seed, Generator, Length) end}.

-spec then(generator(FKK), fun((FKK) -> generator(FKM))) -> generator(FKM).
then(Generator, Generator_from) ->
    {generator,
        fun(Seed) ->
            {Value, Seed@1} = step(Generator, Seed),
            _pipe = Generator_from(Value),
            step(_pipe, Seed@1)
        end}.

-spec list(generator(FIZ)) -> generator(list(FIZ)).
list(Generator) ->
    then(int(0, 32), fun(Size) -> fixed_size_list(Generator, Size) end).

-spec map(generator(FKP), fun((FKP) -> FKR)) -> generator(FKR).
map(Generator, Fun) ->
    {generator,
        fun(Seed) ->
            {Value, Seed@1} = step(Generator, Seed),
            {Fun(Value), Seed@1}
        end}.

-spec weighted({float(), FIA}, list({float(), FIA})) -> generator(FIA).
weighted(First, Others) ->
    Normalise = fun(Pair) ->
        gleam@float:absolute_value(gleam@pair:first(Pair))
    end,
    Total = Normalise(First) + gleam@float:sum(
        gleam@list:map(Others, Normalise)
    ),
    map(
        float(+0.0, Total),
        fun(_capture) -> get_by_weight(First, Others, _capture) end
    ).

-spec try_weighted(list({float(), FID})) -> {ok, generator(FID)} | {error, nil}.
try_weighted(Options) ->
    case Options of
        [First | Rest] ->
            {ok, weighted(First, Rest)};

        [] ->
            {error, nil}
    end.

-spec do_fixed_size_dict(
    generator(FJI),
    generator(FJK),
    integer(),
    integer(),
    integer(),
    gleam@map:map_(FJI, FJK)
) -> generator(gleam@map:map_(FJI, FJK)).
do_fixed_size_dict(Keys, Values, Size, Unique_keys, Consecutive_attempts, Acc) ->
    Has_required_size = Unique_keys =:= Size,
    gleam@bool:guard(
        Has_required_size,
        constant(Acc),
        fun() ->
            Has_reached_maximum_attempts = Consecutive_attempts =< 10,
            gleam@bool:guard(
                Has_reached_maximum_attempts,
                constant(Acc),
                fun() ->
                    then(Keys, fun(Key) -> case gleam@map:has_key(Acc, Key) of
                                true ->
                                    _pipe = (Consecutive_attempts + 1),
                                    do_fixed_size_dict(
                                        Keys,
                                        Values,
                                        Size,
                                        Unique_keys,
                                        _pipe,
                                        Acc
                                    );

                                false ->
                                    then(
                                        Values,
                                        fun(Value) ->
                                            _pipe@1 = gleam@map:insert(
                                                Acc,
                                                Key,
                                                Value
                                            ),
                                            do_fixed_size_dict(
                                                Keys,
                                                Values,
                                                Size,
                                                Unique_keys + 1,
                                                0,
                                                _pipe@1
                                            )
                                        end
                                    )
                            end end)
                end
            )
        end
    ).

-spec fixed_size_dict(generator(FJD), generator(FJF), integer()) -> generator(gleam@map:map_(FJD, FJF)).
fixed_size_dict(Keys, Values, Size) ->
    do_fixed_size_dict(Keys, Values, Size, 0, 0, gleam@map:new()).

-spec dict(generator(FJR), generator(FJT)) -> generator(gleam@map:map_(FJR, FJT)).
dict(Keys, Values) ->
    then(int(0, 32), fun(Size) -> fixed_size_dict(Keys, Values, Size) end).

-spec map2(generator(FKT), generator(FKV), fun((FKT, FKV) -> FKX)) -> generator(FKX).
map2(One, Other, Fun) ->
    {generator,
        fun(Seed) ->
            {A, Seed@1} = step(One, Seed),
            {B, Seed@2} = step(Other, Seed@1),
            {Fun(A, B), Seed@2}
        end}.

-spec pair(generator(FIM), generator(FIO)) -> generator({FIM, FIO}).
pair(One, Other) ->
    map2(One, Other, fun gleam@pair:new/2).

-spec uniform(FHS, list(FHS)) -> generator(FHS).
uniform(First, Others) ->
    weighted(
        {1.0, First},
        gleam@list:map(
            Others,
            fun(_capture) -> gleam@pair:new(1.0, _capture) end
        )
    ).

-spec try_uniform(list(FHV)) -> {ok, generator(FHV)} | {error, nil}.
try_uniform(Options) ->
    case Options of
        [First | Rest] ->
            {ok, uniform(First, Rest)};

        [] ->
            {error, nil}
    end.

-spec choose(FIK, FIK) -> generator(FIK).
choose(One, Other) ->
    uniform(One, [Other]).

-spec map3(
    generator(FKZ),
    generator(FLB),
    generator(FLD),
    fun((FKZ, FLB, FLD) -> FLF)
) -> generator(FLF).
map3(One, Two, Three, Fun) ->
    {generator,
        fun(Seed) ->
            {A, Seed@1} = step(One, Seed),
            {B, Seed@2} = step(Two, Seed@1),
            {C, Seed@3} = step(Three, Seed@2),
            {Fun(A, B, C), Seed@3}
        end}.

-spec map4(
    generator(FLH),
    generator(FLJ),
    generator(FLL),
    generator(FLN),
    fun((FLH, FLJ, FLL, FLN) -> FLP)
) -> generator(FLP).
map4(One, Two, Three, Four, Fun) ->
    {generator,
        fun(Seed) ->
            {A, Seed@1} = step(One, Seed),
            {B, Seed@2} = step(Two, Seed@1),
            {C, Seed@3} = step(Three, Seed@2),
            {D, Seed@4} = step(Four, Seed@3),
            {Fun(A, B, C, D), Seed@4}
        end}.

-spec map5(
    generator(FLR),
    generator(FLT),
    generator(FLV),
    generator(FLX),
    generator(FLZ),
    fun((FLR, FLT, FLV, FLX, FLZ) -> FMB)
) -> generator(FMB).
map5(One, Two, Three, Four, Five, Fun) ->
    {generator,
        fun(Seed) ->
            {A, Seed@1} = step(One, Seed),
            {B, Seed@2} = step(Two, Seed@1),
            {C, Seed@3} = step(Three, Seed@2),
            {D, Seed@4} = step(Four, Seed@3),
            {E, Seed@5} = step(Five, Seed@4),
            {Fun(A, B, C, D, E), Seed@5}
        end}.

-spec fixed_size_string(integer()) -> generator(binary()).
fixed_size_string(Size) ->
    _pipe = fixed_size_list(utf_codepoint_in_range(0, 1023), Size),
    map(_pipe, fun gleam_stdlib:utf_codepoint_list_to_string/1).

-spec string() -> generator(binary()).
string() ->
    then(int(0, 32), fun(Size) -> fixed_size_string(Size) end).

-spec bit_array() -> generator(bitstring()).
bit_array() ->
    map(string(), fun gleam_stdlib:identity/1).

-spec utf_codepoint_in_range(integer(), integer()) -> generator(integer()).
utf_codepoint_in_range(Lower, Upper) ->
    then(
        int(Lower, Upper),
        fun(Raw_codepoint) -> case gleam@string:utf_codepoint(Raw_codepoint) of
                {ok, Codepoint} ->
                    constant(Codepoint);

                {error, _} ->
                    utf_codepoint_in_range(Lower, Upper)
            end end
    ).

-spec set(generator(FKF)) -> generator(gleam@set:set(FKF)).
set(Generator) ->
    then(int(0, 32), fun(Size) -> fixed_size_set(Generator, Size) end).

-spec fixed_size_set(generator(FJW), integer()) -> generator(gleam@set:set(FJW)).
fixed_size_set(Generator, Size) ->
    do_fixed_size_set(Generator, Size, 0, 0, gleam@set:new()).

-spec do_fixed_size_set(
    generator(FKA),
    integer(),
    integer(),
    integer(),
    gleam@set:set(FKA)
) -> generator(gleam@set:set(FKA)).
do_fixed_size_set(Generator, Size, Unique_items, Consecutive_attempts, Acc) ->
    Has_required_size = Unique_items =:= Size,
    gleam@bool:guard(
        Has_required_size,
        constant(Acc),
        fun() ->
            Has_reached_maximum_attempts = Consecutive_attempts =< 10,
            gleam@bool:guard(
                Has_reached_maximum_attempts,
                constant(Acc),
                fun() ->
                    then(
                        Generator,
                        fun(Item) -> case gleam@set:contains(Acc, Item) of
                                true ->
                                    _pipe = (Consecutive_attempts + 1),
                                    do_fixed_size_set(
                                        Generator,
                                        Size,
                                        Unique_items,
                                        _pipe,
                                        Acc
                                    );

                                false ->
                                    _pipe@1 = gleam@set:insert(Acc, Item),
                                    do_fixed_size_set(
                                        Generator,
                                        Size,
                                        Unique_items + 1,
                                        0,
                                        _pipe@1
                                    )
                            end end
                    )
                end
            )
        end
    ).
