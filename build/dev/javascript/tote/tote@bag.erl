-module(tote@bag).
-compile([no_auto_import, nowarn_unused_vars]).

-export([from_map/1, to_map/1, map/2, fold/3, new/0, remove_all/2, copies/2, remove/3, insert/3, from_list/1, update/3, contains/2, is_empty/1, size/1, intersect/2, merge/2, subtract/2, filter/2, to_list/1, to_set/1]).
-export_type([bag/1]).

-opaque bag(FME) :: {bag, gleam@map:map_(FME, integer())}.

-spec from_map(gleam@map:map_(FMK, integer())) -> bag(FMK).
from_map(Map) ->
    {bag, Map}.

-spec to_map(bag(FOK)) -> gleam@map:map_(FOK, integer()).
to_map(Bag) ->
    erlang:element(2, Bag).

-spec map(bag(FNX), fun((FNX, integer()) -> FNZ)) -> bag(FNZ).
map(Bag, Fun) ->
    fold(
        Bag,
        new(),
        fun(Acc, Item, Copies) -> insert(Acc, Copies, Fun(Item, Copies)) end
    ).

-spec fold(bag(FNU), FNW, fun((FNW, FNU, integer()) -> FNW)) -> FNW.
fold(Bag, Initial, Fun) ->
    gleam@map:fold(erlang:element(2, Bag), Initial, Fun).

-spec new() -> bag(any()).
new() ->
    {bag, gleam@map:new()}.

-spec remove_all(bag(FMU), FMU) -> bag(FMU).
remove_all(Bag, Item) ->
    {bag, gleam@map:delete(erlang:element(2, Bag), Item)}.

-spec copies(bag(FNA), FNA) -> integer().
copies(Bag, Item) ->
    case gleam@map:get(erlang:element(2, Bag), Item) of
        {ok, Copies} ->
            Copies;

        {error, nil} ->
            0
    end.

-spec remove(bag(FMR), integer(), FMR) -> bag(FMR).
remove(Bag, To_remove, Item) ->
    To_remove@1 = gleam@int:absolute_value(To_remove),
    Item_copies = copies(Bag, Item),
    case gleam@int:compare(To_remove@1, Item_copies) of
        lt ->
            {bag,
                gleam@map:insert(
                    erlang:element(2, Bag),
                    Item,
                    Item_copies - To_remove@1
                )};

        gt ->
            remove_all(Bag, Item);

        eq ->
            remove_all(Bag, Item)
    end.

-spec insert(bag(FMO), integer(), FMO) -> bag(FMO).
insert(Bag, To_add, Item) ->
    case gleam@int:compare(To_add, 0) of
        lt ->
            remove(Bag, To_add, Item);

        eq ->
            Bag;

        gt ->
            {bag,
                gleam@map:update(
                    erlang:element(2, Bag),
                    Item,
                    fun(N) -> gleam@option:unwrap(N, 0) + To_add end
                )}
    end.

-spec from_list(list(FMH)) -> bag(FMH).
from_list(List) ->
    gleam@list:fold(List, new(), fun(Bag, Item) -> insert(Bag, 1, Item) end).

-spec update(bag(FMX), FMX, fun((integer()) -> integer())) -> bag(FMX).
update(Bag, Item, Fun) ->
    Count = copies(Bag, Item),
    New_count = Fun(Count),
    case gleam@int:compare(New_count, 0) of
        lt ->
            remove_all(Bag, Item);

        eq ->
            remove_all(Bag, Item);

        gt ->
            _pipe = remove_all(Bag, Item),
            insert(_pipe, New_count, Item)
    end.

-spec contains(bag(FNC), FNC) -> boolean().
contains(Bag, Item) ->
    gleam@map:has_key(erlang:element(2, Bag), Item).

-spec is_empty(bag(any())) -> boolean().
is_empty(Bag) ->
    erlang:element(2, Bag) =:= gleam@map:new().

-spec size(bag(any())) -> integer().
size(Bag) ->
    fold(Bag, 0, fun(Sum, _, Copies) -> Sum + Copies end).

-spec intersect(bag(FNI), bag(FNI)) -> bag(FNI).
intersect(One, Other) ->
    fold(
        One,
        new(),
        fun(Acc, Item, Copies_in_one) -> case copies(Other, Item) of
                0 ->
                    Acc;

                Copies_in_other ->
                    insert(
                        Acc,
                        gleam@int:min(Copies_in_one, Copies_in_other),
                        Item
                    )
            end end
    ).

-spec merge(bag(FNM), bag(FNM)) -> bag(FNM).
merge(One, Other) ->
    fold(
        One,
        Other,
        fun(Acc, Item, Copies_in_one) -> insert(Acc, Copies_in_one, Item) end
    ).

-spec subtract(bag(FNQ), bag(FNQ)) -> bag(FNQ).
subtract(One, Other) ->
    fold(
        Other,
        One,
        fun(Acc, Item, Copies_in_other) ->
            remove(Acc, Copies_in_other, Item)
        end
    ).

-spec filter(bag(FOB), fun((FOB, integer()) -> boolean())) -> bag(FOB).
filter(Bag, Predicate) ->
    fold(Bag, new(), fun(Acc, Item, Copies) -> case Predicate(Item, Copies) of
                true ->
                    insert(Acc, Copies, Item);

                false ->
                    Acc
            end end).

-spec to_list(bag(FOE)) -> list({FOE, integer()}).
to_list(Bag) ->
    gleam@map:to_list(erlang:element(2, Bag)).

-spec to_set(bag(FOH)) -> gleam@set:set(FOH).
to_set(Bag) ->
    gleam@set:from_list(gleam@map:keys(erlang:element(2, Bag))).
