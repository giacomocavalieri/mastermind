-module(lustre@ui@sequence).
-compile([no_auto_import, nowarn_unused_vars, nowarn_unused_function]).

-export(['of'/3, sequence/2, breakpoint/1, split/1, packed/0, tight/0, relaxed/0, loose/0, space/1]).

-spec 'of'(
    fun((list(lustre@attribute:attribute(JBC)), list(lustre@element:element(JBC))) -> lustre@element:element(JBC)),
    list(lustre@attribute:attribute(JBC)),
    list(lustre@element:element(JBC))
) -> lustre@element:element(JBC).
'of'(Element, Attributes, Children) ->
    Element(
        [lustre@attribute:class(<<"lustre-ui-sequence"/utf8>>) | Attributes],
        Children
    ).

-spec sequence(
    list(lustre@attribute:attribute(JAW)),
    list(lustre@element:element(JAW))
) -> lustre@element:element(JAW).
sequence(Attributes, Children) ->
    'of'(fun lustre@element@html:'div'/2, Attributes, Children).

-spec breakpoint(binary()) -> lustre@attribute:attribute(any()).
breakpoint(Break) ->
    lustre@attribute:style([{<<"--break"/utf8>>, Break}]).

-spec split(integer()) -> lustre@attribute:attribute(any()).
split(N) ->
    case N < 3 of
        true ->
            lustre@attribute:class(<<""/utf8>>);

        false ->
            lustre@attribute:attribute(
                <<"data-split-at"/utf8>>,
                gleam@int:to_string(N)
            )
    end.

-spec packed() -> lustre@attribute:attribute(any()).
packed() ->
    lustre@attribute:class(<<"packed"/utf8>>).

-spec tight() -> lustre@attribute:attribute(any()).
tight() ->
    lustre@attribute:class(<<"tight"/utf8>>).

-spec relaxed() -> lustre@attribute:attribute(any()).
relaxed() ->
    lustre@attribute:class(<<"relaxed"/utf8>>).

-spec loose() -> lustre@attribute:attribute(any()).
loose() ->
    lustre@attribute:class(<<"loose"/utf8>>).

-spec space(binary()) -> lustre@attribute:attribute(any()).
space(Gap) ->
    lustre@attribute:style([{<<"--gap"/utf8>>, Gap}]).
