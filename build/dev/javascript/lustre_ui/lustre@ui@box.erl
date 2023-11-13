-module(lustre@ui@box).
-compile([no_auto_import, nowarn_unused_vars, nowarn_unused_function]).

-export(['of'/3, box/2, packed/0, tight/0, relaxed/0, loose/0, space/1]).

-spec 'of'(
    fun((list(lustre@attribute:attribute(JDF)), list(lustre@element:element(JDF))) -> lustre@element:element(JDF)),
    list(lustre@attribute:attribute(JDF)),
    list(lustre@element:element(JDF))
) -> lustre@element:element(JDF).
'of'(Element, Attributes, Children) ->
    Element(
        [lustre@attribute:class(<<"lustre-ui-box"/utf8>>) | Attributes],
        Children
    ).

-spec box(
    list(lustre@attribute:attribute(JCZ)),
    list(lustre@element:element(JCZ))
) -> lustre@element:element(JCZ).
box(Attributes, Children) ->
    'of'(fun lustre@element@html:'div'/2, Attributes, Children).

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
