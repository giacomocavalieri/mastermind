-module(lustre@ui@cluster).
-compile([no_auto_import, nowarn_unused_vars, nowarn_unused_function]).

-export(['of'/3, cluster/2, from_start/0, from_end/0, packed/0, tight/0, relaxed/0, loose/0, space/1]).

-spec 'of'(
    fun((list(lustre@attribute:attribute(IRF)), list(lustre@element:element(IRF))) -> lustre@element:element(IRF)),
    list(lustre@attribute:attribute(IRF)),
    list(lustre@element:element(IRF))
) -> lustre@element:element(IRF).
'of'(Element, Attributes, Children) ->
    Element(
        [lustre@attribute:class(<<"lustre-ui-cluster"/utf8>>) | Attributes],
        Children
    ).

-spec cluster(
    list(lustre@attribute:attribute(IQZ)),
    list(lustre@element:element(IQZ))
) -> lustre@element:element(IQZ).
cluster(Attributes, Children) ->
    'of'(fun lustre@element@html:'div'/2, Attributes, Children).

-spec from_start() -> lustre@attribute:attribute(any()).
from_start() ->
    lustre@attribute:class(<<"from-start"/utf8>>).

-spec from_end() -> lustre@attribute:attribute(any()).
from_end() ->
    lustre@attribute:class(<<"from-end"/utf8>>).

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
