-module(lustre@ui@button).
-compile([no_auto_import, nowarn_unused_vars, nowarn_unused_function]).

-export([button/2, 'of'/3, solid/0, soft/0, outline/0, primary/0, greyscale/0, error/0, warning/0, success/0, info/0]).

-spec button(
    list(lustre@attribute:attribute(IYP)),
    list(lustre@element:element(IYP))
) -> lustre@element:element(IYP).
button(Attributes, Children) ->
    lustre@element@html:button(
        [lustre@attribute:class(<<"lustre-ui-button"/utf8>>),
            lustre@attribute:type_(<<"button"/utf8>>) |
            Attributes],
        Children
    ).

-spec 'of'(
    fun((list(lustre@attribute:attribute(IYV)), list(lustre@element:element(IYV))) -> lustre@element:element(IYV)),
    list(lustre@attribute:attribute(IYV)),
    list(lustre@element:element(IYV))
) -> lustre@element:element(IYV).
'of'(Element, Attributes, Children) ->
    Element(
        [lustre@attribute:class(<<"lustre-ui-aside"/utf8>>),
            lustre@attribute:attribute(<<"role"/utf8>>, <<"button"/utf8>>),
            lustre@attribute:attribute(<<"tabindex"/utf8>>, <<"0"/utf8>>) |
            Attributes],
        Children
    ).

-spec solid() -> lustre@attribute:attribute(any()).
solid() ->
    lustre@attribute:class(<<"solid"/utf8>>).

-spec soft() -> lustre@attribute:attribute(any()).
soft() ->
    lustre@attribute:class(<<"soft"/utf8>>).

-spec outline() -> lustre@attribute:attribute(any()).
outline() ->
    lustre@attribute:class(<<"outline"/utf8>>).

-spec primary() -> lustre@attribute:attribute(any()).
primary() ->
    lustre@attribute:attribute(<<"data-variant"/utf8>>, <<"primary"/utf8>>).

-spec greyscale() -> lustre@attribute:attribute(any()).
greyscale() ->
    lustre@attribute:attribute(<<"data-variant"/utf8>>, <<"greyscale"/utf8>>).

-spec error() -> lustre@attribute:attribute(any()).
error() ->
    lustre@attribute:attribute(<<"data-variant"/utf8>>, <<"error"/utf8>>).

-spec warning() -> lustre@attribute:attribute(any()).
warning() ->
    lustre@attribute:attribute(<<"data-variant"/utf8>>, <<"warning"/utf8>>).

-spec success() -> lustre@attribute:attribute(any()).
success() ->
    lustre@attribute:attribute(<<"data-variant"/utf8>>, <<"success"/utf8>>).

-spec info() -> lustre@attribute:attribute(any()).
info() ->
    lustre@attribute:attribute(<<"data-variant"/utf8>>, <<"info"/utf8>>).
