-module(lustre@ui@aside).
-compile([no_auto_import, nowarn_unused_vars, nowarn_unused_function]).

-export(['of'/4, aside/3, content_first/0, content_last/0, align_start/0, align_centre/0, align_end/0, stretch/0, packed/0, tight/0, relaxed/0, loose/0, space/1, min_width/1]).

-spec 'of'(
    fun((list(lustre@attribute:attribute(IVS)), list(lustre@element:element(IVS))) -> lustre@element:element(IVS)),
    list(lustre@attribute:attribute(IVS)),
    lustre@element:element(IVS),
    lustre@element:element(IVS)
) -> lustre@element:element(IVS).
'of'(Element, Attributes, Side, Main) ->
    Element(
        [lustre@attribute:class(<<"lustre-ui-aside"/utf8>>) | Attributes],
        [Side, Main]
    ).

-spec aside(
    list(lustre@attribute:attribute(IVM)),
    lustre@element:element(IVM),
    lustre@element:element(IVM)
) -> lustre@element:element(IVM).
aside(Attributes, Side, Main) ->
    'of'(fun lustre@element@html:'div'/2, Attributes, Side, Main).

-spec content_first() -> lustre@attribute:attribute(any()).
content_first() ->
    lustre@attribute:class(<<"content-first"/utf8>>).

-spec content_last() -> lustre@attribute:attribute(any()).
content_last() ->
    lustre@attribute:class(<<"content-last"/utf8>>).

-spec align_start() -> lustre@attribute:attribute(any()).
align_start() ->
    lustre@attribute:class(<<"align-start"/utf8>>).

-spec align_centre() -> lustre@attribute:attribute(any()).
align_centre() ->
    lustre@attribute:class(<<"align-centre"/utf8>>).

-spec align_end() -> lustre@attribute:attribute(any()).
align_end() ->
    lustre@attribute:class(<<"align-end"/utf8>>).

-spec stretch() -> lustre@attribute:attribute(any()).
stretch() ->
    lustre@attribute:class(<<"stretch"/utf8>>).

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

-spec min_width(integer()) -> lustre@attribute:attribute(any()).
min_width(Width) ->
    case {Width < 10, Width > 90} of
        {true, _} ->
            lustre@attribute:style([{<<"--min"/utf8>>, <<"10%"/utf8>>}]);

        {false, false} ->
            lustre@attribute:style(
                [{<<"--min"/utf8>>,
                        <<(gleam@int:to_string(Width))/binary, "%"/utf8>>}]
            );

        {_, true} ->
            lustre@attribute:style([{<<"--min"/utf8>>, <<"90%"/utf8>>}])
    end.
