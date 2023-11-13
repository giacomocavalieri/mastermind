-module(lustre@ui).
-compile([no_auto_import, nowarn_unused_vars, nowarn_unused_function]).

-export([base/0, variant/1]).
-export_type([theme/0, variant/0]).

-type theme() :: {theme,
        lustre@ui@colour:scale(),
        lustre@ui@colour:scale(),
        lustre@ui@colour:scale(),
        lustre@ui@colour:scale(),
        lustre@ui@colour:scale(),
        lustre@ui@colour:scale()}.

-type variant() :: primary | greyscale | error | warning | success | info.

-spec base() -> theme().
base() ->
    {theme,
        lustre@ui@colour:iris(),
        lustre@ui@colour:slate(),
        lustre@ui@colour:red(),
        lustre@ui@colour:yellow(),
        lustre@ui@colour:green(),
        lustre@ui@colour:blue()}.

-spec variant(variant()) -> lustre@attribute:attribute(any()).
variant(Variant) ->
    lustre@attribute:attribute(<<"data-variant"/utf8>>, case Variant of
            primary ->
                <<"primary"/utf8>>;

            greyscale ->
                <<"greyscale"/utf8>>;

            error ->
                <<"error"/utf8>>;

            warning ->
                <<"warning"/utf8>>;

            success ->
                <<"success"/utf8>>;

            info ->
                <<"info"/utf8>>
        end).
