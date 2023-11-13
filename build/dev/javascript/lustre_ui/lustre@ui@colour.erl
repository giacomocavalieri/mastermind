-module(lustre@ui@colour).
-compile([no_auto_import, nowarn_unused_vars, nowarn_unused_function]).

-export([grey/0, mauve/0, slate/0, sage/0, olive/0, sand/0, gold/0, bronze/0, brown/0, yellow/0, amber/0, orange/0, tomato/0, red/0, ruby/0, crimson/0, pink/0, plum/0, purple/0, violet/0, iris/0, indigo/0, blue/0, cyan/0, teal/0, jade/0, green/0, grass/0, lime/0, mint/0, sky/0]).
-export_type([scale/0]).

-type scale() :: {scale,
        gleam_community@colour:colour(),
        gleam_community@colour:colour(),
        gleam_community@colour:colour(),
        gleam_community@colour:colour(),
        gleam_community@colour:colour(),
        gleam_community@colour:colour(),
        gleam_community@colour:colour(),
        gleam_community@colour:colour(),
        gleam_community@colour:colour(),
        gleam_community@colour:colour(),
        gleam_community@colour:colour(),
        gleam_community@colour:colour()}.

-spec from_radix_scale(
    integer(),
    integer(),
    integer(),
    integer(),
    integer(),
    integer(),
    integer(),
    integer(),
    integer(),
    integer(),
    integer(),
    integer()
) -> scale().
from_radix_scale(A, B, C, D, E, F, G, H, I, J, K, L) ->
    _assert_subject = gleam_community@colour:from_rgb_hex(A),
    {ok, App_background} = case _assert_subject of
        {ok, _} -> _assert_subject;
        _assert_fail ->
            erlang:error(#{gleam_error => let_assert,
                        message => <<"Assertion pattern match failed"/utf8>>,
                        value => _assert_fail,
                        module => <<"lustre/ui/colour"/utf8>>,
                        function => <<"from_radix_scale"/utf8>>,
                        line => 56})
    end,
    _assert_subject@1 = gleam_community@colour:from_rgb_hex(B),
    {ok, App_background_subtle} = case _assert_subject@1 of
        {ok, _} -> _assert_subject@1;
        _assert_fail@1 ->
            erlang:error(#{gleam_error => let_assert,
                        message => <<"Assertion pattern match failed"/utf8>>,
                        value => _assert_fail@1,
                        module => <<"lustre/ui/colour"/utf8>>,
                        function => <<"from_radix_scale"/utf8>>,
                        line => 57})
    end,
    _assert_subject@2 = gleam_community@colour:from_rgb_hex(C),
    {ok, App_border} = case _assert_subject@2 of
        {ok, _} -> _assert_subject@2;
        _assert_fail@2 ->
            erlang:error(#{gleam_error => let_assert,
                        message => <<"Assertion pattern match failed"/utf8>>,
                        value => _assert_fail@2,
                        module => <<"lustre/ui/colour"/utf8>>,
                        function => <<"from_radix_scale"/utf8>>,
                        line => 58})
    end,
    _assert_subject@3 = gleam_community@colour:from_rgb_hex(D),
    {ok, Element_background} = case _assert_subject@3 of
        {ok, _} -> _assert_subject@3;
        _assert_fail@3 ->
            erlang:error(#{gleam_error => let_assert,
                        message => <<"Assertion pattern match failed"/utf8>>,
                        value => _assert_fail@3,
                        module => <<"lustre/ui/colour"/utf8>>,
                        function => <<"from_radix_scale"/utf8>>,
                        line => 59})
    end,
    _assert_subject@4 = gleam_community@colour:from_rgb_hex(E),
    {ok, Element_background_hover} = case _assert_subject@4 of
        {ok, _} -> _assert_subject@4;
        _assert_fail@4 ->
            erlang:error(#{gleam_error => let_assert,
                        message => <<"Assertion pattern match failed"/utf8>>,
                        value => _assert_fail@4,
                        module => <<"lustre/ui/colour"/utf8>>,
                        function => <<"from_radix_scale"/utf8>>,
                        line => 60})
    end,
    _assert_subject@5 = gleam_community@colour:from_rgb_hex(F),
    {ok, Element_background_strong} = case _assert_subject@5 of
        {ok, _} -> _assert_subject@5;
        _assert_fail@5 ->
            erlang:error(#{gleam_error => let_assert,
                        message => <<"Assertion pattern match failed"/utf8>>,
                        value => _assert_fail@5,
                        module => <<"lustre/ui/colour"/utf8>>,
                        function => <<"from_radix_scale"/utf8>>,
                        line => 61})
    end,
    _assert_subject@6 = gleam_community@colour:from_rgb_hex(G),
    {ok, Element_border_strong} = case _assert_subject@6 of
        {ok, _} -> _assert_subject@6;
        _assert_fail@6 ->
            erlang:error(#{gleam_error => let_assert,
                        message => <<"Assertion pattern match failed"/utf8>>,
                        value => _assert_fail@6,
                        module => <<"lustre/ui/colour"/utf8>>,
                        function => <<"from_radix_scale"/utf8>>,
                        line => 62})
    end,
    _assert_subject@7 = gleam_community@colour:from_rgb_hex(H),
    {ok, Element_border_subtle} = case _assert_subject@7 of
        {ok, _} -> _assert_subject@7;
        _assert_fail@7 ->
            erlang:error(#{gleam_error => let_assert,
                        message => <<"Assertion pattern match failed"/utf8>>,
                        value => _assert_fail@7,
                        module => <<"lustre/ui/colour"/utf8>>,
                        function => <<"from_radix_scale"/utf8>>,
                        line => 63})
    end,
    _assert_subject@8 = gleam_community@colour:from_rgb_hex(I),
    {ok, Solid_background} = case _assert_subject@8 of
        {ok, _} -> _assert_subject@8;
        _assert_fail@8 ->
            erlang:error(#{gleam_error => let_assert,
                        message => <<"Assertion pattern match failed"/utf8>>,
                        value => _assert_fail@8,
                        module => <<"lustre/ui/colour"/utf8>>,
                        function => <<"from_radix_scale"/utf8>>,
                        line => 64})
    end,
    _assert_subject@9 = gleam_community@colour:from_rgb_hex(J),
    {ok, Solid_background_hover} = case _assert_subject@9 of
        {ok, _} -> _assert_subject@9;
        _assert_fail@9 ->
            erlang:error(#{gleam_error => let_assert,
                        message => <<"Assertion pattern match failed"/utf8>>,
                        value => _assert_fail@9,
                        module => <<"lustre/ui/colour"/utf8>>,
                        function => <<"from_radix_scale"/utf8>>,
                        line => 65})
    end,
    _assert_subject@10 = gleam_community@colour:from_rgb_hex(K),
    {ok, Text_high_contrast} = case _assert_subject@10 of
        {ok, _} -> _assert_subject@10;
        _assert_fail@10 ->
            erlang:error(#{gleam_error => let_assert,
                        message => <<"Assertion pattern match failed"/utf8>>,
                        value => _assert_fail@10,
                        module => <<"lustre/ui/colour"/utf8>>,
                        function => <<"from_radix_scale"/utf8>>,
                        line => 66})
    end,
    _assert_subject@11 = gleam_community@colour:from_rgb_hex(L),
    {ok, Text_low_contrast} = case _assert_subject@11 of
        {ok, _} -> _assert_subject@11;
        _assert_fail@11 ->
            erlang:error(#{gleam_error => let_assert,
                        message => <<"Assertion pattern match failed"/utf8>>,
                        value => _assert_fail@11,
                        module => <<"lustre/ui/colour"/utf8>>,
                        function => <<"from_radix_scale"/utf8>>,
                        line => 67})
    end,
    {scale,
        App_background,
        App_background_subtle,
        App_border,
        Element_background,
        Element_background_hover,
        Element_background_strong,
        Element_border_subtle,
        Element_border_strong,
        Solid_background,
        Solid_background_hover,
        Text_high_contrast,
        Text_low_contrast}.

-spec grey() -> scale().
grey() ->
    from_radix_scale(
        16#FCFCFC,
        16#F9F9F9,
        16#DDDDDD,
        16#F1F1F1,
        16#EBEBEB,
        16#E4E4E4,
        16#BBBBBB,
        16#D4D4D4,
        16#8D8D8D,
        16#808080,
        16#202020,
        16#646464
    ).

-spec mauve() -> scale().
mauve() ->
    from_radix_scale(
        16#FDFCFD,
        16#FAF9FB,
        16#DFDCE3,
        16#F3F1F5,
        16#ECEAEF,
        16#E6E3E9,
        16#BCBAC7,
        16#D5D3DB,
        16#8E8C99,
        16#817F8B,
        16#211F26,
        16#65636D
    ).

-spec slate() -> scale().
slate() ->
    from_radix_scale(
        16#FCFCFD,
        16#F9F9FB,
        16#DDDDE3,
        16#F2F2F5,
        16#EBEBEF,
        16#E4E4E9,
        16#B9BBC6,
        16#D3D4DB,
        16#8B8D98,
        16#7E808A,
        16#1C2024,
        16#60646C
    ).

-spec sage() -> scale().
sage() ->
    from_radix_scale(
        16#FBFDFC,
        16#F7F9F8,
        16#DCDFDD,
        16#F0F2F1,
        16#E9ECEB,
        16#E3E6E4,
        16#B8BCBA,
        16#D2D5D3,
        16#868E8B,
        16#7A817F,
        16#1A211E,
        16#5F6563
    ).

-spec olive() -> scale().
olive() ->
    from_radix_scale(
        16#FCFDFC,
        16#F8FAF8,
        16#DBDEDB,
        16#F1F3F1,
        16#EAECEA,
        16#E3E5E3,
        16#B9BCB8,
        16#D2D4D1,
        16#898E87,
        16#7C817B,
        16#1D211C,
        16#60655F
    ).

-spec sand() -> scale().
sand() ->
    from_radix_scale(
        16#FDFDFC,
        16#F9F9F8,
        16#DDDDDA,
        16#F2F2F0,
        16#EBEBE9,
        16#E4E4E2,
        16#BCBBB5,
        16#D3D2CE,
        16#8D8D86,
        16#80807A,
        16#21201C,
        16#63635E
    ).

-spec gold() -> scale().
gold() ->
    from_radix_scale(
        16#FDFDFC,
        16#FBF9F2,
        16#DAD1BD,
        16#F5F2E9,
        16#EEEADD,
        16#E5DFD0,
        16#B8A383,
        16#CBBDA4,
        16#978365,
        16#89775C,
        16#3B352B,
        16#71624B
    ).

-spec bronze() -> scale().
bronze() ->
    from_radix_scale(
        16#FDFCFC,
        16#FDF8F6,
        16#E0CEC7,
        16#F8F1EE,
        16#F2E8E4,
        16#EADDD7,
        16#BFA094,
        16#D1B9B0,
        16#A18072,
        16#947467,
        16#43302B,
        16#7D5E54
    ).

-spec brown() -> scale().
brown() ->
    from_radix_scale(
        16#FEFDFC,
        16#FCF9F6,
        16#E8CDB5,
        16#F8F1EA,
        16#F4E9DD,
        16#EFDDCC,
        16#D09E72,
        16#DDB896,
        16#AD7F58,
        16#9E7352,
        16#3E332E,
        16#815E46
    ).

-spec yellow() -> scale().
yellow() ->
    from_radix_scale(
        16#FDFDF9,
        16#FFFBE0,
        16#ECDD85,
        16#FFF8C6,
        16#FCF3AF,
        16#F7EA9B,
        16#C9AA45,
        16#DAC56E,
        16#FBE32D,
        16#F9DA10,
        16#473B1F,
        16#775F28
    ).

-spec amber() -> scale().
amber() ->
    from_radix_scale(
        16#FEFDFB,
        16#FFF9ED,
        16#F5D08C,
        16#FFF3D0,
        16#FFECB7,
        16#FFE0A1,
        16#D6A35C,
        16#E4BB78,
        16#FFC53D,
        16#FFBA1A,
        16#4F3422,
        16#915930
    ).

-spec orange() -> scale().
orange() ->
    from_radix_scale(
        16#FEFCFB,
        16#FFF8F4,
        16#FFC291,
        16#FFEDD5,
        16#FFE0BB,
        16#FFD3A4,
        16#ED8A5C,
        16#FFAA7D,
        16#F76808,
        16#ED5F00,
        16#582D1D,
        16#99543A
    ).

-spec tomato() -> scale().
tomato() ->
    from_radix_scale(
        16#FFFCFC,
        16#FFF8F7,
        16#FAC7BE,
        16#FFF0EE,
        16#FFE6E2,
        16#FDD8D3,
        16#EA9280,
        16#F3B0A2,
        16#E54D2E,
        16#D84224,
        16#5C271F,
        16#C33113
    ).

-spec red() -> scale().
red() ->
    from_radix_scale(
        16#FFFCFC,
        16#FFF7F7,
        16#F9C6C6,
        16#FFEFEF,
        16#FFE5E5,
        16#FDD8D8,
        16#EB9091,
        16#F3AEAF,
        16#E5484D,
        16#D93D42,
        16#641723,
        16#C62A2F
    ).

-spec ruby() -> scale().
ruby() ->
    from_radix_scale(
        16#FFFCFD,
        16#FFF7F9,
        16#F5C7D1,
        16#FEEFF3,
        16#FDE5EA,
        16#FAD8E0,
        16#E592A2,
        16#EEAFBC,
        16#E54666,
        16#DA3A5C,
        16#64172B,
        16#CA244D
    ).

-spec crimson() -> scale().
crimson() ->
    from_radix_scale(
        16#FFFCFD,
        16#FFF7FB,
        16#F4C6DB,
        16#FEEFF6,
        16#FCE5F0,
        16#F9D8E7,
        16#E58FB1,
        16#EDADC8,
        16#E93D82,
        16#DC3175,
        16#621639,
        16#CB1D63
    ).

-spec pink() -> scale().
pink() ->
    from_radix_scale(
        16#FFFCFE,
        16#FFF7FC,
        16#F3C6E2,
        16#FEEEF8,
        16#FCE5F3,
        16#F9D8EC,
        16#E38EC3,
        16#ECADD4,
        16#D6409F,
        16#CD3093,
        16#651249,
        16#C41C87
    ).

-spec plum() -> scale().
plum() ->
    from_radix_scale(
        16#FEFCFF,
        16#FFF8FF,
        16#EBC8ED,
        16#FCEFFC,
        16#F9E5F9,
        16#F3D9F4,
        16#CF91D8,
        16#DFAFE3,
        16#AB4ABA,
        16#A43CB4,
        16#53195D,
        16#9C2BAD
    ).

-spec purple() -> scale().
purple() ->
    from_radix_scale(
        16#FEFCFE,
        16#FDFAFF,
        16#E3CCF4,
        16#F9F1FE,
        16#F3E7FC,
        16#EDDBF9,
        16#BE93E4,
        16#D3B4ED,
        16#8E4EC6,
        16#8445BC,
        16#402060,
        16#793AAF
    ).

-spec violet() -> scale().
violet() ->
    from_radix_scale(
        16#FDFCFE,
        16#FBFAFF,
        16#D7CFF9,
        16#F5F2FF,
        16#EDE9FE,
        16#E4DEFC,
        16#AA99EC,
        16#C4B8F3,
        16#6E56CF,
        16#644FC1,
        16#2F265F,
        16#5746AF
    ).

-spec iris() -> scale().
iris() ->
    from_radix_scale(
        16#FDFDFF,
        16#FAFAFF,
        16#D0D0FA,
        16#F3F3FF,
        16#EBEBFE,
        16#E0E0FD,
        16#9B9EF0,
        16#BABBF5,
        16#5B5BD6,
        16#5353CE,
        16#272962,
        16#4747C2
    ).

-spec indigo() -> scale().
indigo() ->
    from_radix_scale(
        16#FDFDFE,
        16#F8FAFF,
        16#C6D4F9,
        16#F0F4FF,
        16#E6EDFE,
        16#D9E2FC,
        16#8DA4EF,
        16#AEC0F5,
        16#3E63DD,
        16#3A5CCC,
        16#1F2D5C,
        16#3451B2
    ).

-spec blue() -> scale().
blue() ->
    from_radix_scale(
        16#FBFDFF,
        16#F5FAFF,
        16#B7D9F8,
        16#EDF6FF,
        16#E1F0FF,
        16#CEE7FE,
        16#5EB0EF,
        16#96C7F2,
        16#0091FF,
        16#0880EA,
        16#113264,
        16#0B68CB
    ).

-spec cyan() -> scale().
cyan() ->
    from_radix_scale(
        16#FAFDFE,
        16#F2FCFD,
        16#AADEE6,
        16#E7F9FB,
        16#D8F3F6,
        16#C4EAEF,
        16#3DB9CF,
        16#84CDDA,
        16#05A2C2,
        16#0894B3,
        16#0D3C48,
        16#0C7792
    ).

-spec teal() -> scale().
teal() ->
    from_radix_scale(
        16#FAFEFD,
        16#F1FCFA,
        16#AFDFD7,
        16#E7F9F5,
        16#D9F3EE,
        16#C7EBE5,
        16#53B9AB,
        16#8DCEC3,
        16#12A594,
        16#0E9888,
        16#0D3D38,
        16#067A6F
    ).

-spec jade() -> scale().
jade() ->
    from_radix_scale(
        16#FBFEFD,
        16#EFFDF6,
        16#B0E0CC,
        16#E4FAEF,
        16#D7F4E6,
        16#C6ECDB,
        16#56BA9F,
        16#8FCFB9,
        16#29A383,
        16#259678,
        16#1D3B31,
        16#1A7A5E
    ).

-spec green() -> scale().
green() ->
    from_radix_scale(
        16#FBFEFC,
        16#F2FCF5,
        16#B4DFC4,
        16#E9F9EE,
        16#DDF3E4,
        16#CCEBD7,
        16#5BB98C,
        16#92CEAC,
        16#30A46C,
        16#299764,
        16#193B2D,
        16#18794E
    ).

-spec grass() -> scale().
grass() ->
    from_radix_scale(
        16#FBFEFB,
        16#F3FCF3,
        16#B7DFBA,
        16#EBF9EB,
        16#DFF3DF,
        16#CEEBCF,
        16#65BA75,
        16#97CF9C,
        16#46A758,
        16#3D9A50,
        16#203C25,
        16#297C3B
    ).

-spec lime() -> scale().
lime() ->
    from_radix_scale(
        16#FCFDFA,
        16#F7FCF0,
        16#C6DE99,
        16#EDFADA,
        16#E2F5C4,
        16#D5EDAF,
        16#9AB654,
        16#B2CA7F,
        16#BDEE63,
        16#B0E64C,
        16#37401C,
        16#59682C
    ).

-spec mint() -> scale().
mint() ->
    from_radix_scale(
        16#F9FEFD,
        16#EFFEFA,
        16#A6E1D3,
        16#DDFBF3,
        16#CCF7EC,
        16#BBEEE2,
        16#51BDA7,
        16#87D0BF,
        16#86EAD4,
        16#7FE1CC,
        16#16433C,
        16#27756A
    ).

-spec sky() -> scale().
sky() ->
    from_radix_scale(
        16#F9FEFF,
        16#F1FCFF,
        16#A5DCED,
        16#E2F9FF,
        16#D2F4FD,
        16#BFEBF8,
        16#46B8D8,
        16#82CAE0,
        16#7CE2FE,
        16#72DBF8,
        16#19404D,
        16#256E93
    ).
