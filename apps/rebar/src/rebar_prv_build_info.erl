%%% This file is a port of the WhatsApp eqwalizer_build_info_prv module from:
%%%
%%% https://github.com/WhatsApp/eqwalizer
%%%
%%% Which had the following:
%%%
%%% Copyright (c) Meta Platforms, Inc. and affiliates. All rights reserved.
%%%
%%% This source code is licensed under the Apache 2.0 license found in
%%% the LICENSE file in the root directory of this source tree.

-module(rebar_prv_build_info).

-behaviour(provider).

-export([init/1, do/1, format_error/1]).

-type app_build_opts() :: #{
    name := binary(),
    dir := binary(),
    ebin := binary(),
    src_dirs := [binary()],
    extra_src_dirs := [binary()],
    include_dirs := [binary()],
    macros := [atom() | {atom(), any()}],
    parse_transforms := [any()]
}.

init(State) ->
    State1 = rebar_state:add_provider(
        State,
        providers:create([
            {name, build_info},
            {module, ?MODULE},
            {bare, true},
            {deps, [app_discovery]},
            {example, "rebar3 build_info"},
            {short_desc, "Get build_info"},
            {desc, "Dump build information"},
            {opts, [
                {to, $t, "to", {string, undefined},
                    "Which file to dump build information to"}
            ]}
        ])
    ),
    {ok, State1}.

do(State0) ->
    {RawOpts, _} = rebar_state:command_parsed_args(State0),
    State1 = get_deps(State0),
    etf_build_info(State1, RawOpts),
    {ok, State1}.

format_error(Reason) ->
    io_lib:format("~p", [Reason]).

get_deps(State0) ->
    {ok, State1} = rebar_prv_install_deps:do(State0),
    {ok, State2} = rebar_prv_lock:do(State1),
    State2.

get_data(State) ->
    ProjectApps = rebar_state:project_apps(State),
    DepApps = rebar_state:all_deps(State),
    #{
        apps => [app_build_opts(App) || App <- ProjectApps],
        deps => [app_build_opts(App) || App <- DepApps],
        otp_lib_dir => list_to_binary(code:lib_dir()),
        source_root => list_to_binary(rebar_state:dir(State))
    }.

etf_build_info(State, RawOpts) ->
    Data = get_data(State),

    To = proplists:get_value(to, RawOpts),
    case To of
        undefined ->
            rebar_log:log(info, "Build info:~n", []),
            io:fwrite("~p.~n", [Data]);
        File ->
            ok = file:write_file(File, term_to_binary(Data)),
            rebar_log:log(info, "Build info written to: ~ts", [File])
    end.

%% From rebar_compiler:context/1

-spec app_build_opts(rebar_app_info:t()) -> app_build_opts().
app_build_opts(AppInfo) ->
    Name = rebar_app_info:name(AppInfo),
    AppDir = rebar_app_info:dir(AppInfo),
    EbinDir = rebar_app_info:ebin_dir(AppInfo),
    RebarOpts = rebar_app_info:opts(AppInfo),
    SrcDirs = rebar_dir:src_dirs(RebarOpts, ["src"]),
    ExistingSrcDirs = [Dir || Dir <- SrcDirs, ec_file:is_dir(filename:join(AppDir, Dir))],
    ExtraSrcDirs = rebar_dir:extra_src_dirs(RebarOpts),
    ErlOpts = rebar_opts:erl_opts(RebarOpts),
    ErlOptIncludes = proplists:get_all_values(i, ErlOpts),
    InclDirs =
        [filename:join(AppDir, "include")] ++ [filename:absname(Dir) || Dir <- ErlOptIncludes],
    PTrans = proplists:get_all_values(parse_transform, ErlOpts),
    Macros = macros(ErlOpts),

    #{
        name => Name,
        dir => list_to_binary(AppDir),
        ebin => list_to_binary(EbinDir),
        src_dirs => lists_to_binaries(ExistingSrcDirs),
        extra_src_dirs => lists_to_binaries(ExtraSrcDirs),
        include_dirs => lists_to_binaries(InclDirs),
        macros => Macros,
        parse_transforms => PTrans
    }.

macros([{d, Name} | Rest]) -> [Name | macros(Rest)];
macros([{d, Name, Value} | Rest]) -> [{Name, Value} | macros(Rest)];
macros([_ | Rest]) -> macros(Rest);
macros([]) -> [].

lists_to_binaries(Strings) ->
    [list_to_binary(String) || String <- Strings].
