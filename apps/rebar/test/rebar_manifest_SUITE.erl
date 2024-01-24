-module(rebar_manifest_SUITE).

-export([all/0,
         init_per_testcase/2,
         end_per_testcase/2,
         main/1]).

-include_lib("common_test/include/ct.hrl").

all() -> [main].

init_per_testcase(Case, Config0) ->
    %% Create a project directory in the test run's priv_dir
    Config = rebar_test_utils:init_rebar_state(Config0),
    %% Create toy applications
    AppDir = ?config(apps, Config),
    Name = rebar_test_utils:create_random_name("app1_"++atom_to_list(Case)),
    Vsn = rebar_test_utils:create_random_vsn(),
    rebar_test_utils:create_app(AppDir, Name, Vsn, [kernel, stdlib]),
    %% Add the data to the test config
    [{name, Name} | Config].

end_per_testcase(_, Config) ->
    Config.

main(Config) ->
    Name = ?config(name, Config),
    rebar_test_utils:run_and_check(Config, [],
                                   ["manifest"],
                                   {ok, [{app, Name}]}).
