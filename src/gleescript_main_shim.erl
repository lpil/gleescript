-module(gleescript_main_shim).

-export([main/1]).

main(_) ->
    io:setopts(standard_io, [binary, {encoding, utf8}]),
    io:setopts(standard_error, [{encoding, utf8}]),
    ApplicationModule = list_to_atom(os:getenv("GLEESCRIPT_MAIN")),
    {ok, _} = application:ensure_all_started(ApplicationModule),
    ApplicationModule:main().
