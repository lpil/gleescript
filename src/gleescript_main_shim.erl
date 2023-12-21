-module(gleescript_main_shim).

-export([main/1]).

main(_) ->
    io:setopts(standard_io, [binary, {encoding, utf8}]),
    io:setopts(standard_error, [{encoding, utf8}]),
    (list_to_atom(os:getenv("GLEESCRIPT_MAIN"))):main().
