-module(chunx_cli).

%% API exports
-export([main/1]).

-define(Arguments,
        [ #{ name => all, help => "process all the modules with docs (default)",
             long => "-all", short => $a, type => boolean },
          #{ name => file, help => "take list of modules from file",
             long => "-from-file", short => $f, type => string },
          #{ name => modules, help => "use the list of modules",
             long => "-modules", short => $m, type => string, nargs => nonempty_list },
          #{ name => chunks, help => "use the list of chunk files",
             long => "-chunk-files", short => $c, type => string, nargs => nonempty_list },
          #{ name => json, help => "produce JSON output",
             long => "-json", short => $j, type => boolean },
          #{ name => quiet, help => "do not produce any verbose output",
             long => "-quiet", short => $q, type => boolean },
          #{ name => verbose, help => "be verbose, can use multiple times",
             long => "-verbose", short => $v, type => boolean, action => count }
        ]).

-define(Commands,
        #{ "list" => #{ handler => fun do_list/1 },
           "man"  => #{ handler => fun do_man/1 }
         }).

%%====================================================================
%% API functions
%%====================================================================

main(Args) ->
    argparse:run(Args, cli(), #{progname => chunx}).

%%====================================================================
%% Internal functions
%%====================================================================

cli() ->
    #{ arguments => ?Arguments,
       commands  => ?Commands
     }.

%---------------------------------------------------------------------

do_list(_A) ->
    [ io:format("~p~n", [M]) || M <- chunx:all_mods() ],
    ok.

%---------------------------------------------------------------------

do_man(_A) ->
    io:format("man: not implemented yet.~n"),
    ok.

%---------------------------------------------------------------------
