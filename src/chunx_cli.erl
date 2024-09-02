%%%-------------------------------------------------------------------
%%% @author Fred Youhanaie <fyrlang@anydata.co.uk>
%%% @copyright 2024, Fred Youhanaie
%%% @doc
%%%
%%% The escript for the `chunx' command line interface.
%%%
%%% The runnable escript will be called `chunx'.
%%%
%%% @end
%%% Created : 01 Sep 2024 by Fred Youhanaie <fyrlang@anydata.co.uk>
%%%-------------------------------------------------------------------
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
             long => "-verbose", short => $v, type => boolean, action => count },
          #{ name => help, help => "display help/usage information",
             long => "-help", short => $h, type => boolean}
        ]).

-define(Commands,
        #{ "list"    => #{ help => "produce a list of module names",
                           handler => fun do_list/1 },
           "man"     => #{ help => "generate per module mardown pages suitable for pandoc",
                           handler => fun do_man/1 },
           "summary" => #{ help => "produce per module summaries",
                           handler => fun do_summary/1 },
           "help"    => #{ help => "display help/usage information",
                           handler => fun do_help/1 }
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

do_summary(_A) ->
    io:format("summary: not implemented yet.~n"),
    ok.

%---------------------------------------------------------------------

do_help(_A) ->
    io:format("help: not implemented yet.~n"),
    ok.

%---------------------------------------------------------------------
