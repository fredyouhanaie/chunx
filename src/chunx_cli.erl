-module(chunx_cli).

%% API exports
-export([main/1]).

-define(Arguments,
        [ #{ name => all,     long => "all",        short => $a, type => boolean },
          #{ name => file,    long => "from-file",  short => $f, type => string },
          #{ name => module,  long => "module",     short => $m, type => string },
          #{ name => chunk,   long => "chunk-file", short => $c, type => string },
          #{ name => json,    long => "json",       short => $j, type => string },
          #{ name => quiet,   long => "quiet",      short => $q, type => boolean },
          #{ name => verbose, long => "verbose",    short => $v, type => boolean }
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
