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

%%--------------------------------------------------------------------

-include_lib("kernel/include/logger.hrl").

%%--------------------------------------------------------------------

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
          #{ name => verbose, help => "be verbose, can use multiple times for warning..debug",
             long => "-verbose", short => $v, type => boolean, action => count }
        ]).

-define(Commands,
        #{ "list"    => #{ help => "produce a list of module names",
                           handler => fun do_list/1 },
           "man"     => #{ help => "generate per module mardown pages suitable for pandoc",
                           handler => fun do_man/1 },
           "summary" => #{ help => "produce per module summaries",
                           handler => fun do_summary/1 }
         }).

-define(Progname, #{progname => chunx}).

%%====================================================================
%% API functions
%%====================================================================

main(Args) ->
    %% set up default logger (single line)
    logger:set_handler_config(default, formatter, {logger_formatter, #{}}),
    logger:set_primary_config(level, error),

    %% scan the args and run
    argparse:run(Args, cli(), ?Progname),

    timer:sleep(100), %% give the logger a chance to flush all the messages!!
    ok.

%%====================================================================
%% Internal functions
%%====================================================================

cli() ->
    #{ arguments => ?Arguments,
       commands  => ?Commands
     }.

%%--------------------------------------------------------------------

do_list(Args) ->
    check_verbosity(Args),
    [ io:format("~p~n", [M]) || M <- chunx:all_mods() ],
    ok.

%%--------------------------------------------------------------------

do_man(Args) ->
    check_verbosity(Args),
    io:format("man: not implemented yet.~n"),
    ok.

%%--------------------------------------------------------------------

do_summary(Args) ->
    check_verbosity(Args),
    io:format("summary: not implemented yet.~n"),
    ok.

%%--------------------------------------------------------------------

-spec check_verbosity(map()) -> ok.
check_verbosity(Args) ->
    %% check/set the verbosity
    Level = case maps:get(verbose, Args, 0) of
                0 -> error;
                1 -> warning;
                2 -> notice;
                3 -> info;
                _ -> debug
            end,
    logger:set_primary_config(level, Level),
    ?LOG_NOTICE(#{ arg_map => Args }).

%%--------------------------------------------------------------------
