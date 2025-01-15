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

-include_lib("chunx_cli.hrl").

%%--------------------------------------------------------------------

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
    #{ arguments => ?Args_chunx_cli,
       commands  => ?Cmds_chunx_cli
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
