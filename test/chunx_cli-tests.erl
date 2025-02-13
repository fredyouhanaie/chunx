%%%-------------------------------------------------------------------
%%% @author Fred Youhanaie <fyrlang@anydata.co.uk>
%%% @copyright 2025, Fred Youhanaie
%%% @doc
%%%
%%% Unit tests for the `chunx_cli' module.
%%%
%%% @end
%%% Created : 13 Feb 2025 by Fred Youhanaie <fyrlang@anydata.co.uk>
%%%-------------------------------------------------------------------
-module('chunx_cli-tests').

-include_lib("eunit/include/eunit.hrl").

-include_lib("chunx_cli.hrl").

%%--------------------------------------------------------------------

-define(Args_cmds, #{arguments => ?Args_chunx_cli,
                     commands  => ?Cmds_chunx_cli}).

%%--------------------------------------------------------------------

chunx_cli_1_test_() ->
    {"chunx_cli general tests",
     [ { "cli_args_cmds/0 returns map of args+commands",
         ?_assertEqual(?Args_cmds, chunx_cli:cli()) }
     ] }.

%%--------------------------------------------------------------------
