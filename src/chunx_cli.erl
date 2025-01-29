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

-type(mod_source()) :: all | file | modules | chunks.

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

-spec do_list(map()) -> ok.
do_list(Args) ->
    check_verbosity(Args),
    case get_mods_source(Args) of
        {ok, Source} ->
            Mods = get_mod_names(Source, Args),
            case maps:get(json, Args, false) of
                true ->
                    io:format("~s~n", [json:encode(Mods)]);
                false ->
                    [ io:format("~p~n", [M]) || M <- Mods ]
            end,
            ok;
        {error, Error} ->
            ?LOG_ERROR(Error),
            error
    end.

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
%% Return the list of modules for the supplied source
%%
-spec get_mod_names(mod_source(), map()) -> [module()].
get_mod_names(all, _Args) ->
    chunx:all_mods();

get_mod_names(file, Args) ->
    case file:consult(map_get(file, Args)) of
        {ok, Mods} ->
            Mods;
        {error, Error} ->
            ?LOG_ERROR("could not read list of mods: ~p.", [Error]),
            []
    end;

get_mod_names(modules, Args) ->
    [ list_to_atom(M) || M <- map_get(modules, Args) ];

get_mod_names(chunks, Args) ->
    Chunk_files = map_get(chunks, Args),
    [ list_to_atom(filename:basename(C, ".chunk")) || C <- Chunk_files ].

%%--------------------------------------------------------------------
%% Get/check the source of modules option
%%
-spec get_mods_source(map()) -> {ok, mod_source()} | {error, term()}.
get_mods_source(Args) ->
    Options = lists:sort([all, file, modules, chunks]),
    Actuals = lists:sort(maps:keys(Args)),
    Sources = lists:filter(fun (X) -> lists:member(X, Options) end,
                           Actuals),
    case length(Sources) of
        0 -> %% default
            {ok, all};
        1 ->
            {ok, hd(Sources)};
        _ ->
            {error, "only one of 'all', 'file', 'modules' or 'chunks' allowed"}
    end.

%%--------------------------------------------------------------------
