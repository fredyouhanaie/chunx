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
-export([do_docs/1, do_list/1, do_man/1, do_summary/1]).

%%--------------------------------------------------------------------

-include_lib("kernel/include/logger.hrl").

-include_lib("chunx_cli.hrl").

%%--------------------------------------------------------------------

-type(mod_choice()) :: all | file | modules.

%%--------------------------------------------------------------------

-define(Progname, #{progname => chunx}).

-define(Remove_empty_maps(Map_list),
        lists:filter(fun (M) -> M =/= #{} end, Map_list)).

-define(Mods_with_docs(Mods_list),
        lists:filter(fun chunx:has_doc/1, Mods_list)).

%%--------------------------------------------------------------------

-ifdef(EUNIT).
-export([cli/0]).
-endif.

%%====================================================================
%% API functions
%%====================================================================

main(Args) ->
    %% set up default logger (single line)
    ok = logger:set_handler_config(default, formatter, {logger_formatter, #{}}),
    ok = logger:set_primary_config(level, error),

    %% scan the args and run
    case argparse:run(Args, cli(), ?Progname) of
        {ok, Result, Args_map} ->
            print(Result, Args_map);

        {error, Err} ->
            ?LOG_ERROR(Err)
    end,

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
%% process and run the `list' subcommand
%%
-spec do_list(map()) -> {ok, [term()], map()} | {error, term()}.
do_list(Args) ->
    check_verbosity(Args),

    case check_args(Args) of
        {ok, _Source, Mods} when is_list(Mods) ->
            {ok, ?Mods_with_docs(Mods), Args};

        {ok, _Source, Mods_files} when is_map(Mods_files) ->
            {ok, ?Mods_with_docs(maps:keys(Mods_files)), Args};

        {error, Error} ->
            {error, Error}
    end.

%%--------------------------------------------------------------------
%% process and run the `man' subcommand
%%
-spec do_man(map()) -> {ok, [term()], map()} | {error, term()}.
do_man(Args) ->
    check_verbosity(Args),
    ?LOG_WARNING("man: not implemented yet."),
    {ok, [], Args}.

%%--------------------------------------------------------------------
%% process and run the `summary' subcommand
%%
-spec do_summary(map()) -> {ok, [term()], map()} | {error, term()}.
do_summary(Args) ->
    check_verbosity(Args),
    case check_args(Args) of
        {ok, available_mods, Mods} ->
            Mods_info = [ chunx:chunk_info(M) || M <- Mods ],
            {ok, ?Remove_empty_maps(Mods_info), Args};

        {ok, beam, Mods_files} ->
            Mods_info = [ chunx:chunk_info_from_beam(F) ||
                            {_M,F} <- maps:to_list(Mods_files) ],
            {ok, ?Remove_empty_maps(Mods_info), Args};

        {error, Error} ->
            {error, Error}
    end.

%%--------------------------------------------------------------------
%% print the per module docs
%%
-spec do_docs(map()) -> {ok, [term()], map()} | {error, term()}.
do_docs(Args) ->
    check_verbosity(Args),
    case check_args(Args) of
        {ok, available_mods, Mods} ->
            Mod_docs = [ D ||
                           {ok, D} <- [ chunx:chunk_to_map(M)
                                        || M <- Mods ]
                       ],
            {ok, ?Remove_empty_maps(Mod_docs), Args};

        {ok, beam, Mods_files} ->
            Mod_docs = [ chunx:beam_chunk_to_map(F) ||
                           {_M,F} <- maps:to_list(Mods_files) ],
            {ok, ?Remove_empty_maps(Mod_docs), Args};

        {error, Error} ->
            {error, Error}
    end.

%%--------------------------------------------------------------------
%% @doc print the generated data based on the command line flags
%%
%% We expect the data to be a list. For JSON, there cannot be any
%% `tuples' in the list.
%%
%% @end
%%--------------------------------------------------------------------
-spec print(list(), map()) -> ok.
print(Data, Args) ->
    case maps:get(json, Args, false) of
        true ->
            io:format("~s~n", [json:encode(chunx:untuplize(Data))]);
        false ->
            [ io:format("~p.~n", [M]) || M <- Data ],
            ok
    end.

%%--------------------------------------------------------------------
%% @doc check/set the requested level of verbosity on the command line
%%
%% The default is `error', each `-v' will increase it from `warning'
%% to `debug'.
%%
%% @end
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
    ok = logger:set_primary_config(level, Level),
    ?LOG_NOTICE(#{ arg_map => Args }),
    ok.

%%--------------------------------------------------------------------
%% Return the list of modules for the supplied choice
%%
-spec get_mod_names(mod_choice(), [module()], map()) -> [module()].
get_mod_names(all, Mods, _Args) ->
    Mods;

get_mod_names(file, Mods, Args) ->
    case file:consult(map_get(file, Args)) of
        {ok, Mods_choice} ->
            F = fun(X) -> lists:member(X, Mods_choice) end,
            lists:filter(F, Mods);
        {error, Error} ->
            ?LOG_ERROR("could not read list of mods: ~p.", [Error]),
            []
    end;

get_mod_names(modules, Mods, Args) ->
    Mods_choice = [ list_to_atom(M) || M <- map_get(modules, Args) ],
    F = fun(X) -> lists:member(X, Mods_choice) end,
    lists:filter(F, Mods).

%%--------------------------------------------------------------------
%% Get/check the choice of modules selection options
%%
-spec get_mods_choice(map()) -> {ok, mod_choice()} | {error, term()}.
get_mods_choice(Args) ->
    Options = lists:sort([all, file, modules]),
    Actuals = lists:sort(maps:keys(Args)),
    Choices = lists:filter(fun (X) -> lists:member(X, Options) end,
                           Actuals),
    case length(Choices) of
        0 -> %% default
            {ok, all};
        1 ->
            {ok, hd(Choices)};
        _ ->
            {error, "only one of 'all', 'file' or 'modules' allowed"}
    end.

%%--------------------------------------------------------------------
%% @doc get doc source type, and list of module/docs
%%
%% `--source' should be one of `chunk', `beam' or `erl'. We expect
%% appropriate filenames to be provided with `--doc-sources'.
%%
%% If `--source' is not given, then the currently available modules are
%% used, and `--doc-sources', if given, will be ignored.
%%
%% If `--doc-sources' is not given, then an empty list will be used,
%% which is basically useless! Any filenames with type mismatch will
%% be ignored.
%%
%% @end
%%--------------------------------------------------------------------
-spec check_args(map()) ->
          {ok, atom(), [module()] | #{module() => string()}} |
          {error, term()}.
check_args(Args) ->
    {Source, Mods_files} =
        case maps:get(source, Args, available_mods) of
            available_mods ->
                {available_mods, chunx:all_mods()};
            File_type ->
                SS = binary_to_atom(File_type),
                FF = maps:get(docsources, Args, []),
                Suffix = "." ++ binary_to_list(File_type),
                MM = [ list_to_atom(filename:basename(F, Suffix))|| F <- FF ],
                {SS, maps:from_list(lists:zip(MM, FF))}
        end,
    case get_mods_choice(Args) of
        {ok, Choice} ->
            case Source of
                available_mods when is_list(Mods_files) ->
                    MF2 = get_mod_names(Choice, Mods_files, Args);
                _ ->
                    Mods_choice = get_mod_names(Choice, maps:keys(Mods_files), Args),
                    MF2 = maps:with(Mods_choice, Mods_files)
            end,
            {ok, Source, MF2};
        Error ->
            Error
    end.

%%--------------------------------------------------------------------
