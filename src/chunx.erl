%%%-------------------------------------------------------------------
%%% @author Fred Youhanaie <fyrlang@anydata.co.uk>
%%% @copyright 2024, Fred Youhanaie
%%% @doc
%%%
%%% Utility functions for handling EEP-48 document chunks.
%%%
%%% @end
%%% Created : 2024-06-30 by Fred Youhanaie <fyrlang@anydata.co.uk>
%%%-------------------------------------------------------------------
-module(chunx).

-export([chunk_to_map/1, chunk_to_map/2, has_doc/1]).
-export([all_mods/0]).

%%--------------------------------------------------------------------
%% @doc Return the docs for module `Mod' as a map
%%
%% The documentaion for a single module is returned as a map. The list
%% of individual function/type docs is also converted to a list of
%% maps.
%%
%% @end
%%--------------------------------------------------------------------
-spec chunk_to_map(Mod::module()) -> {ok, map()} | {error, term()}.
chunk_to_map(Mod) ->
    case code:get_doc(Mod) of
        {ok, Chunk} ->
            {ok, chunk_to_map(Mod, Chunk)};
        Error ->
            Error
    end.

%%--------------------------------------------------------------------
%% @doc convert the module doc chunk to a map.
%%
%% @end
%%--------------------------------------------------------------------
-spec chunk_to_map(Mod::module(), tuple()) -> map().
chunk_to_map(Mod, Chunk) ->
    {docs_v1, Anno, Beam_lang, Format, Mod_docs, Metadata, Docs} = Chunk,
    M = #{ mod   => Mod,
           anno  => Anno,
           lang  => Beam_lang,
           frmt  => Format,
           mdoc  => Mod_docs,
           mdata => Metadata,
           docs  => docs_to_map(Docs, []) },
    M.

%%--------------------------------------------------------------------
%% @doc convert the list of doc tuples to a list of maps.
%%
%% Each list element corresponds to the documentation of a module
%% element, such as a `function', `type' or `callback'.
%%
%% @end
%%--------------------------------------------------------------------
-spec docs_to_map([tuple()], [map()]) -> [map()].
docs_to_map([], Map) ->
    Map;

docs_to_map([D|Docs], Maps) ->
    {KNA, Anno, Sig, Doc, Metadata} = D,
    M = #{ kna   => KNA, %% {Kind, Name, Arity}
           anno  => Anno,
           sig   => Sig,
           doc   => Doc,
           mdata => Metadata },
    docs_to_map(Docs, [M|Maps]).

%%--------------------------------------------------------------------
%% @doc Returns true if module `Mod' has a document chunk.
%%
%% An expected use for this is in a `filtermap' for collecting
%% multiple module docs from a list of module names.
%%
%% @end
%%--------------------------------------------------------------------
-spec has_doc(Mod::module()) -> false | {true, term()}.
has_doc(Mod) ->
    case code:get_doc(Mod) of
        {ok, Doc} ->
            {true, Doc};
        _ ->
            false
    end.

%%--------------------------------------------------------------------
%% return a list of all the modules that have EEP-48 style docs
%%
all_mods() ->
    All_mods = lists:usort([ M || {M, _} <- code:all_loaded() ]),
    F = fun(M) ->
                case code:get_doc(M) of
                    {ok, _} -> true;
                    _ -> false
                end
        end,
    lists:filter(F, All_mods).

%%--------------------------------------------------------------------
