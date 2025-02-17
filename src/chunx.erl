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
-export([chunk_info/1]).
-export([get_docs_from_beam/1, chunk_info_from_beam/1, beam_chunk_to_map/1]).
-export([get_docs_from_source/1]).
-export([get_docs_from_chunk/1]).
-export([untuplize/1]).

%%--------------------------------------------------------------------

-include_lib("kernel/include/logger.hrl").

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
    %% skip the generated docs
    case is_map_key(generated, Metadata) of
        true  -> #{};
        false -> M
    end.

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
-spec has_doc(Mod::module()) -> false | true.
has_doc(Mod) ->
    case code:get_doc(Mod) of
        {ok, {docs_v1, _, _, _, _, Metadata, _}} ->
            %% we treat `generated' docs as `false'
            not maps:get(generated, Metadata, false);
        _ ->
            false
    end.

%%--------------------------------------------------------------------
%% @doc return a list of all the available modules.
%%
%% Returns a unique sorted list of module names, whether they have
%% embedded docs or not..
%%
%% @end
%%--------------------------------------------------------------------
-spec all_mods() -> [module()].
all_mods() ->
    lists:usort([ list_to_atom(M)
                  || {M, _, _} <- code:all_available() ]).

%%--------------------------------------------------------------------
%% @doc extract the summary info of a chunk
%%
%% returns the module name, the language and format as a map. If error
%% in retrieving the module chunk, we return an empty map.
%%
%% @end
%%--------------------------------------------------------------------
-spec chunk_info(atom() | map()) -> map().
chunk_info(Mod_name) when is_atom(Mod_name) ->
    case chunk_to_map(Mod_name) of
        {ok, Mod_map} ->
            chunk_info(Mod_map);
        {error, _Error} ->
            #{}
    end;

chunk_info(Chunk_map) when is_map(Chunk_map) ->
    F = fun (K, _V) -> lists:member(K, [mod, lang, frmt]) end,
    maps:filter(F, Chunk_map).

%%--------------------------------------------------------------------
%% @doc check for and extract the doc chunk from a beam file
%%
%% The doc chunk will be returned as a map. If it is missing, an empty
%% map is returned.
%%
%% @end
%%--------------------------------------------------------------------
-spec chunk_info_from_beam(file:filename_all()) -> map().
chunk_info_from_beam(File) ->
    case get_docs_from_beam(File) of
        {ok, {Mod, Chunk}} ->
            Chunk_map = chunk_to_map(Mod, Chunk),
            chunk_info(Chunk_map);
        Error ->
            ?LOG_WARNING("could not read beam file: ~p.", [Error]),
            #{}
    end.

%%--------------------------------------------------------------------
%% @doc get the doc chunks from the beam files
%%
%% @end
%%--------------------------------------------------------------------
-spec get_docs_from_beam(file:filename_all()) ->
          {error, missing_chunk} | {ok, {module(), tuple()}}.
get_docs_from_beam(File) ->
    case beam_lib:chunks(File, [documentation]) of
        {ok, {Mod, [{documentation, Chunk}]}} ->
            {ok, {Mod, Chunk}};

        _Error ->
            {error, bad_beam}
    end.

%%--------------------------------------------------------------------
%% @doc generate chunk map from a beam file
%%
%% If the beam file does not contain a docs chunk, an empty map is
%% returned.
%%
%% @end
%%--------------------------------------------------------------------
-spec beam_chunk_to_map(file:filename_all()) -> map().
beam_chunk_to_map(File) ->
    case get_docs_from_beam(File) of
        {error, _} ->
            #{};

        {ok, {Mod, Chunk}} ->
            chunk_to_map(Mod, Chunk)
    end.

%%--------------------------------------------------------------------
%% @doc get the doc chunks from the source file
%%
%% This is equivalent to the edoc command:
%%
%% `edoc -chunks files FILENAME'
%%
%% The chunk file will end up in a subdirectory called `./chunks/'.
%%
%% @end
%%--------------------------------------------------------------------
-spec get_docs_from_source(file:filename_all()) -> ok.
get_docs_from_source(File) ->
    Opts = [ {doclet, edoc_doclet_chunks},
             {layout, edoc_layout_chunks},
             {preprocess, true} ],
    edoc:files([File], Opts).

%%--------------------------------------------------------------------
%% @doc get the doc chunks from the chunk file
%%
%% We expect the filename to be of the form `DIR/MODULE.chunk'.
%%
%% @end
%%--------------------------------------------------------------------
-spec get_docs_from_chunk(file:filename_all()) ->
          {error, term()} | {ok, module(), map()}.
get_docs_from_chunk(File) ->
    case file:read_file(File) of
        {ok, Bin} ->
            Mod = list_to_atom(filename:basename(File, ".chunk")),
            {ok, Mod, binary_to_term(Bin)};
        Error ->
            Error
    end.

%%--------------------------------------------------------------------
%% @doc recursively convert all tuples in `Term' to lists
%%
%% This is for the benefit of `json:encode/1', which cannot handle
%% tuples.
%%
%% Currently, we do not touch map keys. This should be OK for our use
%% case since the map keys are atoms.
%%
%% @end
%%--------------------------------------------------------------------
-spec untuplize(term()) -> term().
untuplize(Term) when is_tuple(Term) ->
    untuplize(tuple_to_list(Term));

untuplize(Term) when is_list(Term) ->
    [ untuplize(T) || T <- Term ];

untuplize(Term) when is_map(Term) ->
    F = fun(_K, V) -> untuplize(V) end,
    maps:map(F, Term);

untuplize(Term) ->
    Term.

%%--------------------------------------------------------------------
