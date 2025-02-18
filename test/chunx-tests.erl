%%%-------------------------------------------------------------------
%%% @author Fred Youhanaie <fyrlang@anydata.co.uk>
%%% @copyright 2024, Fred Youhanaie
%%% @doc
%%%
%%% Unit tests for the `chunx' module.
%%%
%%% @end
%%% Created : 28 Sep 2024 by Fred Youhanaie <fyrlang@anydata.co.uk>
%%%-------------------------------------------------------------------

-module('chunx-tests').

-include_lib("eunit/include/eunit.hrl").

%%--------------------------------------------------------------------

chunx_test_() ->
    {"chunx general tests",
     [ { "all_mods/0 returns list",
         ?_assertMatch(Mods when is_list(Mods), chunx:all_mods()) },
       { "has_doc/1 existing module",
         ?_assertMatch(true, chunx:has_doc(lists)) },
       { "has_doc/1 fictitious module",
         ?_assertMatch(false, chunx:has_doc(lliissttss)) },
       { "chunk_to_map/1 generated module",
         ?_assertEqual({ok, #{}}, chunx:chunk_to_map(chunx)) },
       { "chunk_to_map/1 existing module",
         ?_assertMatch({ok, M} when is_map(M), chunx:chunk_to_map(lists)) },
       { "chunk_to_map/1 fictitious module",
         ?_assertMatch({error, _}, chunx:chunk_to_map(lliissttss)) }
     ]}.

%%--------------------------------------------------------------------

chunk_info_test_() ->
    {"chunk info/summary tests",
     [ { "chunk_info by module, missing chunk",
         ?_assertEqual(#{}, chunx:chunk_info(chunx)) },
       { "chunk_info by module, containing chunk",
         ?_assertEqual(#{mod  => lists,
                         frmt => <<"text/markdown">>,
                         lang => erlang},
                       chunx:chunk_info(lists)) }
     ] }.

%%--------------------------------------------------------------------

chunk_beam_test_() ->
    {"chunk docs from beam file",
     [ { "docs from bad beam file",
         ?_assertEqual({error, bad_beam},
                       chunx:get_docs_from_beam("bad_mod.beam")) },

       { "docs from beam file - missing doc chunk",
         ?_assertEqual({error, bad_beam},
                       chunx:get_docs_from_beam(code:which(chunx))) },

       { "docs from good beam file",
         ?_assertMatch({ok, {lists, _}},
                       chunx:get_docs_from_beam(code:which(lists))) },

       { "map from beam file - bad file",
         ?_assertEqual(#{},
                       chunx:beam_chunk_to_map("bad_mod.beam")) },

       { "map from beam file - missing doc chunk",
         ?_assertEqual(#{},
                       chunx:beam_chunk_to_map(code:which(chunx))) },

       { "map from good beam file",
         ?_assertMatch(#{mod := lists},
                       chunx:beam_chunk_to_map(code:which(lists))) }

     ] }.

%%--------------------------------------------------------------------

chunk_info_beam_test_() ->
    {"chunk info/summary from beam tests",
     [ { "chunk_info by module, missing chunk",
         ?_assertEqual(#{},
                       chunx:chunk_info_from_beam(code:which(chunx))) },

       { "chunk_info by module, containing chunk",
         ?_assertEqual(#{mod  => lists,
                         frmt => <<"text/markdown">>,
                         lang => erlang},
                       chunx:chunk_info_from_beam(code:which(lists))) }
     ] }.

%%--------------------------------------------------------------------

untuplize_test_() ->
    {"untuplize tests",
     [ { "number",
         ?_assertEqual(42, chunx:untuplize(42)) },
       { "atom",
         ?_assertEqual(an_atom, chunx:untuplize(an_atom)) },
       { "string",
         ?_assertEqual("a string", chunx:untuplize("a string")) },
       { "list",
         ?_assertEqual([1, 2, 3, 4], chunx:untuplize([1, 2, 3, 4])) },
       { "tuple",
         ?_assertEqual([a, b, c], chunx:untuplize({a, b, c})) },
       { "nested tuples",
         ?_assertEqual([a, [b, c], d], chunx:untuplize({a, {b, c}, d})) },
       { "map with tuples",
         ?_assertEqual(#{nums => [1, 2, 3], abc => [a, b, c]},
                       chunx:untuplize(#{nums => {1, 2, 3}, abc => {a, b, c}})) }
     ] }.

%%--------------------------------------------------------------------
