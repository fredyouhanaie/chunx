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
         ?_assertMatch({true, _}, chunx:has_doc(lists)) },
       { "has_doc/1 fictitious module",
         ?_assertMatch(false, chunx:has_doc(lliissttss)) },
       { "chunk_to_map/1 existing module",
         ?_assertMatch({ok, M} when is_map(M), chunx:chunk_to_map(lists)) },
       { "chunk_to_map/1 fictitious module",
         ?_assertMatch({error, _}, chunx:chunk_to_map(lliissttss)) }
     ]}.

%%--------------------------------------------------------------------
