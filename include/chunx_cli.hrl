%%%-------------------------------------------------------------------
%%% @author Fred Youhanaie <fyrlang@anydata.co.uk>
%%% @copyright 2025, Fred Youhanaie
%%% @doc
%%%
%%% Basic definitions used within `chunx_cli'.
%%%
%%% @end
%%% Created : 15 Jan 2025 by Fred Youhanaie <fyrlang@anydata.co.uk>
%%%-------------------------------------------------------------------

%%--------------------------------------------------------------------
%% Main command arguments (see `argparse')
%%
-define(Args_chunx_cli,
        [
         %% selection of modules, only one is allowed
         #{ name => all, help => "process all the modules with docs (default)",
            long => "-all", short => $a, type => boolean },
         #{ name => file, help => "take the list of modules from file",
            long => "-from-file", short => $f, type => string },
         #{ name => modules, help => "use the list of modules",
            long => "-modules", short => $m, type => string, nargs => nonempty_list },

         %% source of doc chunks, only one is allowed
         #{ name => source, help => "source of docs, chunk/erl/beam files (default=all available modules)",
            long => "-source", short => $s, type => {binary, [<<"chunk">>, <<"beam">>, <<"erl">>]} },
         #{ name => docsources, help => "chunk/erl/beam files",
            long => "-doc-sources", short => $d, type => string, nargs => nonempty_list },

         %% output format
         #{ name => json, help => "produce JSON output",
            long => "-json", short => $j, type => boolean },

         #{ name => verbose, help => "be verbose, can use multiple times for warning..debug",
            long => "-verbose", short => $v, type => boolean, action => count }
        ]).

%%--------------------------------------------------------------------
%% CLI (sub)commands (see `argparse')
%%
-define(Cmds_chunx_cli,
        #{ "list"    => #{ help => "produce a list of module names",
                           handler => fun chunx_cli:do_list/1 },
           "man"     => #{ help => "generate per module mardown pages suitable for pandoc",
                           handler => fun chunx_cli:do_man/1 },
           "summary" => #{ help => "produce per module summaries",
                           handler => fun chunx_cli:do_summary/1 },
           "docs"    => #{ help => "produce per module docs",
                           handler => fun chunx_cli:do_docs/1 }
         }).

%%--------------------------------------------------------------------
