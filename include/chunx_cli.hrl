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

%%--------------------------------------------------------------------
%% CLI (sub)commands (see `argparse')
%%
-define(Cmds_chunx_cli,
        #{ "list"    => #{ help => "produce a list of module names",
                           handler => fun do_list/1 },
           "man"     => #{ help => "generate per module mardown pages suitable for pandoc",
                           handler => fun do_man/1 },
           "summary" => #{ help => "produce per module summaries",
                           handler => fun do_summary/1 }
         }).

%%--------------------------------------------------------------------
