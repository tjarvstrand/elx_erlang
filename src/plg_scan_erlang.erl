%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% @doc
%%% @end
%%% @author Thomas Järvstrand <tjarvstrand@gmail.com>
%%% @copyright
%%% Copyright 2012 Thomas Järvstrand <tjarvstrand@gmail.com>
%%%
%%% This file is part of parse_lib_grammars.
%%%
%%% parse_lib_grammars is free software: you can redistribute it and/or modify
%%% it under the terms of the GNU General Public License as published by
%%% the Free Software Foundation, either version 3 of the License, or
%%% (at your option) any later version.
%%%
%%% parse_lib_grammars is distributed in the hope that it will be useful,
%%% but WITHOUT ANY WARRANTY; without even the implied warranty of
%%% MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
%%% GNU General Public License for more details.
%%%
%%% You should have received a copy of the GNU General Public License
%%% along with parse_lib_grammars. If not, see <http://www.gnu.org/licenses/>.
%%% @end
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%%_* Module declaration =======================================================
-module(plg_scan_erlang).

%%%_* Exports ==================================================================
-export([grammar/0,

        atom/3]).

%%%_* Includes =================================================================

-include_lib("eunit/include/eunit.hrl").

%%%_* Defines ==================================================================

%%%_* Types ====================================================================

%%%_* API ======================================================================

grammar() ->
    [{"[a-z][a-zA-Z0-9_@]*", fun atom/3}].

%%%_* Internal functions =======================================================

atom(Chars, Start, End) ->
    {token, parse_lib_scan:token(list_to_atom(Chars), Chars, Start, End)}.

%%%_* Tests ====================================================================

grammar_symbol_test_() ->
    [ ?_assertEqual({ok,
                     [parse_lib_scan:token(a,
                                           "a",
                                           parse_lib_scan:point(1, 1, 1),
                                           parse_lib_scan:point(1, 1, 1))]},
                    parse_lib_scan:string("a", grammar())),
      ?_assertEqual({ok,
                     [parse_lib_scan:token('and',
                                           "and",
                                           parse_lib_scan:point(1, 1, 1),
                                           parse_lib_scan:point(3, 1, 3))]},
                    parse_lib_scan:string("and", grammar()))
    ].

%%%_* Test helpers =============================================================

%%%_* Emacs ====================================================================
%%% Local Variables:
%%% allout-layout: t
%%% erlang-indent-level: 2
%%% End:
