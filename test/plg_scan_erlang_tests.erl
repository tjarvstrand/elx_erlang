%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% @doc Tests for plg_scan_erlang
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
-module(plg_scan_erlang_tests).

%%%_* Includes =================================================================

-include_lib("eunit/include/eunit.hrl").

%%%_* Tests ====================================================================

symbol_test_() ->
  [?_assertEqual({ok, [token(atom, a, "a", {1, 1, 1}, {1, 1, 1})]},
                 scan("a")),
   ?_assertEqual({ok, [token(keyword, 'and', "and", {1, 1, 1}, {3, 1, 3})]},
                 scan("and")),
   ?_assertEqual({ok, [token(atom, 'fand', "fand", {1, 1, 1}, {4, 1, 4})]},
                 scan("fand")),
   ?_assertEqual({ok, [token(variable, 'Foo', "Foo", {1, 1, 1}, {3, 1, 3})]},
                 scan("Foo")),
   ?_assertEqual({error, {syntax_error, {{1, 1, 1}, "1foo"}}},
                 scan("1foo"))
  ].

string_test_() ->
  [?_assertEqual({ok, [token(atom, a, "'a'", {1, 1, 1}, {3, 1, 3})]},
                 scan("'a'")),
   ?_assertEqual({ok, [token(string, "a", "\"a\"", {1, 1, 1}, {3, 1, 3})]},
                 scan("\"a\""))
  ].

integer_test_() ->
  [?_assertEqual({ok, [token(integer, 16, "16", {1, 1, 1}, {2, 1, 2})]},
                 scan("16")),
   ?_assertEqual({ok, [token(integer, -16, "-16", {1, 1, 1}, {3, 1, 3})]},
                 scan("-16")),
   ?_assertEqual({ok, [token(integer, 22, "16#16", {1, 1, 1}, {5, 1, 5})]},
                 scan("16#16")),
   ?_assertEqual({ok, [token(integer, -22, "-16#16", {1, 1, 1}, {6, 1, 6})]},
                 scan("-16#16")),
   %% Note: The below is slighly different behaviour from the OTP compiler which
   %% would give a bad_integer error.
   ?_assertEqual({error, {syntax_error, {{3, 1, 3}, "#-16"}}},
                 scan("16#-16")),
   ?_assertEqual({error, {{illegal_base, 128}, {{1, 1, 1}, "128#16"}}},
                 scan("128#16"))
  ].

float_test_() ->
  [?_assertEqual({ok, [token(float, 16.15, "16.15", {1, 1, 1}, {5, 1, 5})]},
                 scan("16.15")),
   ?_assertEqual({ok, [token(float, 1.2e4, "1.2e4", {1, 1, 1}, {5, 1, 5})]},
                 scan("1.2e4")),
   ?_assertEqual({ok, [token(float, 1.2e-4, "1.2e-4", {1, 1, 1}, {6, 1, 6})]},
                 scan("1.2e-4"))
  ].

character_test_() ->
  [?_assertEqual({ok, [token(character, $a, "$a", {1, 1, 1}, {2, 1, 2})]},
                 scan("$a")),
   ?_assertEqual({ok, [token(character, $a, "$\\a", {1, 1, 1}, {3, 1, 3})]},
                 scan("$\\a")),
   ?_assertEqual({ok, [token(character, $a, "$\\a", {1, 1, 1}, {3, 1, 3})]},
                 scan("$\\\a")),
   ?_assertEqual({ok, [token(character, $\", "$\"", {1, 1, 1}, {2, 1, 2})]},
                 scan("$\"")),
   ?_assertEqual({ok, [token(character, $\n, "$\\n", {1, 1, 1}, {3, 1, 3})]},
                 scan("$\\n")),
   ?_assertEqual({ok, [token(character, $\^t, "$\\^t", {1, 1, 1}, {4, 1, 4})]},
                 scan("$\\^t")),
   ?_assertEqual({error, {syntax_error, {{1, 1, 1}, "$"}}},
                 scan("$")),
   ?_assertEqual({ok, [token(character, $\123, "$\\123", {1, 1, 1}, {5, 1, 5})]},
                 scan("$\\123"))
  ].

punctuation_test_() ->
  [?_assertEqual({ok, [token(punctuation, '(', "(", {1, 1, 1}, {1, 1, 1})]},
                 scan("(")),
   ?_assertEqual({ok, [token(punctuation, '#{', "#{", {1, 1, 1}, {2, 1, 2})]},
                 scan("#{")),
   ?_assertEqual({ok, [token(punctuation, '#{', "#  {", {1, 1, 1}, {4, 1, 4})]},
                 scan("#  {"))
  ].

%%%_* Test helpers =============================================================

scan(Str) ->
    %% DebugF = fun(Fmt0, Args) ->
    %%                  io:fwrite(user, list_to_binary(Fmt0 ++ "~n"), Args)
    %%          end,
  parse_lib_scan:string(Str,
                        plg_scan_erlang:grammar(),
                        [{re_groups, named_only}
                         %% {debug_fun, DebugF},
                         %% {debug, true}
                        ]
                       ).

token(Type,
      Term,
      Str,
      {StartIndex, StartLine, StartCol},
      {EndIndex, EndLine, EndCol}) ->
  parse_lib_scan:token(Type,
                       Term,
                       Str,
                       parse_lib_scan:point(StartIndex, StartLine, StartCol),
                       parse_lib_scan:point(EndIndex, EndLine, EndCol)).

%%%_* Emacs ====================================================================
%%% Local Variables:
%%% allout-layout: t
%%% erlang-indent-level: 2
%%% End:
