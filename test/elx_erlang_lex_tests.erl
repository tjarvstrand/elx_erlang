%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% @doc Tests for elx_erlang_lex
%%% @end
%%% @author Thomas Järvstrand <tjarvstrand@gmail.com>
%%% @copyright
%%% Copyright 2012 Thomas Järvstrand <tjarvstrand@gmail.com>
%%%
%%% This file is part of ELX Erlang.
%%%
%%% ELX Erlang is free software: you can redistribute it and/or modify
%%% it under the terms of the GNU General Public License as published by
%%% the Free Software Foundation, either version 3 of the License, or
%%% (at your option) any later version.
%%%
%%% ELX Erlang is distributed in the hope that it will be useful,
%%% but WITHOUT ANY WARRANTY; without even the implied warranty of
%%% MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
%%% GNU General Public License for more details.
%%%
%%% You should have received a copy of the GNU General Public License
%%% along with ELX Erlang. If not, see <http://www.gnu.org/licenses/>.
%%% @end
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%%_* Module declaration =======================================================
-module(elx_erlang_lex_tests).

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
                 scan("1foo")),
   ?_assertEqual({error, {syntax_error, {{3, 1, 3}, "2foo"}}},
                 scan("1.2foo"))
  ].

string_test_() ->
  [?_assertEqual({ok, [token(atom, a, "'a'", {1, 1, 1}, {3, 1, 3})]},
                 scan("'a'")),
   ?_assertEqual({ok,
                  [token(string, "a\nb", "\"a\nb\"", {1, 1, 1}, {5, 2, 2})]},
                 scan("\"a\nb\"")),
   ?_assertEqual({ok, [token(string, "a", "\"a\"", {1, 1, 1}, {3, 1, 3})]},
                 scan("\"a\"")),
   ?_assertEqual({ok, [token(string,
                             "a\\\"b",
                             "\"a\\\"b\"",
                             {1, 1, 1},
                             {6, 1, 6})]},
                 scan("\"a\\\"b\"")),
   ?_assertEqual({error, {unterminated_quote, {{5, 1, 5}, "\""}}},
                 scan("\"a\"b\""))
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
   ?_assertEqual({ok,
                  [token(character, $\123, "$\\123", {1, 1, 1}, {5, 1, 5})]},
                 scan("$\\123")),
   ?_assertEqual({ok, [token(character, $\12, "$\\12", {1, 1, 1}, {4, 1, 4})]},
                 scan("$\\12")),
   ?_assertEqual({ok, [token(character, $\1, "$\\1", {1, 1, 1}, {3, 1, 3})]},
                 scan("$\\1"))
  ].

punctuation_test_() ->
  [?_assertEqual({ok, [token(punctuation, '(', "(", {1, 1, 1}, {1, 1, 1})]},
                 scan("(")),
   ?_assertEqual({ok, [token(punctuation, '+', "+", {1, 1, 1}, {1, 1, 1})]},
                 scan("+")),
   ?_assertEqual({ok, [token(punctuation, '#{', "#{", {1, 1, 1}, {2, 1, 2})]},
                 scan("#{")),
   ?_assertEqual({ok, [token(punctuation, '#{', "#  {", {1, 1, 1}, {4, 1, 4})]},
                 scan("#  {"))
  ].

whitespace_test_() ->
  [?_assertEqual({ok, []},
                 scan(" "))
  ].

expression_test_() ->
  [?_assertEqual({ok, [token(integer,     16,  "16", {1, 1, 1}, {2, 1, 2}),
                       token(punctuation, '+', "+",  {4, 1, 4}, {4, 1, 4}),
                       token(integer,     16,  "16", {6, 1, 6}, {7, 1, 7})]},
                 scan("16 + 16"))
  ].


%%%_* Test helpers =============================================================

scan(Str) ->
    %% DebugF = fun(Fmt0, Args) ->
    %%                  io:fwrite(user, list_to_binary(Fmt0 ++ "~n"), Args)
    %%          end,
  elx_lex:string(Str,
                 elx_erlang_lex:grammar(),
                 [
                  %% {debug_fun, DebugF},
                  {debug, true},
                  {re_groups, named_only}]
                ).

token(Type,
      Term,
      Str,
      {StartIndex, StartLine, StartCol},
      {EndIndex, EndLine, EndCol}) ->
  elx_lex:token(Type,
                Term,
                Str,
                elx_lex:point(StartIndex, StartLine, StartCol),
                elx_lex:point(EndIndex, EndLine, EndCol)).

%%%_* Emacs ====================================================================
%%% Local Variables:
%%% allout-layout: t
%%% erlang-indent-level: 2
%%% End:
