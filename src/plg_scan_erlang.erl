%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% @doc Erlang grammar for parse_lib_scan
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
         atom/4,
         character/4,
         float/4,
         keyword/4,
         integer/4,
         string/4,
         variable/4]).

%%%_* Includes =================================================================

-include_lib("eunit/include/eunit.hrl").

%%%_* Defines ==================================================================

-define(ERLANG_KEYWORDS, ["after",
                          "and",
                          "andalso",
                          "band",
                          "begin",
                          "bnot",
                          "bor",
                          "bsl",
                          "bsr",
                          "bxor",
                          "case",
                          "catch",
                          "cond",
                          "div",
                          "end",
                          "fun",
                          "if",
                          "let",
                          "not",
                          "of",
                          "or",
                          "orelse",
                          "receive",
                          "rem",
                          "try",
                          "when",
                          "xor"]).

-define(ERLANG_INTEGER_BASE_MAX, 32).

%%%_* Types ====================================================================

%%%_* Grammar ==================================================================

grammar() ->
    [{?ERLANG_KEYWORDS,                              fun keyword/4},
     {"(?:-|\\+)?[0-9]+\\.[0-9]+(e(-|\\+)?[0-9]+)?", fun float/4},
     {"(?<sign>-|\\+)?"
      "((?<base>[0-9]+)(#))?"
      "(?<value>[0-9]+)",                            fun integer/4},
     {{["\\$\\\\(?<octal>[0-7]?[0-7]?[0-7])",
        "\\$\\\\\\^(?<control>[A-Za-z])",
        "\\$\\\\(?<escape>[bdefnrstv])",
        "\\$\\\\?(?<normal>.)"],
       [dotall]},                                    fun character/4},
     {"\"(.*\")*.*\"",                               fun string/4},
     {["[a-z][a-zA-Z0-9_@]*", "'(.*')*.*'"],         fun atom/4},
     {"[A-Z_][a-zA-Z0-9_]*",                         fun variable/4}].

%%%_* Token parsers ============================================================


atom(Chars, _Matches, Start, End) ->
  Term = list_to_atom(string:strip(Chars, both, $')),
  {token,
   parse_lib_scan:token(atom, Term, Chars, Start, End)}.

character(Chars, Matches, Start, End) ->
  {Group, MatchStr} = character_match(Chars, Matches),
  Term              = character_term(Group, MatchStr),
  {token, parse_lib_scan:token(character, Term, Chars, Start, End)}.

float(Chars, _Matches, Start, End) ->
  {token,
   parse_lib_scan:token(float, list_to_float(Chars), Chars, Start, End)}.

integer(Chars, Matches, Start, End) ->
  try do_integer(Chars, Matches, Start, End)
  catch
    error:{illegal_base, _} = Rsn -> {error, Rsn};
    error:badarg                  -> {error, illegal_integer}
  end.

keyword(Chars, _Matches, Start, End) ->
  {token,
   parse_lib_scan:token(keyword, list_to_atom(Chars), Chars, Start, End)}.

variable(Chars, _Matches, Start, End) ->
  {token,
   parse_lib_scan:token(variable, list_to_atom(Chars), Chars, Start, End)}.

string(Chars, _Matches, Start, End) ->
  Term = string:strip(Chars, both, $"),
  {token,
   parse_lib_scan:token(string, Term, Chars, Start, End)}.


%%%_* Internal functions =======================================================

character_match(Str, Matches) ->
  character_match(Str, Matches, [normal, escape, control, octal]).

character_match(Str, Matches, [Group|Groups]) ->
  io:fwrite(user, <<"~p ~p ~p: Matches = ~p~n">>, [self(), ?MODULE, ?LINE, Matches]),
  case parse_lib_scan:match_named_group_string(Str, Group, Matches) of
    {error, nomatch}  -> character_match(Str, Matches, Groups);
    {ok,    MatchStr} -> {Group, MatchStr}
  end.

character_term(normal,  [C])       -> C;
character_term(control, [C])       -> C band 31;
character_term(escape,  "b")       -> $\b;
character_term(escape,  "d")       -> $\d;
character_term(escape,  "e")       -> $\e;
character_term(escape,  "f")       -> $\f;
character_term(escape,  "n")       -> $\n;
character_term(escape,  "r")       -> $\r;
character_term(escape,  "s")       -> $\s;
character_term(escape,  "t")       -> $\t;
character_term(escape,  "v")       -> $\v;
character_term(octal,   [X, Y, Z]) -> (X * 64) + (Y * 8) + Z - ($0 * 8).


do_integer(Chars, Matches, Start, End) ->
  SignStr =
    case parse_lib_scan:match_named_group_string(Chars, sign, Matches) of
      {error, nomatch} -> "";
      {ok, SignStr0} -> SignStr0
    end,
  Base = case parse_lib_scan:match_named_group_string(Chars, base, Matches) of
           {error, nomatch} -> 10;
           {ok, BaseStr}    ->
             case list_to_integer(BaseStr) of
               Base0 when Base0 > ?ERLANG_INTEGER_BASE_MAX ->
                 erlang:error({illegal_base, Base0});
               Base0 ->
                 Base0
             end
         end,
  {ok, ValueStr} = parse_lib_scan:match_named_group_string(Chars, value, Matches),
  Term = list_to_integer(SignStr ++ ValueStr, Base),
  {token, parse_lib_scan:token(integer, Term, Chars, Start, End)}.


%%%_* Tests ====================================================================

%%%_* Test helpers =============================================================

%%%_* Emacs ====================================================================
%%% Local Variables:
%%% allout-layout: t
%%% erlang-indent-level: 2
%%% End:
