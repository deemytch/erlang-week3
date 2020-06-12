-module(ezequiel).
-export([create/1]).

create(Name) ->
  {ok,File} = file:open(Name,[read]),
  create(File, 1, #{}).

create(File, Line_number, Map) ->
  case io:get_line(File,"") of
    eof -> file:close(File),
           Map;
    Line ->
      create(File, Line_number + 1,
             lists:foldl(
               fun(Word, New_map) ->
                   maps:update_with(
                     string:uppercase(Word),
                     fun(Lines) ->
                         [Line_number|Lines]
                     end,
                     [Line_number],
                     New_map)
               end,
               Map,
               re:split(Line, "\s *|[[:^alnum:]]", [trim])))
  end.
