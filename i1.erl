-module(i1).
-export([test/0,show_stats/1,get_file_contents/1]).

% Divide, count and gnaw
get_file_contents(Name) ->
  {ok,File} = file:open(Name,[read]),
  [gnaw(Word)||Word<-collect_stones(get_all_lines(File,[],1),[])].

% Building array [{Word, LineNo},...]
get_all_lines(File,Partial,N) ->
  case io:get_line(File,"") of
    eof ->
      file:close(File),
      Partial;
    Line -> % yes, it copies last arg. 
      get_all_lines(File,Partial++[{Word,N}||Word<-parse_line(Line),length(Word)>2],N+1)
  end.

parse_line([])  -> [];
parse_line(Line)-> string:tokens( string:casefold(Line), " .,?\\/-_;" ).

collect_stones([],Stones)->Stones;
collect_stones([{Word,N}|WordList],Stones)->
  % are we had added that word already?
  case search(Word,Stones) of
    false -> collect_stones(WordList,[{Word,[N]++just_numbers(Word,WordList)}|Stones]);
    true  -> collect_stones(WordList,Stones)
  end.

just_numbers(Word,WordList)->lists:sort([N||{W,N}<-WordList,Word==W]).

gnaw({Word,Numbers}) -> { Word, gnaw({},Numbers,[]) }.
% storing ranges into nested arrays
gnaw(Range,[],Gnawed)      ->lists:reverse([Range|Gnawed]);
gnaw({},[N|List],Gnawed)   ->gnaw({N},List,Gnawed);
gnaw({N},[N|List],Gnawed)  ->gnaw({N},List,Gnawed);
gnaw({X},[Y|List],Gnawed)   when Y==X orelse Y-1==X -> gnaw({X,Y},List,Gnawed);
gnaw({X,Y},[N|List],Gnawed) when Y==N orelse N-1==Y -> gnaw({X,N},List,Gnawed);
gnaw(Range,[N|List],Gnawed) -> gnaw({N},List,[Range|Gnawed]).

search(_Word,[])->false;
search(Word,[{Word,_Numbers}|_WordList])->true;
search(Word,[{X,_Numbers}|WordList])->
  case string:casefold(Word)==string:casefold(X) of
      true -> true;
      false -> search(Word,WordList)
  end.

% Show the contents of a list of strings.
show_stats(List) ->
  [io:format("~ts: ~p~n",[Word,Numbers])||{Word,Numbers}<-List].

% tests are tested with modified testing text.
test()->
  Wl=[{"Видал",[1]},{"and",[3,5,8,12,17,29]}],
  {"Word",[{1,5},{10},{12,13},{22}]}=gnaw({"Word",[1,2,3,4,5,5,5,5,10,10,12,13,22]}),
  false=search("Slovo",Wl),
  true =search("видал",Wl),
  true =search("and",Wl),
  Gettysburg=get_file_contents("gettysburg-address.txt"),
  WordAndPos=[Found||{Word,Found}<-Gettysburg,Word=="and"],
  [[{3},{5},{8},{12},{17},{29}]]=WordAndPos,
  ok.
