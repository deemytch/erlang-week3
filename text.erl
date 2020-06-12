-module(text).
-export([test/0,load_file/2,file2words/1,text2words/1,words2lines/2,lines2wrapped/3,load_and_format/2,pad/3]).

test()->
  Filename="vwt3.txt",
  Width=32,
  Wordlist=["Die","Radlagerung","vorne","am","T3","mit","Hinterradantrieb","besteht","aus","2","Kegelrollenlagern","in","sog.","O-Anordnung.","Die","Lager","haben","Zollabmessungen.","Auf","der","Innenseite","befindet","sich","ein","Wellendichtring","251407641A.","Die","Lager","sind","in","die","in","die","Bremstrommel","integrierte","Nabe","eingepresst.","Auf","dem","Achszapfen","des","Achsschenkels","haben","sie","Schiebesitz."],

  Linelist=[ {26,["Die","Radlagerung","vorne","am","T3","mit"]},
             {27,["Hinterradantrieb","besteht","aus","2"]},
             {23,["Kegelrollenlagern","in","sog."]},
             {25,["O-Anordnung.","Die","Lager","haben"]},
             {22,["Zollabmessungen.","Auf","der"]},
             {25,["Innenseite","befindet","sich","ein"]},
             {29,["Wellendichtring","251407641A.","Die"]},
             {19,["Lager","sind","in","die","in","die"]},
             {27,["Bremstrommel","integrierte","Nabe"]},
             {28,["eingepresst.","Auf","dem","Achszapfen"]},
             {24,["des","Achsschenkels","haben","sie"]},
             {12,["Schiebesitz."]}],
  Wordlist=file2words(Filename),
  Linelist=words2lines(Width, Wordlist),
" Die Radlagerung vorne am T3 mit
  Hinterradantrieb besteht aus 2
       Kegelrollenlagern in sog.
    O-Anordnung. Die Lager haben
        Zollabmessungen. Auf der
    Innenseite befindet sich ein
 Wellendichtring 251407641A. Die
        Lager sind in die in die
   Bremstrommel integrierte Nabe
 eingepresst. Auf dem Achszapfen
     des Achsschenkels haben sie
                    Schiebesitz."=text:lines2wrapped(lp,Width,Linelist),
  ok.

load_and_format(Width,Filename)->
  lines2wrapped(lp,Width,
    words2lines(Width,
      file2words(Filename) )).

%% the record for storing lines of parapgarph is { TotalWordLength, ["List", "of", "words"]}
% gets justify hint and source text as array of words wrapped into lines;
% returns formatted paragraph as string.
-spec lines2wrapped( atom(), integer(), [{integer(),unicode:charlist()}] )->unicode:charlist().
lines2wrapped(Command, Width, Text)->
  string:join( [pad(string:join(L," "),Width,Command)||{_W,L}<-Text], "\n" ).
%  [Func(X)||X<-Text].

pad( String, Width, cv ) when length(String) >= Width -> String;
pad( String, Width, cv ) when length(String)+2 < Width -> pad( " "++String++" ", Width, cv );
pad( String, Width, cv ) when length(String)+1 < Width -> pad( " "++String, Width, cv );

pad( String, Width, lp ) when length(String) >= Width -> String;
pad( String, Width, lp ) when length(String)+1 =< Width -> pad( " "++String, Width, lp );

pad( String, Width, rp ) when length(String) >= Width -> String;
pad( String, Width, rp ) when length(String)+1 =< Width -> pad( String++" ", Width, rp ).

% gets the whole paragraph as an array of words,
% then combines it into array of { LineLength, WordList },
% with total length of a line no more than Width,
% including one space per word
-spec words2lines(integer(),[string()]) -> [ { integer(), [string()] } ].
words2lines( Width, Text )->words2lines( Width, {0,[]}, [], Text ).

-spec words2lines(integer(), { integer(), [string()] }, [string()], [{ integer(), [string()] }] ) ->
  [{ integer(), [string()] }].
words2lines( _Width,{ Length, Line }, Output, [] ) -> Output++[{ Length, Line }];

words2lines( Width, {0,[]}, Output, [Word|Source] )  ->
  words2lines( Width, { length(Word), [Word] }, Output, Source );

words2lines( Width, { Length, Line }, Output, [Word|Source] )->
  % word count is min spaces count + words length
  if length(Line) - 1 + Length + length(Word) > Width ->
    words2lines( Width, { length(Word),[Word] }, Output++[{Length, Line}], Source );
  true ->
    words2lines( Width, { Length + length(Word), Line ++ [Word] }, Output, Source )
  end.

% Building array [Word,...] from long string
% we believe that there is one paragraph here
text2words(Text)-> string:tokens(Text," \t\n\r").

-spec file2words(string())->[string()].
file2words(Name) ->
  {ok,File} = file:open(Name,[read]),
  text2words(load_file(File,[])).

% loads a file into long string
-spec load_file( pid(), [string()] )->[string()].
load_file(File,Accum) ->
  case io:get_line(File,"") of
    eof ->
      file:close(File),
      Accum;
    Line ->
      load_file(File, Accum++Line)
  end.
