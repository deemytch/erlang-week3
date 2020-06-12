-module(kevin).
-export([get_file_contents/1,show_file_contents/1,indexer/1]).

% Used to read a file into a list of lines.
% Example files available in:
%   gettysburg-address.txt (short)
%   dickens-christmas.txt  (long)
 

% Get the contents of a text file into a list of lines.
% Each line has its trailing newline removed.

get_file_contents(Name) ->
    {ok,File} = file:open(Name,[read]),
    Rev = get_all_lines(File,[]),
lists:reverse(Rev).

% Auxiliary function for get_file_contents.
% Not exported.

get_all_lines(File,Partial) ->
    case io:get_line(File,"") of
        eof -> file:close(File),
               Partial;
        Line -> {Strip,_} = lists:split(length(Line)-1,Line),
                get_all_lines(File,[Strip|Partial])
    end.   

% Show the contents of a list of strings.
% Can be used to check the results of calling get_file_contents.
 
show_file_contents([L|Ls]) ->
    io:format("~s~n",[L]),
    show_file_contents(Ls);
 show_file_contents([]) ->
    ok.    

% Data is stored as list of tuples. Each tuple contains a name and 
% list of tuple of lines.
% { "foo" , [{3,5},{7,7},{11,13}] }
% { string(), [{integer(),integer()}] }

% return the name of the list of tuples.
-spec get_word(integer(),{ string(), [{integer(),integer()}] }) -> string().
get_word(N,L) ->  
% io:format("get word N is ~p~n",[N]),
  element(1,lists:nth(N,L)).

% return a list of the tuples of lines.
-spec get_lines(integer(),{ string(), [{integer(),integer()}] }) -> [{integer(),integer()}].
get_lines(N,L) ->
  element(2,lists:nth(N,L)). 

% InFile  - File to read 
-spec indexer(string()) -> [ { string(), [{integer(),integer()}] } ].
indexer(InFile) ->
  Lines = get_file_contents(InFile),
  analyse_words(Lines,1,[]).   % build a list of words and line numbers
  
  
% build a list of words and line numbers
-spec analyse_words([string()],integer(),[ { string(), [{integer(),integer()}] } ]) -> [ { string(), [{integer(),integer()}] } ].
analyse_words([],_N,WordsList) -> % N is line number
  WordsList;
analyse_words([L|Ls],N,WordsList) ->   % L represents a sentence
  FilteredText = string:uppercase(L),
  Words = sentence_to_words(FilteredText,N),      % get a list of the words in the current line
  NewWordsList = update_index(Words,WordsList),   % update the master list of words
  analyse_words(Ls,N+1,NewWordsList).             

% split a sentence in a list of seperate words.
-spec sentence_to_words([string()],integer()) -> [string()].
sentence_to_words([],_LineNum) ->
  [];
sentence_to_words([_L|_Ls]=List,LineNum) ->
  readWords(List,[],LineNum).      % read the words in a line 

% build a list of words in a sentence.
-spec readWords([string()],[string()],integer()) -> [string()]. 
readWords([],Acc,_LineNum) ->
  Acc;
readWords([_L|_Ls]=List,Acc,LineNum) ->
  {Word,ListRem} = readWord(List,[]),
  readWords(ListRem,lists:append(Acc,[{ Word , [{LineNum,LineNum}] }]),LineNum). % add list of words to the master word list

% get the get word in a sentence.
-spec readWord([string()],[string()]) -> string().
readWord([],Acc) ->
  {lists:reverse(Acc),[]};
readWord([L|Ls],Acc) when (L == 32) or (L == 9) or (L == 46) ->   % use space, tab or full stop to indicate end of word
  {lists:reverse(Acc),skipWhitespace(Ls)};                      % return tuple of word and list with the starting position of next word
readWord([L|Ls],Acc) when (L >= $A) and (L =< $Z ) ->           % if letter add to current word
  readWord(Ls,[L|Acc]);             
readWord([_L|Ls],Acc) ->                                          % if not letter ignore eg door-knob is treated as doorknob.
  readWord(Ls,Acc).

% find start of next word
-spec skipWhitespace([string()]) -> [string()]. 
skipWhitespace([]) ->     % end of line
  [];
skipWhitespace([L|Ls]) when not((L >= $A) and (L =< $Z )) ->     % ignore character if not a letter
  skipWhitespace(Ls);                                            
skipWhitespace([L|Ls]) ->                                        % return list starting with a letter
  [L|Ls].

% update master list of words
-spec update_index({ string(), [{integer(),integer()}] },[ { string(), [{integer(),integer()}] } ] ) -> [ { string(), [{integer(),integer()}] } ]. 
update_index(L,WordsList) ->    
  update_index(1,L,WordsList).  

% N is the current position in the master list of words
-spec update_index(integer(),{ string(), [{integer(),integer()}] },[ { string(), [{integer(),integer()}] } ] ) -> [ { string(), [{integer(),integer()}] } ].
update_index(N,L,WordsList) when N > length(L) ->                % end of master list of words so return master list
  WordsList;
update_index(N,L,WordsList) ->
  Word = get_word(N,L),                                        % get Nth word in the master list of words
% Lines = get_lines(N,L),
  
  case length(WordsList) of
    0 ->    % words list is empty so add current entry
      update_index(N+1,L,[lists:nth(N,L)]);  
    _ ->    % 
      {Found,Place} = is_word_in_wordsList(Word,WordsList),      % is the current word in the master list of words
      case Found of                                            
        true  ->   
          MasterLines = get_lines(Place,WordsList),       %  get the line numbers for the current word 
          {WordListStart,WordListFinish} = lists:nth(length(MasterLines),MasterLines), % get last set of line numbers     

          %[Drop = 0, Replace = 1, Add = 2] -> return values from condense last index function
          {Command,{NewStart,NewFinish}} = condense_last_index({WordListStart,WordListFinish},N),
          case Command of
            % Drop                  % current line is the same as the last one seen so ignore 
            0 ->
              update_index(N+1,L,WordsList);  
            % Replace last item in list with updated version e.g. {1,2},{2,3} with {1,3}
            1 ->
              DroppedLastLines = lists:sublist(MasterLines,length(MasterLines) - 1),   % return a list of lines expect the last one
              UpdatedLines = lists:append(DroppedLastLines,[{NewStart,NewFinish}]),    % add the updated version
              NewRec = [{ Word , UpdatedLines }],                                      % combine word and lines number
              ReplacedWordsList = replace(Place,WordsList,NewRec),                     % replace entry for word in master list
              update_index(N+1,L,ReplacedWordsList);                  
            %                                 
            2 ->  % add current line number to master list of words 
              NewLines = lists:append(MasterLines,[{NewStart,NewFinish}]),
              NewRec = [{ Word , NewLines }],   % combine word and lines number
              ReplacedWordsList = replace(Place,WordsList,NewRec),   
              update_index(N+1,L,ReplacedWordsList)
          end;

        false ->  % Add word as it is not in the master list of words
                  % data is added to the end of the list so is unsorted.
            AddedWordsList = lists:append(WordsList,[lists:nth(N,L)]),   
            update_index(N+1,L,AddedWordsList)
        end         
  end.
  
  
% search for a word in the list of words 
% return true if found along with position in the list
% return false if not found with the value -1
% The list is searched linearly.
% A better solution would be to search binary but this needs the data to be sorted 
-spec is_word_in_wordsList(string(), [ { string(), [ { integer(),integer() } ] } ] ) -> {boolean(),integer()}.  
is_word_in_wordsList(Word,WordsList) ->
  is_word_in_wordsList(1,Word,WordsList).


-spec is_word_in_wordsList(integer(), string(), [ { string(), [ { integer(),integer() } ] } ] ) -> {boolean(),integer()}.
is_word_in_wordsList(_N,_Word,[]) ->
  {false,-1};
is_word_in_wordsList(N,_Word,WordsList) when N > length(WordsList) ->
  {false,-1};
is_word_in_wordsList(_N,[],_WordsList) ->
  {false,-1};
is_word_in_wordsList(N,Word,WordsList) ->
  case Word == get_word(N,WordsList) of
    true ->   {true,N};
    false ->  is_word_in_wordsList(N+1,Word,WordsList)
  end.  
  

% place new entry (e.g. { "foo" , [{3,5},{7,7},{11,13}] }) in the master word list
% and return new list.
-spec replace(integer(),[{ string(), [{integer(),integer()}] }],{ string(), [{integer(),integer()}] }) -> [{ string(), [{integer(),integer()}] }].
replace(Place,WordsList,L) ->
  lists:sublist(WordsList,Place-1) ++ L ++ lists:nthtail(Place,WordsList).

% compare current line with last in entry for word
% if the same signal that the current line number should be dropped.
% if overlapping the signal last numbers should be replace with an updated version. eg. given {1,3} and 4 return {1,4}.
% if non-concurrent signal current line number should be added to the list of entries.
%[Drop = 0, Replace = 1, Add = 2]
-spec condense_last_index({integer(),integer()},integer()) -> {integer(),{integer(),integer()}}.
condense_last_index({LastX,LastY},N) ->
  
  if 
    % the last index is the same as the index of the current word - drop last numbers
    (LastX == N) and (LastY == N) ->
      {0,{LastX,LastY}};
    % Condense lines e.g.{1,2},{2,3} becomes {1,3}. - replace in list of line numbers
    ((LastY + 1) == N) or (LastY == N) ->
      {1,{LastX,N}};
    % These lines are not in the master list so add
    true ->
      {2,{N,N}}
  end.


  