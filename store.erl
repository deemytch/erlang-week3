-module(store).
-export([test/0,items/1,discounts/1,tax_item/2,tax/1,print/1]).
-record( item, { id :: integer(), cost :: integer(), name :: nonempty_string() } ).
-record( discount, { id ::integer(), item_id ::integer(), percent ::integer(), qty ::integer(), d ::integer() } ).

test()->
  18130=tax([{ check, 92981, 0.5 },{ check, 5646, 3 }, { check, 94416, 0.6 }]),
  24900=tax([{ check, 49063, 3.2 }, { check, 17417, 4 }, { check, 5649, 5 } ]),
  ok.

% require 'ffaker'; 20.times{|i| puts "#item{ id=#{ rand(99999) }, cost=#{ rand(30)*100 }, name=\"#{ FFaker::Food.fruit }\" }," }
items(Id) ->
  Items=[ #item{ id=96567, cost=1400, name="Cranberry" },
    #item{ id=52927, cost=2100, name="Ugli fruit" },
    #item{ id=31660, cost=700, name="Kumquat" },
    #item{ id=17417, cost=2000, name="Mandarine" },
    #item{ id=75404, cost=1600, name="Banana" },
    #item{ id=49063, cost=500, name="Nectarine" },
    #item{ id=92523, cost=400, name="Huckleberry" },
    #item{ id=76966, cost=2200, name="Grapefruit" },
    #item{ id=6006, cost=1900, name="Pineapple" },
    #item{ id=94416, cost=1800, name="Mandarine" },
    #item{ id=86956, cost=800, name="Lychee" },
    #item{ id=22622, cost=300, name="Strawberry" },
    #item{ id=92981, cost=2300, name="Blueberry" },
    #item{ id=14985, cost=800, name="Huckleberry" },
    #item{ id=74078, cost=1600, name="Rock melon" },
    #item{ id=59552, cost=2800, name="Pomegranate" },
    #item{ id=49742, cost=1400, name="Persimmon" },
    #item{ id=22381, cost=1300, name="Miracle fruit" },
    #item{ id=56675, cost=1700, name="Elderberry" },
    #item{ id=79610, cost=300, name="Dragonfruit" },
    #item{ id=5645, cost=3100, name="White Whiskey" },
    #item{ id=5646, cost=5300, name="Port Rom" },
    #item{ id=5647, cost=5300, name="Tequila" },
    #item{ id=5648, cost=32000, name="Orig. Bormotukha" },
    #item{ id=5649, cost=3100, name="Dry Sherry" }
  ],
  case [I||I<-Items,I#item.id==Id] of [] -> []; Item -> hd(Item) end.

discounts(Item_id)->
  D=[ #discount{ id=3, item_id=5649, qty=2, d=100 } ],
  case [I||I<-D,I#discount.item_id==Item_id] of [] -> []; Item -> hd(Item) end.

% calculate one line of bill
tax_item(Id,Qty)->
  I = items(Id),
  Amount = ceil( I#item.cost * Qty ),
  case discounts(Id) of
    []   -> Amount;
    {discount, _Id, _Iid, _P, DQty, D} ->
      ceil( Amount - floor( Qty / DQty ) * D )
  end.

% bill total
tax(Check)->
  lists:sum([ tax_item(Item_id, Qty)||{check, Item_id, Qty}<-Check ]).

print(Check)->
  PaperW = 31,
  [ io:fwrite("~ts", [formatln(L)] ) ||L<-Check ],
  Total = io_lib:format("£~.2f", [tax(Check)/100]),
  io:format("~nTotal~*...ts~n",[ PaperW - 6, Total ]).

formatln({check, Id, Qty})->
  PaperW = 31,
  Item   = items(Id),
  Discnt = discounts(Id),
  Total  = io_lib:format( "£~.2f", [tax_item(Id,Qty)/100] ),
  lists:flatten( io_lib:format("~ts~*...ts~n", [ Item#item.name, PaperW - 1 - string:length(Item#item.name), Total ] )).
