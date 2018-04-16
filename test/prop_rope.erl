%% Copyright © 2018 Pierre Fenoll ‹pierrefenoll@gmail.com›
%% See LICENSE for licensing information.
%% -*- coding: utf-8 -*-
-module(prop_rope).

-include_lib("proper/include/proper.hrl").
-include("ropes.hrl").


%% Properties

prop_at() ->
    ?FORALL(
       Rope
      ,ne_rope()
      ,?FORALL(Pos, pos(Rope)
              ,begin
                   Char = binary:at(iolist_to_binary(rope:to_iolist(Rope)), Pos),
                   Char =:= rope:at(Rope, Pos)
               end
              )
      ).

prop_concat() ->
    ?FORALL({RopeA,RopeB}, {rope(),rope()}
           ,begin
                Sum = iolist_to_binary(
                        [rope:to_iolist(RopeA)
                        ,rope:to_iolist(RopeB)
                        ]),
                Sum =:= iolist_to_binary(
                          rope:to_iolist(
                            rope:concat(RopeA, RopeB)))
            end
           ).

prop_delete() ->
    ?FORALL(
       Rope
      ,ne_rope()
      ,?FORALL(
          End
         ,pos(Rope)
         ,?FORALL(Start, integer(0, End)
                 ,begin
                      Str = iolist_to_binary(rope:to_iolist(Rope)),
                      {Mid,Tail} = bin_cut(End, Str),
                      {Head,_} = bin_cut(Start, Mid),
                      NewRope = rope:delete(Rope, Start, End),
                      <<Head/binary,Tail/binary>> =:=
                          iolist_to_binary(rope:to_iolist(NewRope))
                  end
                 )
         )
      ).

prop_from_string() ->
    ?FORALL(Str, str()
           ,begin
                BinStr = iolist_to_binary(Str),
                BinStr =:= iolist_to_binary(
                             rope:to_iolist(
                               rope:from_string(Str)))
            end
           ).

prop_insert_at() ->
    ?FORALL({Rope,Str}, {ne_rope(),str()}
           ,?FORALL(Idx, index(Rope)
                   ,begin
                        {L,R} = bin_cut(Idx, iolist_to_binary(rope:to_iolist(Rope))),
                        NewRope = rope:insert_at(Rope, Idx, Str),
                        <<L/binary,Str/binary,R/binary>> =:=
                            iolist_to_binary(rope:to_iolist(NewRope))
                    end
                   )
           ).

prop_len() ->
    ?FORALL(Rope, rope()
           ,begin
                byte_size(iolist_to_binary(rope:to_iolist(Rope)))
                    =:= rope:len(Rope)
            end
           ).

prop_split_at() ->
    ?FORALL(Rope, ne_rope()
           ,?FORALL(Idx, index(Rope)
                   ,begin
                        {LRope,RRope} = rope:split_at(Rope, Idx),
                        iolist_to_binary(rope:to_iolist(Rope))
                            =:= iolist_to_binary(
                                  [rope:to_iolist(LRope)
                                  ,rope:to_iolist(RRope)
                                  ])
                    end
                   )
           ).

prop_balance_again_preserves_ordering() ->
    ?FORALL(Thresh, thresh_small()
           ,?FORALL(Tree, unbalanced_tree(Thresh)
                   ,begin
                        {BTree,_,_} = rope:balance_again(Tree, Thresh),
                        iolist_to_binary(rope:tree_to_iolist(Tree))
                            =:= iolist_to_binary(rope:tree_to_iolist(BTree))
                    end
                   )
           ).

prop_balance_again_does_not_increase_depth() ->
    ?FORALL(Thresh, thresh_small()
           ,?FORALL(Tree, tree()
                   ,begin
                        {_,BDepth,_} = rope:balance_again(Tree, Thresh),
                        abs(rope:depth(Tree)) >= abs(BDepth)
                    end
                   )
           ).

%% Generators

rope() ->
    ?LET({Tree,Thresh}, {tree(),thresh()}, rope(Tree,Thresh)).

rope(Tree, Thresh) ->
    #rope{len = rope:tree_len(Tree)
         ,thresh = Thresh
         ,tree = Tree
         }.

ne_rope() ->
    oneof(
      [?SUCHTHAT(#rope{len = Len}, rope(), Len =/= 0)
      ,?LET({Tree,Thresh}, {ne_tree(),thresh()}, rope(Tree,Thresh))
      ,?LET({Tree,Thresh}, {cx_tree(),thresh()}, rope(Tree,Thresh))
      ]).

thresh_small() ->
    integer(1, 3).

index(#rope{len = Len}) ->
    integer(0, Len).

pos(#rope{len = Len}) ->
    integer(1, Len).

ne_string() ->
    ?SUCHTHAT(Str, list(range($\s,$~)), Str =/= "").

str() ->
    oneof([ne_string()
          ,?LET(NEStr, ne_string(), list_to_binary(NEStr))
          ]).

tree() ->
    ?LAZY(
       ?LET({LHS,RHS}, frequency(
                         [{2, str(), str()}
                         ,{1, str(), tree()}
                         ,{1, tree(), str()}
                         ,{1, tree(), tree()}
                         ])
           ,#tree{weight = rope:tree_len(LHS)
                 ,lhs = LHS
                 ,rhs = RHS
                 }
           )
      ).

ne_tree() ->
    ?SUCHTHAT(
       #tree{weight = W, rhs = RHS}
      ,tree()
      ,W =/= 0 andalso ne(RHS)
      ).

cx_tree() ->
    ?LET({TreeA,TreeB}, {ne_tree(),ne_tree()}
        ,rope:concatenate(TreeA, TreeB)
        ).

unbalanced_tree(Thresh) ->
    ?SUCHTHAT(
       Tree
      ,?LET({TreeA,TreeB}, {cx_tree(),cx_tree()}
           ,rope:concatenate(TreeA, TreeB)
           )
      ,not is_balanced(Tree, Thresh)
      ).

%% Internals

ne(<<>>) -> false;
ne("") -> false;
ne(_) -> true.

is_balanced(Tree, Thresh) ->
    {Balance,_Depth} = balance(Tree, Thresh),
    Balance.

balance(#tree{lhs = LHS, rhs = RHS}, Thresh) ->
    {LB,LD} = ?FUNCTION_NAME(LHS, Thresh),
    {RB,RD} = ?FUNCTION_NAME(RHS, Thresh),
    {LB andalso RB andalso Thresh >= abs(LD - RD)
    ,1 + max(LD, RD)
    };
balance(_Str, _) ->
    {true, 0}.

bin_cut(Idx, Str) ->
    {binary:part(Str, 0, Idx)
    ,binary:part(Str, Idx, byte_size(Str) - Idx)
    }.

%% End of Module.
