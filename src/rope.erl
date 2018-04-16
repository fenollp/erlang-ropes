%% Copyright © 2018 Pierre Fenoll ‹pierrefenoll@gmail.com›
%% See LICENSE for licensing information.
%% -*- coding: utf-8 -*-
-module(rope).

-export([at/2
        ,concat/2
        ,delete/3
        ,from_string/1
        ,insert_at/3
        ,len/1
        ,split_at/2
        ,to_iolist/1
        ]).

-record(tree, {weight :: integer()
              ,lhs :: tree()
              ,rhs :: tree()
              }).
-opaque tree() :: #tree{}.

-type thresh() :: pos_integer().
-record(rope, {len :: non_neg_integer()
              ,thresh = 1 :: thresh()
              ,tree :: maybe(tree())
              }).
-opaque t() :: #rope{}.

%% Note: indices start at zero
-type index() :: non_neg_integer().

-export_type([t/0, tree/0, index/0]).

-type maybe(T) :: nil | T.
-type str() :: char() | string() | binary(). %% Not really iodata()
-define(NE_BINARY, <<_:8,_/binary>>).
-define(IS_INDEX(Idx), is_integer(Idx), Idx >= 0).

%% API

-spec at(t(), index()) -> char().
at(#rope{tree = Tree}, Offset)
  when ?IS_INDEX(Offset) ->
    char_at(Offset, Tree).

-spec concat(t(), t()) -> t().
concat(#rope{len = ALen
            ,thresh = AThresh
            ,tree = ATree
            }
      ,#rope{len = BLen
            ,thresh = BThresh
            ,tree = BTree
            }) ->
    Thresh = max(AThresh, BThresh),
    Unbalanced = concatenate(ATree, BTree),
    {Tree,_Depth,_Balance} = balance_again(Unbalanced, Thresh),
    #rope{len = ALen + BLen
         ,thresh = Thresh
         ,tree = Tree
         }.

-spec delete(t(), index(), index()) -> t().
delete(#rope{len = Len, tree = Tree}=R, Start, End)
  when ?IS_INDEX(Start),
       is_integer(End), End > 0 ->
    R#rope{len = Len - End + Start %+- 1?
          ,tree = remove(Tree, Start, End)
          }.

-spec from_string(str()) -> t().
from_string(Str=?NE_BINARY) ->
    #rope{len = byte_size(Str)
         ,tree = Str
         };
from_string(Str=[_|_]) ->
    #rope{len = length(Str)
         ,tree = Str
         };
from_string(Char)
  when is_integer(Char), Char > 0 ->
    #rope{len = 1
         ,tree = [Char]
         }.

-spec insert_at(t(), index(), str()) -> t().
insert_at(#rope{len = Len, tree = Tree}=R, Idx, Str=?NE_BINARY)
  when ?IS_INDEX(Idx) ->
    R#rope{len = Len + byte_size(Str)
          ,tree = insert(Tree, Idx, Str)
          };
insert_at(#rope{len = Len, tree = Tree}=R, Idx, Str=[_|_])
  when ?IS_INDEX(Idx) ->
    R#rope{len = Len + length(Str)
          ,tree = insert(Tree, Idx, Str)
          };
insert_at(#rope{len = Len, tree = Tree}=R, Idx, Char)
  when ?IS_INDEX(Idx),
       is_integer(Char), Char > 0 ->
    R#rope{len = Len + 1
          ,tree = insert(Tree, Idx, [Char])
          }.

-spec len(t()) -> pos_integer().
len(#rope{len = Len}) -> Len.

-spec split_at(t(), index()) -> {t(), t()}.
split_at(#rope{len = Len, thresh = Thresh, tree = Tree}=R, Idx)
  when ?IS_INDEX(Idx) ->
    {LTree,RTree} = cut(Tree, Idx),
    {BalancedLTree,_,_} = balance_again(LTree, Thresh),
    {BalancedRTree,_,_} = balance_again(RTree, Thresh),
    {R#rope{tree = BalancedLTree, len =       Idx}
    ,R#rope{tree = BalancedRTree, len = Len - Idx}
    }.

-spec to_iolist(t()) -> iolist().
to_iolist(#rope{tree = Tree}) ->
    tree_to_iolist(Tree).

%% Internals

tree_to_iolist(#tree{lhs = LHS, rhs = RHS}) ->
    [tree_to_iolist(LHS) | tree_to_iolist(RHS)];
tree_to_iolist(Str) ->
    Str.

char_at(Idx, #tree{rhs = RHS, weight = W})
  when W < Idx ->
    char_at(Idx - W, RHS);
char_at(Idx, #tree{lhs = LHS}) ->
    char_at(Idx, LHS);
char_at(Idx, Str=?NE_BINARY) ->
    binary:at(Str, Idx);
char_at(Idx, Str=[_|_]) ->
    lists:nth(1 + Idx, Str).

cut(Tree, 0) ->
    {<<>>, Tree};
cut(#tree{weight = W, lhs = LHS, rhs = RHS}, Idx)
  when W > Idx ->
    {LTree1,LTree2} = cut(LHS, Idx),
    Tree = #tree{weight = Idx, lhs = LTree1, rhs = <<>>},
    {Tree, concatenate(LTree2,RHS)};
cut(#tree{weight = W, rhs = RHS}=Tree, Idx) ->
    {RTree1,RTree2} = cut(RHS, Idx - W),
    {Tree#tree{rhs = RTree1}, RTree2};
cut(Str=?NE_BINARY, Idx) ->
    {binary:part(Str, 0, Idx)
    ,binary:part(Str, Idx, byte_size(Str) - Idx)
    };
cut(Str=[_|_], Idx) ->
    lists:split(Idx, Str).

insert(Tree, Idx, Str) ->
    {LTree,RTree} = cut(Tree, Idx),
    concatenate(LTree, concatenate(Str,RTree)).

concatenate(#tree{}=A, #tree{}=B) ->
    #tree{weight = tree_len(A)
         ,lhs = A
         ,rhs = B
         }.

remove(Tree, Start, End) ->
    {MTree,RTree} = cut(Tree, End),
    {LTree,_Rmv} = cut(MTree, Start),
    concatenate(LTree, RTree).

tree_len(#tree{weight = W, rhs = RHS}) ->
    W + tree_len(RHS);
tree_len(Str=?NE_BINARY) ->
    byte_size(Str);
tree_len(Str=[_|_]) ->
    length(Str).

depth(#tree{lhs = LHS, rhs = RHS}) ->
    1 + max(?FUNCTION_NAME(LHS), ?FUNCTION_NAME(RHS));
depth(_Str) ->
    0.

-spec balance_again(tree(), thresh()) -> {tree(), pos_integer(), integer()}.
balance_again(Str, _)
  when is_binary(Str); is_list(Str) ->
    {Str, 0, 0};
balance_again(#tree{lhs = LHS, rhs = RHS}, Thresh) ->
    {LTree,LDepth,LBalance} = ?FUNCTION_NAME(LHS, Thresh),
    {RTree,RDepth,RBalance} = ?FUNCTION_NAME(RHS, Thresh),
    case LDepth - RDepth of
        LeftHeavyBalance when LeftHeavyBalance > Thresh ->
            MaybeRotatedLTree =
                case LBalance < -Thresh of
                    false -> LTree;
                    true -> rotate_left(LTree)
                end,
            Tree = rotate_right(concatenate(MaybeRotatedLTree, RTree)),
            {Tree, depth(Tree), balance_shallow(Tree)};

        RightHeavyBalance when RightHeavyBalance < -Thresh ->
            MaybeRotatedRTree =
                case RBalance > Thresh of
                    false -> RTree;
                    true -> rotate_right(RTree)
                end,
            Tree = rotate_left(concatenate(LTree, MaybeRotatedRTree)),
            {Tree, depth(Tree), balance_shallow(Tree)};

        Balance ->
            {concatenate(LTree, RTree)
            ,1 + max(LDepth, RDepth)
            ,Balance
            }
    end.

balance_shallow(#tree{lhs = LHS, rhs = RHS}) ->
    depth(LHS) - depth(RHS);
balance_shallow(_Str) ->
    0.

rotate_left(#tree{weight = AW
                 ,lhs = ALHS
                 ,rhs = #tree{weight = BW
                             ,lhs = BLHS
                             ,rhs = BRHS
                             }
                 }) ->
    #tree{weight = AW + BW
         ,lhs = #tree{weight = AW
                     ,lhs = ALHS
                     ,rhs = BLHS
                     }
         ,rhs = BRHS
         }.

rotate_right(#tree{weight = AW
                  ,lhs = #tree{weight = BW
                              ,lhs = BLHS
                              ,rhs = BRHS
                              }
                  ,rhs = ARHS
                  }) ->
    #tree{weight = BW
         ,lhs = BLHS
         ,rhs = #tree{weight = AW - BW
                     ,lhs = BRHS
                     ,rhs = ARHS
                     }
         }.

%% End of Module.
