%% Copyright © 2018 Pierre Fenoll ‹pierrefenoll@gmail.com›
%% See LICENSE for licensing information.
%% -*- coding: utf-8 -*-
-ifndef(ROPES_HRL).
-define(ROPES_HRL, true).

-record(tree, {weight :: integer()
              ,lhs :: #tree{}
              ,rhs :: #tree{}
              }).

-type thresh() :: pos_integer().
-record(rope, {len :: non_neg_integer()
              ,thresh = 1 :: thresh()
              ,tree :: maybe(#tree{})
              }).

%% Note: indices start at zero
-type index() :: non_neg_integer().

-type maybe(T) :: nil | T.
-type str() :: char() | nonempty_string() | binary(). %% Not really iodata()
-define(NE_BINARY, <<_:8,_/binary>>).
-define(IS_INDEX(Idx), is_integer(Idx), Idx >= 0).

-endif.
