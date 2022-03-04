%%% rtree header file defining macros and types
-define(MIN, 2).

%% Set Max at low value during testing
-ifdef(TEST).
-define(MAX, 3).
-else.
-define(MAX, 8).
-endif.
