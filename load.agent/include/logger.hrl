-define(log_info(Msg), io:format("INFO: " ++ Msg ++ "~n", [])).
-define(log_info(Format, Args), io:format("INFO: " ++ Format ++ "~n", Args)).

-define(log_error(Msg), io:format("ERROR: " ++ Msg ++ "~n", [])).
-define(log_error(Format, Args), io:format("ERROR: " ++ Format ++ "~n", Args)).