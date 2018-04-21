%% OTP Specs

-type otp_app_start_type() :: normal | {takeover, node()} | {failover, node()}.
-type otp_app_start_ret() :: {ok, pid()} | {ok, pid(), term()} | {error, term()}.

-type otp_sup_start_ret() :: {ok, pid()} | ignore | {error, term()}.
-type otp_sup_init_ret() ::
    {ok, {SupFlags :: supervisor:sup_flags(), [ChildSpec :: supervisor:child_spec()]}} |
    ignore.

-type otp_gen_srv_start_ret() :: {ok, Pid :: pid()} | ignore | {error, Reason :: term()}.
-type otp_gen_srv_init_ret() :: 
    {ok, State :: term()} | {ok, State :: term(), timeout() | hibernate} |
    {stop, Reason :: term()} | ignore.
-type otp_gen_srv_call_ret() ::
    {reply, Reply :: term(), NewState :: term()} |
    {reply, Reply :: term(), NewState :: term(), timeout() | hibernate} |
    {noreply, NewState :: term()} |
    {noreply, NewState :: term(), timeout() | hibernate} |
    {stop, Reason :: term(), Reply :: term(), NewState :: term()} |
    {stop, Reason :: term(), NewState :: term()}.
-type otp_gen_srv_cast_ret() ::
    {noreply, NewState :: term()} |
    {noreply, NewState :: term(), timeout() | hibernate} |
    {stop, Reason :: term(), NewState :: term()}.
-type otp_gen_srv_info_ret() ::
    {noreply, NewState :: term()} |
    {noreply, NewState :: term(), timeout() | hibernate} |
    {stop, Reason :: term(), NewState :: term()}.

