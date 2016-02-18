-module(request).
-export([create_req_id/1]).
-define(REQ_ID, "req_id").


create_req_id(KeyValuePid) ->
    key_value_gen:get_then_increment(?REQ_ID, KeyValuePid).
