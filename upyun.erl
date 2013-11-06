-module(upyun).

-export([get/1, usage/0, info/1, delete/1, mkdir/1, put/2]).
-import(md5, [md5/1]).
%-import(digest_auth, [request/6]).
%% http method
-define(POST, post).
-define(PUT, put).
-define(GET, get).
-define(DELETE, delete).
-define(HEAD, head).

-define(Bucket, "your bucket_name").
-define(UserName, "your user_name").
-define(Password, "your pwd"). 

%% request url
% 根据网络条件自动选择接入点
-define(ED_AUTO, "v0.api2.upyun.com").
% 电信接入点
-define(ED_TELECOM, "v1.api.upyun.com").
% 网通接入点
-define(ED_CNC, "v2.api.upyun.com").
% 移动铁通接入点
-define(ED_CTT, "v3.api.upyun.com").

% request optins
-define(DEFAULT_TIMEOUT, "30000").
-define(HEADER_SDK_VERSION, "x-up-version").
-define(HEADER_MKDIR, "x-up-mkdir").
-define(HEADER_AUTH, "Authorization").
-define(HEADER_CONTENTLENGTH, "Content-Length").

-define(HEADER_UP_FILE_TYPE, "x-up-file-type").
-define(HEADER_UP_FILE_SIZE, "x-up-file-size").
-define(HEADER_UP_FILE_DATE, "x-up-file-date").

%% 上传文件
put(Url, LocalFilePath) ->
    RequestUrl = lists:append(["http://", ?ED_AUTO, Url]),
    case LocalFilePath =/= '' of
        true ->
            %{ok, F} = file:open(LocalFilePath, read),
            {ok, Binary}=file:read_file(LocalFilePath),
            Data = erlang:binary_to_list(Binary);
        false ->
            Data = ''
    end,
    do_put(RequestUrl, Url, Data).

%% 创建目录
mkdir(Url) ->
    RequestUrl = lists:append(["http://", ?ED_AUTO, Url]),
    do_post(RequestUrl, Url).

%% 删除文件或目录
delete(Url) ->
    RequestUrl = lists:append(["http://", ?ED_AUTO, Url]),
    do_delete(RequestUrl, Url).

%% 获得文件或目录信息
info(Url) ->
    RequestUrl = lists:append(["http://", ?ED_AUTO, Url]),
    do_head(RequestUrl, Url).

%% 查看空间使用量
usage() ->
    Url = lists:append(["/", ?Bucket, "/?usage"]),
    RequestUrl = lists:append(["http://", ?ED_AUTO, "/", ?Bucket, "/?usage"]),
    do_get(RequestUrl, Url).

%% 读取文件
get(Url) ->
    RequestUrl = lists:append(["http://", ?ED_AUTO, Url]),
    {StateLine, ResponseHeader, Body} = do_get(RequestUrl, Url),
    Body.

do_put(RequestUrl, Url, Data) ->
     do_basic(post, RequestUrl, Url, Data).  


do_get(RequestUrl, Url) ->
    do_basic(get, RequestUrl, Url, '').

do_head(RequestUrl, Url) ->
    do_basic(head, RequestUrl, Url, '').

do_delete(RequestUrl, Url) ->
    do_basic(delete, RequestUrl, Url, '').

do_post(RequestUrl, Url) ->
    do_basic(post, RequestUrl, Url, '').

do_basic(Method, RequestUrl, Url, Data) ->
    ContentLength = string:len(Data),
    GMTDate = get_gmt_time(),
    MethodStr = string:to_upper(atom_to_list(Method)),
    Header = [{"Date", GMTDate},
              {?HEADER_SDK_VERSION, "3.0"},
              {?HEADER_AUTH, do_sign(MethodStr, Url, GMTDate,integer_to_list(ContentLength))},
              {"timeout", "30000"},
              {?HEADER_MKDIR, "true"}],
    inets:start(),
    {ok, Result} = case Method of 
        post->
            httpc:request(Method, {RequestUrl, Header, "", Data}, [], []);
        put ->
            httpc:request(Method, {RequestUrl, Header, "", Data}, [], []); 
        _ ->
            httpc:request(Method, {RequestUrl, Header}, [], [])
    end,
    inets:stop(),
    Result. 

do_sign(Method, Url, GmtDate, ContentLength) ->
    Sign = string:join([Method, Url, GmtDate, ContentLength, md5:md5(?Password)], "&"),
    lists:concat(["Upyun ", ?UserName, ":", md5:md5(Sign)]).

get_gmt_time() ->
    {{Year,Month,Day},{Hour,Minutes,Seconds}} = calendar:universal_time(),
    WeekDay = day(calendar:day_of_the_week(Year, Month, Day)),
    lists:concat([WeekDay, ', ', Day, " ", month_to_list(Month) , " ", Year, " ", Hour, ":", Minutes, ":", Seconds, " GMT"]).

day(1) -> "Mon";
day(2) -> "Tue";
day(3) -> "Wed";
day(4) -> "Thu";

day(5) -> "Fri";
day(6) -> "Sat";
day(7) -> "Sun".

month_to_list(1)  -> "Jan";
month_to_list(2)  -> "Feb";
month_to_list(3)  -> "Mar";
month_to_list(4)  -> "Apr";
month_to_list(5)  -> "May";
month_to_list(6)  -> "Jun";
month_to_list(7)  -> "Jul";
month_to_list(8)  -> "Aug";
month_to_list(9)  -> "Sep";
month_to_list(10) -> "Oct";
month_to_list(11) -> "Nov";
month_to_list(12) -> "Dec".

list_to_month("Jan") -> 1;
list_to_month("Feb") -> 2;
list_to_month("Mar") -> 3;
list_to_month("Apr") -> 4;
list_to_month("May") -> 5;
list_to_month("Jun") -> 6;
list_to_month("Jul") -> 7;
list_to_month("Aug") -> 8;
list_to_month("Sep") -> 9;
list_to_month("Oct") -> 10;
list_to_month("Nov") -> 11;
list_to_month("Dec") -> 12.
