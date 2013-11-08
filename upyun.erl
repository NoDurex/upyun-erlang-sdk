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

-define(Bucket, "your bucket name").
-define(UserName, "your account name").
-define(Password, "your password"). 

%% request url
% 根据网络条件自动选择接入点
-define(ED_AUTO, "v0.api.upyun.com").
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
-define(HEADER_TIMEOUT, "timeout").
-define(HEADER_DATE, "Date").

-define(HEADER_UP_FILE_TYPE, "x-up-file-type").
-define(HEADER_UP_FILE_SIZE, "x-up-file-size").
-define(HEADER_UP_FILE_DATE, "x-up-file-date").
-define(HEADER_UP_FOLDER, "x-up-folder").

%% 上传文件
put(Url, LocalFilePath) ->
    case file:read_file(LocalFilePath) of
        {ok, Binary} ->
            Data = binary_to_list(Binary);
        _ ->
            Data = ''
    end,
    do_put(Url, Data, true).

%% 创建目录
mkdir(Url) ->
    do_post(Url, true).

%% 删除文件或目录
delete(Url) ->
    do_delete(Url).

%% 获得文件或目录信息
info(Url) ->
    do_head(Url).

%% 查看空间使用量
usage() ->
    Url = lists:append(["/", ?Bucket, "/?usage"]),
    do_get(Url).

%% 读取文件
get(Url) ->
    %{StateLine, ResponseHeader, Body} = do_get(Url),
    {_, _, Body} = do_get(Url),
    Body.

do_put(Url, Data, AutoMkDir) ->
    case Data of
        '' ->
            {error, file_is_not_exsits};
        _ ->
            do_basic(post, Url, Data, AutoMkDir)
    end.

do_get(Url) ->
    do_basic(get, Url, "", undefined).

do_head(Url) ->
    do_basic(head, Url, "", undefined).

do_delete(Url) ->
    do_basic(delete, Url,"", undefined).

do_post(Url, AutoMkDir) ->
    do_basic(post, Url, "", AutoMkDir).

do_basic(Method, Url, Data, AutoMkDir) ->
    RequestUrl = lists:append(["http://", ?ED_AUTO, Url]),
    ContentLength = string:len(Data),
    GMTDate = get_gmt_time(),
    MethodStr = string:to_upper(atom_to_list(Method)),
    Header = [{?HEADER_DATE, GMTDate},
              {?HEADER_SDK_VERSION, "3.0"},
              {?HEADER_AUTH, do_sign(MethodStr, Url, GMTDate,integer_to_list(ContentLength))},
              {?HEADER_TIMEOUT, "30000"}],
    case AutoMkDir =/= undefined of 
        true ->
            TrailHeader = [{?HEADER_MKDIR, atom_to_list(AutoMkDir)}] ++ Header;
        false ->
            TrailHeader = Header
    end,
    inets:start(),
    {ok, Result} = case Method of
        post->
            MkdirHeader = [{?HEADER_UP_FOLDER, "true"}] ++  TrailHeader,
            httpc:request(Method, {RequestUrl, MkdirHeader, "", Data}, [], []);
        put ->
            httpc:request(Method, {RequestUrl, TrailHeader, "", Data}, [], []);
        _ ->
            httpc:request(Method, {RequestUrl, TrailHeader}, [], [])
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
