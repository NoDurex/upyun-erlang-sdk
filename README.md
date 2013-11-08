upyun-erlang-sdk
================

配置基本项
---

在upyun.erl中

* 定义空间名，操作员账户名，密码：

```erlang
-define(Bucket, "your bucket_name").
-define(UserName, "your user_name").
-define(Password, "your pwd").

```

* 定义域名接入点，一般情况下选用 __ED_AUTO__

``` erlang
%% request url
% 根据网络条件自动选择接入点
-define(ED_AUTO, "v0.api.upyun.com").
% 电信接入点
-define(ED_TELECOM, "v1.api.upyun.com").
% 网通接入点
-define(ED_CNC, "v2.api.upyun.com").
% 移动铁通接入点
-define(ED_CTT, "v3.api.upyun.com").
```


使用方法
---

引用upyun.erl module.

```
-import(upyun, [get/1, usage/0, info/1, delete/1, mkdir/1, put/2]).
```

### Method

* 上传文件

```
upyun:put("/bucket_name/text.txt", "/home/upyun/text.txt").
```

* 创建目录

```
upyun:mkdir("/bucket_name/folder1/folder2/").
```

* 删除文件或目录


```
% 删除文件
upyun:delete("/bucket_name/text.txt").

% 删除目录
upyun:delete("/bucket_name/folder/folder/").
```

* 获得文件或目录信息

```
% 获得文件信息
upyun:info("/bucket_name/text.txt").

% 获得目录信息
upyun:info("/bucket_name/folder1/folder2/").
```

* 查看空间使用情况

```
% 查看空间使用情况
upyun:usage().
```

* 读取文件

```
% 读取文件
upyun:get("/bucket_name/text.txt").
```


Ps：目前为beta版本，各个接口较简易，没有做参数正确性等判断，后续将完善各个功能。
