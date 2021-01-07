---
title: "Skia 源码分析 - Chapter 2"
subtitle: "源码构建"
date: 2021-01-07T10:02:24+08:00
categories: ["cs"]
tags: ["graphics", "skia"]
draft: true
---

## 目录

* [Chapter 1 - 介绍](/posts/skia-1/)
* [Chapter 2 - 源码构建](/posts/skia-2/)
* [Chapter 3 - 模块初探](/posts/skia-3/)
* [Chapter 4 - 牛刀小试](/posts/skia-4/)
* ...

## 引言

在上一章的最后，我们已经拿到了 Skia 源码。我们这里使用的分支是以 `chrome/m*` 为开头的分支版本，`m`代表 Milestone，我们这里以 `m88` 为例，即 Milestone 88 版本。

简单查看该分支下的文件，主要有以下文件与目录
```
├── animations/
├── bazel/
├── bench/
├── bin/
├── build/
├── build_overrides/
├── buildtools/
├── client_utils/
├── common/
├── demos.skia.org/
├── dm/
├── docker/
├── docs/
├── example/
├── experimental/
├── fuzz/
├── gm/
├── gn/
├── include/
├── infra/
├── modules/
├── platform_tools/
├── resources/
├── samplecode/
├── site/
├── specs/
├── src/
├── tests/
├── third_party/
├── tools/
├── AUTHORS*
├── BUILD.bazel
├── BUILD.gn
├── codereview.settings
├── CONTRIBUTING
├── CQ_COMMITTERS
├── DEPS
├── go.mod
├── go.sum
├── LICENSE
├── OWNERS
├── PRESUBMIT.py
├── public.bzl
├── README
├── README.chromium
├── RELEASE_NOTES.txt
├── whitespace.txt
└── WORKSPACE.bazel
```

我们将会在下一章中详细介绍每一个文件与目录是什么。在那之前，我们首先需要对 Skia 的源码进行构建，获得一个“Skia 原来是这么玩”的一个直观印象。

## 官方构建

构建 Skia 最简单快速的方式是直接使用其官方构建方式。在上一章的最后，已经让 Skia 构建工具为我们准备好了 `gn` 工具与其所依赖的三方库。其中，`gn` 是一个“元"构建工具，可以帮助生成 Ninja 的构建文件，命令如下
```
bin/gn gen out/Debug --args='is_debug=true is_component_build=true'
```

在这条命令中，`gn` 根据用户给定的 `--args` 参数与 Skia 根目录下的 `BUILD.gn` 脚本
* 判断要构建的是正式版还是开发版
* 判断要构建的是动态链接版本还是静态链接版本
* 判断构建平台的操作系统
* 判断构建平台的 CPU 指令集
* 判断构建平台的编译器种类

然后根据这些判断
* 指定各个源码模块与源码文件的组合
* 指定编译器参数，包括控制条件编译的宏

我们让 Ninja 读取指定目录下（此处是 `out/Debug`）的构建文件，调用编译器来构建 Skia。
```
ninja -C out/Debug
```

最后，我们便会得到 `libskia.so` 动态链接库或者 `libskia.a` 静态链接库。

### 官方样例

目前为止，我们只得到了一个编译好的 Skia 二进制链接库。而通过官方样例，我们可以看到 Skia 渲染出的效果。

## CMake 构建
