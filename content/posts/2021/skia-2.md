---
title: "Skia 源码分析 - Chapter 2"
subtitle: "源码构建"
date: 2021-01-07T10:02:24+08:00
categories: ["cs"]
tags: ["graphics", "skia"]
---

## 目录

* [Chapter 1 - 介绍](/posts/skia-1/)
* [Chapter 2 - 源码构建](/posts/skia-2/)
* [Chapter 3 - 牛刀小试](/posts/skia-3/)
* [Chapter 4 - 模块初探](/posts/skia-4/)
* ...

## 引言

在上一章的最后，我们已经拿到了 Skia 源码以及三方依赖库的源码。其实，在 Skia 的源码库里有很多分支，比如以 `chrome/m*` 为开头的分支版本。这里的 `m`代表 Milestone，即 Skia 的主要公开版本。我们接下来所有的讨论都会基于 `chrome/m88` 分支，即 Milestone 88 版本。要切换到该分支，执行如下命令即可
```
git checkout chrome/m88
```

我们简单查看该分支中的内容，主要有以下文件与目录
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

可以看到，Skia 目录中有非常多的文件与目录。这往往会让我们云里雾里，不知道从哪里开始看起。不过没关系，让我们先从对 Skia 源码的构建开始，慢慢揭开 Skia 源码的神秘面纱。

## 官方构建

构建 Skia 最简单快速的方式是直接使用其官方构建方式。在上一章的最后，我们已经让 Skia 为我们准备好了 `gn` 工具。`gn` 是一个“元"构建工具，可以帮助我们生成 Ninja 的构建文件，从而对 Skia 进行构建。生成 Ninja 构建文件的命令如下
```
bin/gn gen out/Official --args='is_official_build=true'
```

在这条命令中，`gn` 根据用户给定的 `--args` 参数与 Skia 根目录下的 `BUILD.gn` 脚本
* 判断要构建的是官方配置好的正式版还是开发版（通过指定`is_official_build=true`来设定为正式版本，默认为`false`）
* 判断要构建的是动态链接版本还是静态链接版本（通过指定`is_component_build=true`来设定为动态链接版本，默认为`false`）
* 判断构建平台的操作系统（`gn`自行判断，下同）
* 判断构建平台的 CPU 指令集
* 判断构建平台的编译器种类

然后根据这些判断，`gn` 会
* 指定各个源码模块与源码文件的组合
* 指定编译器参数，包括控制条件编译的宏

从而生成 Ninja 构建文件。其实这些构建文件会有很多，因此生成的是一个目录。

我们让 Ninja 读取该目录下的构建文件，即可调用编译器来构建 Skia。
```
ninja -C out/Official
```

在这里的 `Official` 构建中，我们最终会得到一个大约 20 多兆的 `libskia.a` 文件。这便是 Skia 可以给其他工程使用的预编译好的二进制静态链接库。

## 官方样例构建

目前为止，我们只得到了一个编译好的 Skia 二进制链接库，仅此而已。

有没有办法看到 Skia 渲染出的二维图形呢？当然可以。Skia 自带了一些用于测试与调试的样例。不过在 `gn` 的构建脚本中，对这些样例的编译做了一些限制：
1. 不能是 `Official` 构建。
2. 不能是动态链接版本的构建，因为这些样例都是静态链接到 Skia 的二进制库的。

有兴趣的小伙伴们可以自行分析 `BUILD.gn` 这个构建脚本以及 `gn/` 目录下的相关脚本。在这里，我们只要执行下面的命令即可
```
bin/gn gen out/Debug
ninja -C out/Debug
```

在等待构建的时候，我们可以通过 `gn args` 命令查看此处的构建与我们第一次的 `Official` 构建有什么不同
```
bin/gn args out/Official --list > official_args
bin/gn args out/Debug --list > debug_args

# 如果安装了 vim
vimdiff official_args debug_args

# 否则使用系统自带的 diff 命令
diff -u official_args debug_args | less
```

可以看到，这两个版本的构建，除了一些关键参数的明显区别以外，`Debug` 版本
* 打开了一些工具的编译，如
  * `skia_enable_tools=true`，其中官方样例的编译就是依赖它（该参数会与 `is_official_build=true` 冲突）。
  * `skia_enable_android_utils=true`
* 打开了一些 GPU 调试层的编译，如
  * `skia_enable_gpu_debug_layers=true`
  * `skia_enable_direct3d_debug_layer=true`
  * `skia_enable_vulkan_debug_layers=true`
* 打开了一些编程语言上的特性，如
  * `skia_enable_sksl_interpreter=true` 是打开了 SKSL 语言的解释器
  * `skia_enable_skvm_jit_when_possible=true` 是允许 SKVM 的 JIT 特性
  * `skia_enable_spirv_validation=true` 是打开 SPIRV 的验证
* 额外使用了一些三方库，如
  * `skia_use_libheif=true` 是为了使用 HEIF 图像格式
  * `skia_use_lua=true` 是为了使用 Lua 作为脚本语言
* 编译并链接使用下载的三方库的源码，而不是使用系统自带的三方库。比如 `skia_use_system_zlib=false` 的意思是编译并链接使用 `third_party/externals/zlib` 目录下的 zlib 源码，而不使用系统提供的 zlib 二进制预编译库。这是为了在调试的时候可以跟踪到三方库的源码里面，而系统自带的二进制预编译库一般是没有这个调试信息的。

等到 `Debug` 版本构建完成，我们可以发现 `out/Debug` 目录下比 `Official` 版本多出一些可执行文件，比如 `viewer`、`SkiaSDLExample`、`HelloWorld` 等。

我们可以在根目录下运行 `viewer`（因为它会读取当前目录下的 `resources` 目录）

{{< figure src="/img/skia-viewer.png" width="500px" >}}

`viewer` 默认使用 CPU 后端，我们使用 `d` 键切换到 GPU 后端，比如 OpenGL 后端，然后按左右方向键切换用例。另外也可以按 `h` 键查看 `viewer` 的帮助。通过它我们可以对 Skia 的渲染能力有一个初步的感性认识。

这里需要说明的是，`viewer` 中样例的主要目的是为了对 Skia 进行测试与调试，比如有些从 Chrome 报告过来的 bugs，就会在 `viewer` 中复现与调试。因此这些样例除了少数几个，基本在图形美感与意义方面没有过多的设计。

## 自定义构建

### 自定义构建的原因

我们在上述小节中已经看到，Skia 官方基于 `gn` 工具的构建会有一些问题。比如说众多的编译参数，不仅没有详细的文档，而且参数之间还可能存在冲突。这些构建的规则其实完全写在 `BUILD.gn` 文件与 `gn/` 目录下众多的脚本当中。要分析这些脚本、从而设定恰当的编译选项，得到自己想要的构建版本，其实是一件非常吃力不讨好的事情。

比如说，我们会发现 `Debug` 构建版本生成的 `libskia.a` 竟然有 600M 的大小，而各个可执行文件的体积也都在两三百兆左右。相比之下，`Official` 构建版本生成的 `libskia.a` 文件只有 20 多兆。

要记住，我们刚才生成 `Debug` 版本的目的是为了得到官方样例程序，比如 `viewer`。那么有没有可能生成一个 `Release` 版本的 `viewer`，并且让生成的文件小一点呢？经过对脚本的研究，我们发现使用 `is_skia_dev_build=true` 是不行的，因为这个是 Skia 内部的私有参数。因此我们只能
```
bin/gn gen out/Release --args="is_debug=false"
ninja -C out/Release
```

然后悲剧地发现，生成的 `libskia.a` 文件虽然小了一点，但也还是有将近 600M 的大小。而那些可执行文件的大小却几乎没怎么变化。

也就是说，虽然 Skia 官方的构建方式十分适合正式版的构建场景与测试版版的开发场景，但对于我们研究 Skia 源码却是一个不小的阻碍。

因此，我们倒不如从零出发，构建出一套构建体系。可能没有 Skia 官方构建脚本中那么细致，但至少让我们能够对 Skia 的源码与构建过程有一个清清楚楚、明明白白的理解。
