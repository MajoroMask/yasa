
<!-- README.md is generated from README.Rmd. Please edit that file -->

# HZSK_LCMS_application

<!-- badges: start -->

[![Lifecycle:
experimental](https://img.shields.io/badge/lifecycle-experimental-orange.svg)](https://lifecycle.r-lib.org/articles/stages.html#experimental)
<!-- badges: end -->

`HZSK_LCMS_application`是基于R/shiny的申科LC-MS数据分析应用工具。

## 安装和部署

你可以使用以下代码，安装开发版本:

``` r
library(devtools)
install_github("ShenkeBio/HZSK_LCMS_application")
```

安装成功后，使用以下代码启动shiny app：

``` r
library(yasa)
options(shiny.port = 3838, shiny.host = "0.0.0.0")
run_app()
```

或者，你可以使用我们预装好的docker容器启用shiny app：

``` bash
docker run --rm \
  -p 3838:3838 \
  quay.io/shenkebio/yasa:0.0.0.9008 \
  R -e 'options(shiny.port = 3838, shiny.host = "0.0.0.0"); library(yasa); run_app()'
```

## 中文本地化

目前HZSK_LCMS_application支持中文和英文两种语言。可通过以下设置，在启用app时启用中文：

``` r
library(yasa)

set_i18n('cn')
set_i18n(system.file('ext/i18n_cn.csv', package = 'yasa')

run_app()
```

## 更多问题

如果你遇到问题或发现bug，请在[GitHub
issue](https://github.com/ShenkeBio/HZSK_LCMS_application/issues%60)提交。

本项目基于[golem开发框架](https://golemverse.org/)，更多文档请参见。

- [Mastering Shiny](https://mastering-shiny.org/index.html)
- [Engineering Production-Grade Shiny
  Apps](https://engineering-shiny.org/index.html)
