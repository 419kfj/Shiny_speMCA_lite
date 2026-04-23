# app.R 0.92aから分離 2026/04/21
library(shiny)
library(shinythemes)
library(GDAtools)
library(tidyverse)
library(ggrepel)
library(DT)
library(plotly)
library(showtext)
# 追加ライブラリ
library(shinyWidgets)

showtext::showtext_auto(TRUE)

## 履歴

# - 2026/04/21 ver0.92a res.speMCAの表示を「一覧」「リスト」で追加。
# - 2026/04/19 ver0.92 使い方のlinkを追加。使い方は、quarto doc でwww.fujimotolabo.uk/Shiny_app_how2/
# - 2026/04/19 ver0.91 eta2グラフを、active、追加変数統合して表示。1−2軸、3−2軸、1−3軸
# - 2026/04/17 ver 0.8 交互作用plotをplotly化して、1−2、3−2、1−3次元も描画した。　
# - 2026/04/06 ver 0.7 個体グラフ描画をplotlyを使うものに変更（変数マップでは文字が消える）
# - 2026/02/24 ver 0.6 initial release

# DESCRIPTIONファイルから情報を読み取る
desc_info <- read.dcf("DESCRIPTION")
app_version <- desc_info[1, "Version"]

# is_debug_mode <- file.exists("./code_check/debug_data.rds")
#
# # 2. データの準備（アプリ起動時に1回だけ実行される）
# if (is_debug_mode) {
#   # デバッグ時は保存済みのRDSを読み込む
#   mca_result <- readRDS("./code_check/debug_data.rds")
#   message("Debug Mode: Loaded data from RDS.")
# }
# #message("Production Mode: Processing data...")

