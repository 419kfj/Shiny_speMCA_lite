# app.R -- speMCA フルアプリ（.rda アップロード対応、GDAtools 関数版）
# 2025/10/05 19:59version
library(shiny)
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

# ui ----
ui <- fluidPage(
  #  shinyWidgets::useShinyWidgets(),
  titlePanel("speMCA 分析アプリ"),
  sidebarLayout( ## sidebarLayout ----
    sidebarPanel(
      # バージョン情報の表示
      helpText(paste("Version:", app_version)),
#      hr(), # 区切り線
      fileInput("file", "rdaファイルをアップロードしてください", accept = ".rda"),
      actionButton("load_rda", "rdaファイルを読み込む", class = "btn-primary"),
      hr(),

      # 動的に生成する UI（df が読み込まれた後に表示）
      uiOutput("variable_selectors"),
      uiOutput("junk_selector"),
      hr(),
      downloadButton("download_mca", "speMCA結果をダウンロード"),
      uiOutput("supvar_selectors"),
      uiOutput("interaction_selectors"),
      uiOutput("ellipse_selectors"),

#      hr(),
#      downloadButton("download_mca", "speMCA結果をダウンロード")
    ),

    mainPanel( ## mainPanel ----
      tabsetPanel(
        tabPanel("選択変数", uiOutput("selected_info")),
        tabPanel("修正慣性率", tableOutput("eig_table"), plotOutput("eig_plot")),
        tabPanel("変数マップ",
                 plotOutput("var_map_12", height = "1200px"),
                 plotOutput("var_map_32", height = "1200px"),
                 plotOutput("var_map_13", height = "1200px")),
        tabPanel("η2マップ",
                 plotOutput("eta2_map_12",height = "80vh"),# height="1200px"),
                 plotOutput("eta2_map_32",height = "80vh"),
                 plotOutput("eta2_map_13",height = "80vh")),
        tabPanel("個体マップ",
                 plotlyOutput("ind_map_12", height = "80vh"),
                 plotlyOutput("ind_map_32", height = "80vh"),
                 plotlyOutput("ind_map_13", height = "80vh")),
        tabPanel("データ表示", DTOutput("data_table")),
        tabPanel("supvarsの情報", verbatimTextOutput("supvars_out")),
        tabPanel("変数マップ＋supvars", plotOutput("supvars_map", height = "1200px")),
        tabPanel("交互作用plot", plotOutput("interaction_map", height = "1200px")), # plotlyだと文字がでない
        tabPanel("集中楕円",
                 plotlyOutput("kellipses_map_12", height = "80vh"),
                 plotlyOutput("kellipses_map_32", height = "80vh"),
                 plotlyOutput("kellipses_map_13", height = "80vh")
                 ),
        tabPanel("speMCAのresult:リスト",
                 listviewer::jsoneditOutput("mca_result_tree", height = "600px")),
        tabPanel("speMCAのresult:一覧", verbatimTextOutput("mca_result_list")),
        tabPanel("使い方",
                 p("アプリの詳細な使い方は、以下のリンク先をご確認ください。"),
                 a("Shinyアプリの使い方ガイド（外部サイト）",
                   href = "https://www.fujimotolabo.uk/Shiny_app_how2/",
                   target = "_blank") # 新しいタブで開く設定
        )
      )
    )
  )
)

# server =====
server <- function(input, output, session) {

## rda 読み込み：load ボタンで eventReactive ----

  dataset <- eventReactive(input$load_rda, {
    req(input$file)
    e <- new.env()
    obj_names <- load(input$file$datapath, envir = e)
    objs <- mget(obj_names, envir = e)
    message("ロードされたオブジェクト: ", paste(names(objs), collapse = ", "))
    objs
  })

# （.rda の最初のオブジェクトを）df として使う ----
  df_reactive <- reactive({
    req(dataset())
    dataset()[[1]]
  })

# 動的 UI（df があるとき）

# Active変数の選択 -------------------------------------------------------------
  output$variable_selectors <- renderUI({
    req(df_reactive())
    choices <- names(df_reactive())
    selectInput("variables", "Active変数を選んでください",
                choices = choices, multiple = TRUE, selectize = FALSE, size = 7)
  })

# 追加変数の選択 -----------------------------------------------------------------
  output$supvar_selectors <- renderUI({
    req(df_reactive())
    choices <- names(df_reactive())
    selectInput("supvars", "追加変数を選んでください",
                choices = choices, multiple = TRUE, selectize = FALSE, size = 7)
  })

# 交互作用変数の選択 ----
  output$interaction_selectors <- renderUI({
    req(df_reactive())
    choices <- names(df_reactive())
    tagList(
      selectInput("inter_v1", "交互作用 v1 を選んでください",
                  choices = choices, selected = choices[1]),
      uiOutput("inter_v1_selector"),
      selectInput("inter_v2", "交互作用 v2 を選んでください",
                  choices = choices, selected = choices[min(2,length(choices))]),
      uiOutput("inter_v2_selector")
    )
  })

  ### 交互作用変数 v1 の「全選択/全解除」----
  observeEvent(input$select_v1_all, {
    req(input$inter_v1)
    df <- df_reactive()
    v1lv <- levels(as.factor(df[[input$inter_v1]]))
    updateCheckboxGroupInput(session, "selected_categories_v1", selected = v1lv)
  })
  observeEvent(input$deselect_v1_all, {
    updateCheckboxGroupInput(session, "selected_categories_v1", selected = character(0))
  })


  ### 交互作用変数 v2 の「全選択/全解除」----
  observeEvent(input$select_v2_all, {
    req(input$inter_v2)
    df <- df_reactive()
    v2lv <- levels(as.factor(df[[input$inter_v2]]))
    updateCheckboxGroupInput(session, "selected_categories_v2", selected = v2lv)
  })
  observeEvent(input$deselect_v2_all, {
    updateCheckboxGroupInput(session, "selected_categories_v2", selected = character(0))
  })


# 集中楕円変数の選択 ----
  output$ellipse_selectors <- renderUI({
    req(df_reactive())
    choices <- names(df_reactive())
    tagList(
      selectInput("var_ellipses", "集中楕円表示変数を選んでください",
                  choices = choices, multiple = FALSE),
      uiOutput("kellipses_cat_selector")  # 選んだ変数に応じたカテゴリチェックボックスを生成
    )
  })


## 集中楕円変数の「全選択/全解除」----
  observeEvent(input$select_all, {
    req(input$var_ellipses)
    df <- df_reactive()
    lv <- levels(as.factor(df[[input$var_ellipses]]))
    updateCheckboxGroupInput(session, "selected_categories", selected = lv)
  })

  observeEvent(input$deselect_all, {
    updateCheckboxGroupInput(session, "selected_categories", selected = character(0))
  })


# junkカテゴリの選択（getindexcat を使う）----
  #  ※ input$variables が2個以上必要（getindexcat の前提）
  junk_cat <- reactive({
    req(df_reactive())
    if (is.null(input$variables) || length(input$variables) < 2) return(NULL)
    df <- df_reactive()
    df_sub <- df[, input$variables, drop = FALSE]
    jc <- getindexcat(df_sub)
    if (is.null(jc) || length(jc) == 0) return(NULL)
    jc
  })


### juck selector ----
  output$junk_selector <- renderUI({
    jc <- junk_cat()
    if (is.null(jc)) return(NULL)
    tagList(
      selectInput("excluded_cats", "juck指定するカテゴリを選択してください",
                  choices = jc, multiple = TRUE, selectize = FALSE,
                  size = min(10, length(jc))),
      actionButton("run_mca", "speMCAを実行する")
    )
  })

# speMCA 実行（ボタン押下で eventReactive）----

  mca_result <- eventReactive(input$run_mca, {
    req(df_reactive())
    req(input$variables)
    if (length(input$variables) < 2) {
      showNotification("Active変数は少なくとも2つ選んでください。", type = "warning")
      return(NULL)
    }
    df <- df_reactive()
    df_sub <- df[, input$variables, drop = FALSE]
    excl_indices <- NULL
    if (!is.null(input$excluded_cats) && length(input$excluded_cats) > 0) {
      jc <- junk_cat()
      excl_indices <- match(input$excluded_cats, jc)
      excl_indices <- excl_indices[!is.na(excl_indices)]
    }
    tryCatch({
      speMCA(df_sub, excl = excl_indices)
    }, error = function(e) { # ----
      showNotification(paste0("speMCA エラー: ", e$message), type = "error")
      NULL
    })
  })

# speMCAのresultを出力 ----

  output$mca_result_list <- renderPrint({
    # mca_result が reactive または reactiveValues 内にある場合の例
    # もし reactive なら mca_result() と呼び出す必要があります
    req(mca_result())

    # そのまま print する、あるいは summary(mca_result) としてもOK
    print(mca_result())
  })


# supvars の計算（mca_result と input$supvars に依存）----
  supvars_result <- reactive({
    req(mca_result())
    if (is.null(input$supvars) || length(input$supvars) == 0) return(NULL)
    df <- df_reactive()
    tryCatch({
      GDAtools::supvars(resmca = mca_result(), vars = df %>% select(all_of(input$supvars)))
    }, error = function(e) { # ----
      message("supvars エラー: ", e$message)
      NULL
    })
  })


# 交互作用変数 v1 のセレクタ生成 ----
  output$inter_v1_selector <- renderUI({
    req(df_reactive(), input$inter_v1)
    df <- df_reactive()
    v1lv <- levels(as.factor(df[[input$inter_v1]]))

    tagList(
      fluidRow(
        column(6, actionButton("select_v1_all", "全選択")),
        column(6, actionButton("deselect_v1_all", "全解除"))
      ),
      checkboxGroupInput(
        inputId = "selected_categories_v1", # <--- これが input$selected_categories_v1 になる
        label = "表示するカテゴリを選んでください",
        choices = v1lv,
        selected = v1lv # 初期値として全選択
      )
    )
  })

  # 交互作用変数 v2 のセレクタ生成 ----
  output$inter_v2_selector <- renderUI({
    req(df_reactive(), input$inter_v1)
    df <- df_reactive()
    v2lv <- levels(as.factor(df[[input$inter_v2]]))

    tagList(
      fluidRow(
        column(6, actionButton("select_v2_all", "全選択")),
        column(6, actionButton("deselect_v2_all", "全解除"))
      ),
      checkboxGroupInput(
        inputId = "selected_categories_v2", # <--- これが input$selected_categories_v2 になる
        label = "表示するカテゴリを選んでください",
        choices = v2lv,
        selected = v2lv # 初期値として全選択
      )
    )
  })


# 集中楕円変数 のカテゴリセレクタ -----------------------
  output$kellipses_cat_selector <- renderUI({
    req(df_reactive())
    req(input$var_ellipses)
    df <- df_reactive()
    var <- df[[input$var_ellipses]]
    lv <- levels(as.factor(var))

    tagList(
      fluidRow(
        column(6, actionButton("select_all", "全選択")),
        column(6, actionButton("deselect_all", "全解除"))
      ),
      checkboxGroupInput(
        inputId = "selected_categories",
        label = "表示するカテゴリを選んでください",
        choices = lv,
        selected = lv
      )
    )
  })


#  選択変数タブ（常に input に基づいて表示）----
  output$selected_info <- renderUI({
    req(df_reactive())  # df が読み込まれていること
    vars <- input$variables
    junk <- input$excluded_cats
    kellipse_cat <- input$selected_categories

    tagList(
      h4("Active 変数"),
      if (!is.null(vars) && length(vars) > 0) {
        HTML(paste0("<ul>", paste0("<li>", vars, "</li>", collapse = ""), "</ul>"))
      } else {
        em("なし")
      },

      h4("Junk カテゴリ"),
      if (!is.null(junk) && length(junk) > 0) {
        HTML(paste0("<ul>", paste0("<li>", junk, "</li>", collapse = ""), "</ul>"))
      } else {
        em("なし")
      },

      h4("集中楕円描画カテゴリ"),
      if (!is.null(kellipse_cat) && length(kellipse_cat) > 0) {
        HTML(paste0("<ul>", paste0("<li>", kellipse_cat, "</li>", collapse = ""), "</ul>"))
      } else {
        em("なし")
      }
    )
  })


# res.speMCAのリスト表示 ----
  output$mca_result_tree <- listviewer::renderJsonedit({
    req(mca_result())

    # speMCAの結果オブジェクトをそのまま渡す
    listviewer::jsonedit(
      mca_result(),
      mode = "view",    # 閲覧モード
      modes = c("view", "code") # 必要に応じてコード表示も切り替え可能
    )
  })


# 結果表示：データ表 / 修正慣性率 ----
  output$data_table <- renderDT({
    req(df_reactive())
    datatable(df_reactive(), options = list(pageLength = 10))
  })

  output$eig_table <- renderTable({
    res <- mca_result(); req(res)
    data.frame(
      軸 = seq_along(res$eig$mrate),
      修正慣性率 = res$eig$mrate,
      累積慣性率 = res$eig$cum.mrate
    )
  })

  output$eig_plot <- renderPlot({
    res <- mca_result(); req(res)
    df_eig <- data.frame(
      dim = seq_along(res$eig$mrate),
      mrate = res$eig$mrate,
      cum_mrate = res$eig$cum.mrate
    )
    ggplot(df_eig, aes(x = dim)) +
      geom_col(aes(y = mrate)) +
      geom_line(aes(y = cum_mrate), color = "red", size = 1) +
      geom_point(aes(y = cum_mrate), color = "red") +
      labs(x = "次元", y = "修正慣性率", title = "修正慣性率と累積慣性率") +
      theme_minimal()
  })

# η2マップ ----
## 1-2 軸 ----

  get_eta2_coord <- function(mca_data,df_data,supvar_names,axes){
    coord_eta2_sup0 <- dimeta2(resmca = mca_data,
                               vars = df_data %>% select(all_of(supvar_names)),
                               dim = c(1:5))
    coord_eta2_sup1 <- coord_eta2_sup0[,axes]
    colnames(coord_eta2_sup1) <- c("x", "y")
    coord_eta2_sup <- coord_eta2_sup1 %>% as_tibble() %>%
      mutate(
        vnames = supvar_names,
        x = x / 100,
        y = y / 100
      ) %>% select(3,1,2)
  }

  draw_eta2_map <- function(mca_data, axes,df_data ,supvar_names,title){
    # df_data <-　df_reactive()
    # supvar_names <- input$supvars
    p <- GDAtools::ggeta2_variables(resmca = mca_data,axes = axes) +
      theme(aspect.ratio = 1) #+ ggtitle("η2マップ1−2軸")
    supv <- supvar_names

    if(!is.null(supvar_names)){ # supvar_namesがNULLなら飛ばす
　　coord_eta2_sup <- get_eta2_coord(mca_data,df_data,supvar_names,axes)
    p <- p +
      # ポイントの追加
      geom_point(
        data = coord_eta2_sup,
        aes(x = x, y = y),
        size = 2,
        color = "blue" # active変数と区別するために色を付けると見やすいです
      ) +
      # ラベルの追加
      geom_text_repel(
        data = coord_eta2_sup,
        aes(x = x, y = y, label = vnames),
        size = 3,            # 文字の大きさ
        vjust = -1,          # 少し上にずらす（geom_textの場合）
        box.padding = 0.5    # ラベル同士の距離を調整（ggrepelの場合）
      )
    }
    p
  }

  # draw_ind_plot <- function(mca_data, axes, title) {
  #   # ここでは mca_result() ではなく、引数の mca_data を使う
  #   p <- GDAtools::ggcloud_indiv(mca_data, axes = axes) +
  #     theme(aspect.ratio = 1) +
  #     ggtitle(title)
  #
  #   ggplotly(p, tooltip = "text") %>%
  #     layout(yaxis = list(scaleanchor = "x", scaleratio = 1))
  # }
  #

  output$eta2_map_12 <- renderPlot({
    res <- mca_result()
    df_data <-　df_reactive()
    req(res)
#    req(input$supvars)
    supvar_names <- input$supvars
    draw_eta2_map(res, axes=c(1,2), df_data, supvar_names,"η2 1-2軸")
  })

  output$eta2_map_32 <- renderPlot({
    res <- mca_result()
    df_data <-　df_reactive()
    req(res)
#    req(input$supvars)
    supvar_names <- input$supvars
    draw_eta2_map(res, axes=c(3,2), df_data, supvar_names,"η2 3-2軸")
  })

  output$eta2_map_13 <- renderPlot({
    res <- mca_result()
    df_data <-　df_reactive()
    req(res)
 #   req(input$supvars)
    supvar_names <- input$supvars
    draw_eta2_map(res, axes=c(1,3), df_data, supvar_names,"η2 1-3軸")
  })


#   output$eta2_map_12 <- renderPlot({
#     res <- mca_result()
#     req(res)
#     req(input$supvars)
#     eta2_coord <- GDAtools::dimeta2(res,df_reactive() %>% select(all_of(input$supvars)),dim = c(1,2))
#     p <- GDAtools::ggeta2_variables(resmca = res,axes = c(1,2)) + theme(aspect.ratio = 1) + ggtitle("η2マップ1−2軸")
# 　　supv <- input$supvars
#     coord_eta2_sup <- dimeta2(resmca = res,
#                               vars = df_reactive() %>% select(all_of(input$supvars)),
#                               dim = c(1,2)) %>%
#       as_tibble() %>%
#       mutate(
#         vnames = input$supvars,
#         dim.1 = dim.1 / 100,
#         dim.2 = dim.2 / 100
#       ) %>%
#       select(vnames, dim.1, dim.2)
#
#     p <- p +
#       # ポイントの追加
#       geom_point(
#         data = coord_eta2_sup,
#         aes(x = dim.1, y = dim.2),
#         size = 2,
#         color = "blue" # active変数と区別するために色を付けると見やすいです
#       ) +
#       # ラベルの追加
#       geom_text_repel(
#         data = coord_eta2_sup,
#         aes(x = dim.1, y = dim.2, label = vnames),
#         size = 3,            # 文字の大きさ
#         vjust = -1,          # 少し上にずらす（geom_textの場合）
#         box.padding = 0.5    # ラベル同士の距離を調整（ggrepelの場合）
#       )
#     p
#   })

  ## 3-2 軸 ----
  # output$eta2_map_32 <- renderPlot({
  #   res <- mca_result()
  #   req(res)
  #   req(input$supvars)
  #   eta2_coord <- GDAtools::dimeta2(res,df_reactive() %>% select(all_of(input$supvars)),dim = c(3,2))
  #   p <- GDAtools::ggeta2_variables(resmca = res,axes = c(3,2)) + theme(aspect.ratio = 1) + ggtitle("η2マップ3-2軸")
  #   supv <- input$supvars
  #   coord_eta2_sup <- dimeta2(resmca = res,
  #                             vars = df_reactive() %>% select(all_of(input$supvars)),
  #                             dim = c(3,2)) %>%
  #     as_tibble() %>%
  #     mutate(
  #       vnames = input$supvars,
  #       dim.3 = dim.3 / 100,
  #       dim.2 = dim.2 / 100
  #     ) %>%
  #     select(vnames, dim.3, dim.2)
  #
  #   p <- p +
  #     # ポイントの追加
  #     geom_point(
  #       data = coord_eta2_sup,
  #       aes(x = dim.3, y = dim.2),
  #       size = 2,
  #       color = "blue" # active変数と区別するために色を付けると見やすいです
  #     ) +
  #     # ラベルの追加
  #     geom_text_repel(
  #       data = coord_eta2_sup,
  #       aes(x = dim.3, y = dim.2, label = vnames),
  #       size = 3,            # 文字の大きさ
  #       vjust = -1,          # 少し上にずらす（geom_textの場合）
  #       box.padding = 0.5    # ラベル同士の距離を調整（ggrepelの場合）
  #     )
  #   p
  # })
  #
  # ## 1-3 軸 ----
  # output$eta2_map_13 <- renderPlot({
  #   res <- mca_result()
  #   req(res)
  #   req(input$supvars)
  #   eta2_coord <- GDAtools::dimeta2(res,df_reactive() %>% select(all_of(input$supvars)),dim = c(1,3))
  #   p <- GDAtools::ggeta2_variables(resmca = res,axes = c(1,3)) + theme(aspect.ratio = 1) + ggtitle("η2マップ1-3軸")
  #   supv <- input$supvars
  #   coord_eta2_sup <- dimeta2(resmca = res,
  #                             vars = df_reactive() %>% select(all_of(input$supvars)),
  #                             dim = c(1,3)) %>%
  #     as_tibble() %>%
  #     mutate(
  #       vnames = input$supvars,
  #       dim.1 = dim.1 / 100,
  #       dim.3 = dim.3 / 100
  #     ) %>%
  #     select(vnames, dim.1, dim.3)
  #
  #   p <- p +
  #     # ポイントの追加
  #     geom_point(
  #       data = coord_eta2_sup,
  #       aes(x = dim.1, y = dim.3),
  #       size = 2,
  #       color = "blue" # active変数と区別するために色を付けると見やすいです
  #     ) +
  #     # ラベルの追加
  #     geom_text_repel(
  #       data = coord_eta2_sup,
  #       aes(x = dim.1, y = dim.3, label = vnames),
  #       size = 3,            # 文字の大きさ
  #       vjust = -1,          # 少し上にずらす（geom_textの場合）
  #       box.padding = 0.5    # ラベル同士の距離を調整（ggrepelの場合）
  #     )
  #   p
  # })


#  変数マップ（3枚）----

  draw_vari_plot <- function(mca_data, axes, title) {
    # ここでは mca_result() ではなく、引数の mca_data を使う
    p <- GDAtools::ggcloud_variables(mca_data, axes = axes) +
      theme(aspect.ratio = 1) +
      ggtitle(title)
    p
  }

  output$var_map_12 <- renderPlot({
    res <- mca_result() # Reactiveから値を取り出す
    req(res)            # データがあるか確認
    draw_vari_plot(res, c(1, 2), "変数マップ 1-2軸")
  })

  output$var_map_32 <- renderPlot({
  res <- mca_result() # Reactiveから値を取り出す
  req(res)            # データがあるか確認
  draw_vari_plot(res, c(3, 2), "変数マップ 3-2軸")
})

  output$var_map_13 <- renderPlot({
    res <- mca_result() # Reactiveから値を取り出す
    req(res)            # データがあるか確認
    draw_vari_plot(res, c(1, 3), "変数マップ 1-3軸")
  })


#  個体マップ（3枚）----

  # 1. outputのすぐ近くに定義（カプセル化）
  draw_ind_plot <- function(mca_data, axes, title) {
    # ここでは mca_result() ではなく、引数の mca_data を使う
    p <- GDAtools::ggcloud_indiv(mca_data, axes = axes) +
      theme(aspect.ratio = 1) +
      ggtitle(title)

    ggplotly(p, tooltip = "text") %>%
      layout(yaxis = list(scaleanchor = "x", scaleratio = 1))
  }

  # 2. 呼び出し
  output$ind_map_12 <- renderPlotly({
    res <- mca_result() # Reactiveから値を取り出す
    req(res)            # データがあるか確認
    draw_ind_plot(res, c(1, 2), "個体マップ 1-2軸")
  })

  output$ind_map_32 <- renderPlotly({
    res <- mca_result() # Reactiveから値を取り出す
    req(res)            # データがあるか確認
    draw_ind_plot(res, c(3, 2), "個体マップ 3-2軸")
  })


  output$ind_map_13 <- renderPlotly({
    res <- mca_result() # Reactiveから値を取り出す
    req(res)            # データがあるか確認
    draw_ind_plot(res, c(1, 3), "個体マップ 1-3軸")
  })


#  supvars の出力とマップ（GDAtools::ggadd_supvars） -----
  output$supvars_out <- renderPrint({
    res <- supvars_result()
    if (is.null(res)) "supvarsの結果がありません" else print(res)
  })

  output$supvars_map <- renderPlot({
    req(mca_result(), input$supvars)
    res <- mca_result()
    df <- df_reactive()
    tryCatch({
      base_map <- GDAtools::ggcloud_variables(res, col = "lightgrey")
      GDAtools::ggadd_supvars(p = base_map, resmca = res, vars = df %>% select(all_of(input$supvars))) +
        theme(aspect.ratio = 1)

      # ggplotly(p, tooltip = "all") %>%
      #   layout(
      #     # y軸とx軸の比率を 1:1 に固定
      #     yaxis = list(scaleanchor = "x", scaleratio = 1),
      #     hoverlabel = list(bgcolor = "white", font = list(color = "black"))
      #   )
    }, error = function(e) { # ----
      message("ggadd_supvars エラー: ", e$message)
      NULL
    })
  })

# 交互作用プロット（GDAtools::ggadd_interaction）　----
    output$interaction_map <- renderPlot({
    # 【重要】UIそのものではなく、UIの中の「値」を待つ
    req(mca_result(),
        input$inter_v1,
        input$inter_v2,
        input$selected_categories_v1, # ここを outputID から変更
        input$selected_categories_v2) # ここを outputID から変更

    res <- mca_result()
    df <- df_reactive()

    # --- データのフィルタリング処理 ---
    v1_f <- df[[input$inter_v1]]
    v1_f[!(v1_f %in% input$selected_categories_v1)] <- NA

    v2_f <- df[[input$inter_v2]]
    v2_f[!(v2_f %in% input$selected_categories_v2)] <- NA
    # ------------------------------

    tryCatch({
      base_map <- GDAtools::ggcloud_indiv(res, col = "lightgrey")
      (GDAtools::ggadd_interaction(p = base_map, resmca = res,
                                  v1 = v1_f,
                                  v2 = v2_f) +
        theme(aspect.ratio = 1)) #%>% ggplotly()
    }, error = function(e) {
      message("ggadd_interaction エラー: ", e$message)
      NULL
    })
  })#, height = 600)


# 集中楕円（GDAtools::ggadd_kellipses）----
  #  → 引数: base map(indiv), resmca, var=factor, sel=選択カテゴリの番号（整数ベクトル）

  output$kellipses_map_12 <- renderPlotly({
    req(mca_result(), input$var_ellipses, input$selected_categories)
    res <- mca_result()
    df <- df_reactive()
    axes_v <- c(1,2)

    # 因子化してレベル順を得る
    var_factor <- as.factor(df[[input$var_ellipses]])
    levels_var <- levels(var_factor)
    sel_index <- which(levels_var %in% input$selected_categories)

    tryCatch({
      base_map_ind <- GDAtools::ggcloud_indiv(res, axes = axes_v,col = "lightgrey")
      (GDAtools::ggadd_kellipses(p = base_map_ind, resmca = res,axes = axes_v,
                                var = var_factor, sel = sel_index) +
        coord_fixed(ratio = 1)) %>% ggplotly(tooltip = "all")
    }, error = function(e) { # ggadd_kellipses エラー----
      message("ggadd_kellipses エラー: ", e$message)
      NULL
    })
  })#, height = 600)


  output$kellipses_map_32 <- renderPlotly({
    req(mca_result(), input$var_ellipses, input$selected_categories)
    res <- mca_result()
    df <- df_reactive()
    axes_v <- c(3,2)

    # 因子化してレベル順を得る
    var_factor <- as.factor(df[[input$var_ellipses]])
    levels_var <- levels(var_factor)
    sel_index <- which(levels_var %in% input$selected_categories)

    tryCatch({
      base_map_ind <- GDAtools::ggcloud_indiv(res, axes = axes_v,col = "lightgrey")
      (GDAtools::ggadd_kellipses(p = base_map_ind, resmca = res,axes = axes_v,
                                 var = var_factor, sel = sel_index) +
          coord_fixed(ratio = 1)) %>% ggplotly(tooltip = "all")
    }, error = function(e) { # ggadd_kellipses エラー: ----
      message("ggadd_kellipses エラー: ", e$message)
      NULL
    })
  })#, height = 600)

  output$kellipses_map_13 <- renderPlotly({
    req(mca_result(), input$var_ellipses, input$selected_categories)
    res <- mca_result()
    df <- df_reactive()
    axes_v <- c(1,3)

    # 因子化してレベル順を得る
    var_factor <- as.factor(df[[input$var_ellipses]])
    levels_var <- levels(var_factor)
    sel_index <- which(levels_var %in% input$selected_categories)

    tryCatch({
      base_map_ind <- GDAtools::ggcloud_indiv(res, axes = axes_v,col = "lightgrey")
      (GDAtools::ggadd_kellipses(p = base_map_ind, resmca = res,axes = axes_v,
                                 var = var_factor, sel = sel_index) +
          coord_fixed(ratio = 1)) %>% ggplotly(tooltip = "all")
    }, error = function(e) { # ----
      message("ggadd_kellipses エラー: ", e$message)
      NULL
    })
  })#, height = 600)

#  ダウンロード（speMCA結果） ------------------------

    output$download_mca <- downloadHandler(
    filename = function() {
      paste0("speMCA_result_", Sys.Date(), ".rds")
    },
    content = function(file) { # ----
      res <- mca_result()
      if (is.null(res)) {
        showNotification("MCA結果がありません。", type = "error")
        return(NULL)
      }
      saveRDS(res, file)
    }
  )
}


# shinyApp(ui,server) -----

shinyApp(ui, server)
