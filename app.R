# app.R -- speMCA フルアプリ（.rda アップロード対応、GDAtools 関数版）
# 2025/10/05 19:59version
library(shiny)
library(GDAtools)
library(tidyverse)
library(DT)
library(showtext)
# 追加ライブラリ
library(shinyWidgets)


showtext::showtext_auto(TRUE)

ui <- fluidPage(
  #  shinyWidgets::useShinyWidgets(),
  titlePanel("speMCA 分析アプリ"),
  sidebarLayout(
    sidebarPanel(
      fileInput("file", "rdaファイルをアップロードしてください", accept = ".rda"),
      actionButton("load_rda", "rdaファイルを読み込む", class = "btn-primary"),
      hr(),

      # 動的に生成する UI（df が読み込まれた後に表示）
      uiOutput("variable_selectors"),
      uiOutput("junk_selector"),
      uiOutput("supvar_selectors"),
      uiOutput("interaction_selectors"),
      uiOutput("ellipse_selectors"),

      hr(),
      downloadButton("download_mca", "speMCA結果をダウンロード")
    ),

    mainPanel(
      tabsetPanel(
        tabPanel("選択変数", uiOutput("selected_info")),
        tabPanel("修正慣性率", tableOutput("eig_table"), plotOutput("eig_plot")),
        tabPanel("変数マップ",
                 plotOutput("var_map_12", height = "600px"),
                 plotOutput("var_map_32", height = "600px"),
                 plotOutput("var_map_13", height = "600px")),
        tabPanel("個体マップ",
                 plotOutput("ind_map_12", height = "600px"),
                 plotOutput("ind_map_32", height = "600px"),
                 plotOutput("ind_map_13", height = "600px")),
        tabPanel("データ表示", DTOutput("data_table")),
        tabPanel("supvarsの情報", verbatimTextOutput("supvars_out")),
        tabPanel("変数マップ＋supvars", plotOutput("supvars_map")),
        tabPanel("交互作用plot", plotOutput("interaction_map")),
        tabPanel("集中楕円", plotOutput("kellipses_map"))
      )
    )
  )
)

server <- function(input, output, session) {

  # ------------------------
  #  .rda 読み込み：load ボタンで eventReactive
  # ------------------------
  dataset <- eventReactive(input$load_rda, {
    req(input$file)
    e <- new.env()
    obj_names <- load(input$file$datapath, envir = e)
    objs <- mget(obj_names, envir = e)
    message("ロードされたオブジェクト: ", paste(names(objs), collapse = ", "))
    objs
  })

  # df として使う（.rda の最初のオブジェクト）
  df_reactive <- reactive({
    req(dataset())
    dataset()[[1]]
  })

  # ------------------------
  #  動的 UI（df があるとき）
  # ------------------------
  output$variable_selectors <- renderUI({
    req(df_reactive())
    choices <- names(df_reactive())
    selectInput("variables", "Active変数を選んでください",
                choices = choices, multiple = TRUE, selectize = FALSE, size = 7)
  })

  output$supvar_selectors <- renderUI({
    req(df_reactive())
    choices <- names(df_reactive())
    selectInput("supvars", "追加変数を選んでください",
                choices = choices, multiple = TRUE, selectize = FALSE, size = 7)
  })

  output$interaction_selectors <- renderUI({
    req(df_reactive())
    choices <- names(df_reactive())
    tagList(
      selectInput("inter_v1", "交互作用 v1 を選んでください", choices = choices, selected = choices[1]),
      selectInput("inter_v2", "交互作用 v2 を選んでください", choices = choices, selected = choices[min(2,length(choices))])
    )
  })

  output$ellipse_selectors <- renderUI({
    req(df_reactive())
    choices <- names(df_reactive())
    tagList(
      selectInput("var_ellipses", "集中楕円表示変数を選んでください",
                  choices = choices, multiple = FALSE),
      uiOutput("kellipses_cat_selector")  # 選んだ変数に応じたカテゴリチェックボックスを生成
    )
  })

  # ------------------------
  # 全選択／全解除ボタンの動作
  # ------------------------
  observeEvent(input$select_all, {
    req(input$var_ellipses)
    df <- df_reactive()
    lv <- levels(as.factor(df[[input$var_ellipses]]))
    updateCheckboxGroupInput(session, "selected_categories", selected = lv)
  })

  observeEvent(input$deselect_all, {
    updateCheckboxGroupInput(session, "selected_categories", selected = character(0))
  })



  # ------------------------
  #  junkカテゴリ（getindexcat を使う）
  #  ※ input$variables が2個以上必要（getindexcat の前提）
  # ------------------------
  junk_cat <- reactive({
    req(df_reactive())
    if (is.null(input$variables) || length(input$variables) < 2) return(NULL)
    df <- df_reactive()
    df_sub <- df[, input$variables, drop = FALSE]
    jc <- getindexcat(df_sub)
    if (is.null(jc) || length(jc) == 0) return(NULL)
    jc
  })

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

  # ------------------------
  #  speMCA 実行（ボタン押下で eventReactive）
  # ------------------------
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
    }, error = function(e) {
      showNotification(paste0("speMCA エラー: ", e$message), type = "error")
      NULL
    })
  })

  # ------------------------
  #  supvars の計算（mca_result と input$supvars に依存）
  # ------------------------
  supvars_result <- reactive({
    req(mca_result())
    if (is.null(input$supvars) || length(input$supvars) == 0) return(NULL)
    df <- df_reactive()
    tryCatch({
      GDAtools::supvars(resmca = mca_result(), vars = df %>% select(all_of(input$supvars)))
    }, error = function(e) {
      message("supvars エラー: ", e$message)
      NULL
    })
  })

  # ------------------------
  #  kelleses 用カテゴリチェックボックス（var_ellipses を選んだら生成）
  # ------------------------
  # output$kellipses_cat_selector <- renderUI({　#　最初のズラーットチェックボックスが並ぶやつ。
  #   req(df_reactive())
  #   req(input$var_ellipses)
  #   df <- df_reactive()
  #   var <- df[[input$var_ellipses]]
  #   lv <- levels(as.factor(var))
  #   checkboxGroupInput("selected_categories",
  #                      "表示するカテゴリを選んでください",
  #                      choices = lv,
  #                      selected = lv)
  # })

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


  # ------------------------
  #  選択変数タブ（常に input に基づいて表示）
  # ------------------------
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

  # ------------------------
  #  結果表示：データ表 / 修正慣性率
  # ------------------------
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

  # ------------------------
  #  変数マップ（3枚）
  # ------------------------
  output$var_map_12 <- renderPlot({
    res <- mca_result(); req(res)
    GDAtools::ggcloud_variables(res) + theme(aspect.ratio = 1) + ggtitle("変数マップ 1-2軸")
  })
  output$var_map_32 <- renderPlot({
    res <- mca_result(); req(res)
    GDAtools::ggcloud_variables(res, axes = c(3,2)) + theme(aspect.ratio = 1) + ggtitle("変数マップ 3-2軸")
  })
  output$var_map_13 <- renderPlot({
    res <- mca_result(); req(res)
    GDAtools::ggcloud_variables(res, axes = c(1,3)) + theme(aspect.ratio = 1) + ggtitle("変数マップ 1-3軸")
  })

  # ------------------------
  #  個体マップ（3枚）
  # ------------------------
  output$ind_map_12 <- renderPlot({
    res <- mca_result(); req(res)
    GDAtools::ggcloud_indiv(res) + theme(aspect.ratio = 1) + ggtitle("個体マップ 1-2軸")
  })
  output$ind_map_32 <- renderPlot({
    res <- mca_result(); req(res)
    GDAtools::ggcloud_indiv(res, axes = c(3,2)) + theme(aspect.ratio = 1) + ggtitle("個体マップ 3-2軸")
  })
  output$ind_map_13 <- renderPlot({
    res <- mca_result(); req(res)
    GDAtools::ggcloud_indiv(res, axes = c(1,3)) + theme(aspect.ratio = 1) + ggtitle("個体マップ 1-3軸")
  })

  # ------------------------
  #  supvars の出力とマップ（GDAtools::ggadd_supvars）
  # ------------------------
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
    }, error = function(e) {
      message("ggadd_supvars エラー: ", e$message)
      NULL
    })
  })

  # ------------------------
  #  交互作用（GDAtools::ggadd_interaction）
  # ------------------------
  output$interaction_map <- renderPlot({
    req(mca_result(), input$inter_v1, input$inter_v2)
    res <- mca_result()
    df <- df_reactive()
    tryCatch({
      base_map <- GDAtools::ggcloud_variables(res, col = "lightgrey")
      GDAtools::ggadd_interaction(p = base_map, resmca = res,
                                  v1 = df[[input$inter_v1]],
                                  v2 = df[[input$inter_v2]]) +
        theme(aspect.ratio = 1)
    }, error = function(e) {
      message("ggadd_interaction エラー: ", e$message)
      NULL
    })
  }, height = 600)

  # ------------------------
  #  集中楕円（GDAtools::ggadd_kellipses）
  #  → 引数: base map(indiv), resmca, var=factor, sel=選択カテゴリの番号（整数ベクトル）
  # ------------------------
  output$kellipses_map <- renderPlot({
    req(mca_result(), input$var_ellipses, input$selected_categories)
    res <- mca_result()
    df <- df_reactive()

    # 因子化してレベル順を得る
    var_factor <- as.factor(df[[input$var_ellipses]])
    levels_var <- levels(var_factor)
    sel_index <- which(levels_var %in% input$selected_categories)

    tryCatch({
      base_map_ind <- GDAtools::ggcloud_indiv(res, col = "lightgrey")
      GDAtools::ggadd_kellipses(p = base_map_ind, resmca = res,
                                var = var_factor, sel = sel_index) +
        coord_fixed(ratio = 1)
    }, error = function(e) {
      message("ggadd_kellipses エラー: ", e$message)
      NULL
    })
  }, height = 600)

  # ------------------------
  #  ダウンロード（speMCA結果）
  # ------------------------
  output$download_mca <- downloadHandler(
    filename = function() {
      paste0("speMCA_result_", Sys.Date(), ".rds")
    },
    content = function(file) {
      res <- mca_result()
      if (is.null(res)) {
        showNotification("MCA結果がありません。", type = "error")
        return(NULL)
      }
      saveRDS(res, file)
    }
  )
}

shinyApp(ui, server)
