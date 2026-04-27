function(input, output, session) {

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
  output$interaction_map_12 <- renderPlot({
    # 【重要】UIそのものではなく、UIの中の「値」を待つ
    req(mca_result(),
        input$inter_v1,
        input$inter_v2,
        input$selected_categories_v1, # ここを outputID から変更
        input$selected_categories_v2) # ここを outputID から変更

    res <- mca_result()
    df <- df_reactive()
    axes <- c(1,2)

    # --- データのフィルタリング処理 ---
    v1_f <- df[[input$inter_v1]]
    v1_f[!(v1_f %in% input$selected_categories_v1)] <- NA

    v2_f <- df[[input$inter_v2]]
    v2_f[!(v2_f %in% input$selected_categories_v2)] <- NA
    # ------------------------------

    tryCatch({
      base_map <- GDAtools::ggcloud_variables(res, col = "lightgrey",axes = axes)
      (GDAtools::ggadd_interaction(p = base_map, resmca = res,
                                   v1 = v1_f,
                                   v2 = v2_f,axes = axes) +
          theme(aspect.ratio = 1)) #%>% ggplotly()
    }, error = function(e) {
      message("ggadd_interaction エラー: ", e$message)
      NULL
    })
  })#, height = 600)

  output$interaction_map_32 <- renderPlot({
    # 【重要】UIそのものではなく、UIの中の「値」を待つ
    req(mca_result(),
        input$inter_v1,
        input$inter_v2,
        input$selected_categories_v1, # ここを outputID から変更
        input$selected_categories_v2) # ここを outputID から変更

    res <- mca_result()
    df <- df_reactive()
    axes <- c(3,2)

    # --- データのフィルタリング処理 ---
    v1_f <- df[[input$inter_v1]]
    v1_f[!(v1_f %in% input$selected_categories_v1)] <- NA

    v2_f <- df[[input$inter_v2]]
    v2_f[!(v2_f %in% input$selected_categories_v2)] <- NA
    # ------------------------------

    tryCatch({
      base_map <- GDAtools::ggcloud_variables(res, col = "lightgrey",axes = axes)
      (GDAtools::ggadd_interaction(p = base_map, resmca = res,
                                   v1 = v1_f,
                                   v2 = v2_f,axes = axes) +
          theme(aspect.ratio = 1)) #%>% ggplotly()
    }, error = function(e) {
      message("ggadd_interaction エラー: ", e$message)
      NULL
    })
  })#, height = 600)

  output$interaction_map_13 <- renderPlot({
    # 【重要】UIそのものではなく、UIの中の「値」を待つ
    req(mca_result(),
        input$inter_v1,
        input$inter_v2,
        input$selected_categories_v1, # ここを outputID から変更
        input$selected_categories_v2) # ここを outputID から変更

    res <- mca_result()
    df <- df_reactive()
    axes <- c(1,3)

    # --- データのフィルタリング処理 ---
    v1_f <- df[[input$inter_v1]]
    v1_f[!(v1_f %in% input$selected_categories_v1)] <- NA

    v2_f <- df[[input$inter_v2]]
    v2_f[!(v2_f %in% input$selected_categories_v2)] <- NA
    # ------------------------------

    tryCatch({
      base_map <- GDAtools::ggcloud_variables(res, col = "lightgrey",axes = axes)
      (GDAtools::ggadd_interaction(p = base_map, resmca = res,
                                   v1 = v1_f,
                                   v2 = v2_f,axes = axes) +
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


  # -----　ここからENQview function 2026/04/26

  # 選択されたオブジェクトを reactiveVal に格納
  data.df <- reactiveVal(NULL)
  observeEvent(input$rda_object, {
    req(input$rda_object)
    obj <- get(input$rda_object, envir = rda_env())
    if (is.data.frame(obj)) {
      data.df(obj)  # ここで reactiveVal に保存
    } else {
      showNotification("選択したオブジェクトはデータフレームではありません。", type = "error")
      data.df(NULL)
    }
  })

  # ←★ ここを追加：data.df() が更新されたらUIを更新
  observeEvent(data.df(), {
    req(data.df())
    data <- data.df()
    updateSelectInput(session, "select_input_data_for_hist",
                      choices = colnames(data))
  })

  data_for_plot <- reactive({
    req(df_reactive())
#    data.df()
    df_reactive()
  })
  output$hist_var_ui <- renderUI({
    req(df_reactive())
    selectInput("select_input_data_for_hist", "確認したい単変数",
                choices = names(df_reactive()))

    # req(data.df())
    # selectInput("select_input_data_for_hist", "確認したい単変数",
    #             choices = names(data.df()))
  })

  output$barchart2 <- renderPlot({
    req(df_reactive())
    data_for_plot <- df_reactive()
    data_for_plot() %>% count(!!!rlang::syms(input$select_input_data_for_hist)) %>% dplyr::rename(V1=1) %>% filter(V1 != "非該当") %>%
      dplyr::mutate(rate=100* .data[["n"]]/ sum(.data[["n"]])) %>%
      ggplot2::ggplot(aes(x=V1,y=rate)) + ggplot2::geom_col(aes(fill=V1)) + ggplot2::ggtitle(input$select_input_data_for_hist)
  })

  output$simple_table2 <- DT::renderDataTable({ #renderTable({#
    req(df_reactive())
    data_for_plot <- df_reactive()
    table(data_for_plot()[[input$select_input_data_for_hist]]) -> tmp
    round(100*prop.table(tmp),1) -> tmp2
    data.frame(tmp,rate=tmp2)[,c(1,2,4)] #%>% DT::datatable() -> tmp3
    # tmp3 %>%
    #   formatStyle(
    #     columns = colnames(tmp3),
    #     target = 'row',
    #     backgroundColor = 'white',
    #     color = 'black'
    #   )
  })

# ----ここまで

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
