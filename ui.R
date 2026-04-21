fluidPage(
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
