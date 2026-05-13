# library(shiny)
# library(shinythemes)
# library(DT)

shinyUI(
#  tagList(shinythemes::themeSelector(),
  navbarPage("speMCA分析サポート",
      # theme = shinytheme("slate"),
       tabPanel("概要",
           h2("多重対応分析MCA分析サポート",paste("Version:", app_version)),
           p("LeRoux  & Rouanet \"Multiple Correspondence Analysis\"(大隅・小野・鳰薬『多重対応分析』で紹介されているspecficMCAをGDAtoolsをベースに分析をおこないます。"),
           p("データセットは、Rのオブジェクト、.Rdaで用意します。"),
           p("speMCAのリザルトはダウンロードして、別途分析することが可能です。"),

           helpText("うまく動かない可能性もあるのでご注意ください。"),
           # バージョン情報の表示
           # helpText(paste("Version:", app_version)),
           #      hr(), # 区切り線
           h3("Powered by"),
           img(src = "GDAtools.png", width = "100px"),#"8%"),#, height = "10%"),
           img(src = "shiny.webp", width = "100px"),#, height = "10%"),
           img(src = "ggplot2.png", width = "100px"),#, height = "10%"),
           img(src = "dplyr.png", width = "100px"),#, height = "10%"),

           ),

       tabPanel("File読み込み-MCA実行 ",
            p("分析対象のdf（*.rda）を読み込み、Active変数、junkカテゴリを指定しMCAを行います。"),
         sidebarLayout( ## sidebarLayout ----
            sidebarPanel(
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
              uiOutput("hist_var_ui")
            ),

            mainPanel( ## mainPanel ----
                tabsetPanel(
                   tabPanel("選択変数", uiOutput("selected_info")),
                   tabPanel("修正慣性率", tableOutput("eig_table"), plotOutput("eig_plot")),
                   tabPanel("speMCAのresult:リスト",
                            listviewer::jsoneditOutput("mca_result_tree", height = "600px")),
                   tabPanel("speMCAのresult:一覧", verbatimTextOutput("mca_result_list")),
                   tabPanel("データ表示", DTOutput("data_table")),
                   tabPanel("単変数集計",
                              h2("棒グラフと度数分布"),
                              plotOutput("barchart2"),
                              DT::dataTableOutput("simple_table2")
                     ),
                   tabPanel("使い方/関連資料",
                            p("アプリの詳細な使い方は、以下のリンク先をご確認ください。"),
                            a("Shinyアプリの使い方ガイド（外部サイト）",
                              href = "https://www.fujimotolabo.uk/Shiny_app_how2/",
                              target = "_blank") # 新しいタブで開く設定
                            ),
                   　　　　 a("対応分析/多重対応分析関連資料",
                   　　　　   href = "https://www.fujimotolabo.uk/CAMCA_archive/",
                   　　　　   target = "_blank")
                )
            ) # mainPanel
          )

       ),

       tabPanel("変数空間分析", sidebarLayout(
         sidebarPanel(
           uiOutput("supvar_selectors"),
           uiOutput("interaction_selectors"),
           uiOutput("ellipse_selectors")
         ),
         mainPanel(
           tabsetPanel(
             tabPanel("変数マップ",
                      plotOutput("var_map_12", height = "80vh"),
                      plotOutput("var_map_32", height = "80vh"),
                      plotOutput("var_map_13", height = "80vh")),
             tabPanel("η2マップ",
                      plotOutput("eta2_map_12",height = "80vh"),# height="1200px"),
                      plotOutput("eta2_map_32",height = "80vh"),
                      plotOutput("eta2_map_13",height = "80vh")),
             tabPanel("交互作用plot",
                      plotOutput("interaction_map_12", height = "80vh"),
                      plotOutput("interaction_map_32", height = "80vh"),
                      plotOutput("interaction_map_13", height = "80vh")
                      ), # plotlyだと文字がでない
           )
         )
       )),

       tabPanel("個体空間分析", sidebarLayout(
         sidebarPanel(
           uiOutput("ellipse_selectors")
         ),
         mainPanel(
           tabsetPanel(
             tabPanel("個体マップ",
                      plotlyOutput("ind_map_12", height = "80vh"),
                      plotlyOutput("ind_map_32", height = "80vh"),
                      plotlyOutput("ind_map_13", height = "80vh")),
             tabPanel("集中楕円",
                      plotlyOutput("kellipses_map_12", height = "80vh"),
                      plotlyOutput("kellipses_map_32", height = "80vh"),
                      plotlyOutput("kellipses_map_13", height = "80vh")
             ),
           )
         )
       )),


       navbarMenu("関連リンク集",
                  tabPanel("使い方",
                           p("アプリの詳細な使い方は、以下のリンク先をご確認ください。"),
                           a("Shinyアプリの使い方ガイド（外部サイト）",
                             href = "https://www.fujimotolabo.uk/Shiny_app_how2/",
                             target = "_blank") # 新しいタブで開く設定
                  ),

                  tabPanel("MCA関連資料",
#                           p("MCA関連資料"),
                           a("MCA関連資料",href="https://www.fujimotolabo.uk/CAMCA_archive/",
                             target = "_blank")
                  ),

                  tabPanel("ソースコード/GitHUb",
                           a(href="https://github.com/419kfj/Shiny_speMCA_lite",
                             p("https://github.com/419kfj/Shiny_speMCA_lite"))
                  )
       )
   )

)
