
# Close the window and shut down app----
jscode <- "shinyjs.closeWindow = function() { window.close(); }"

choice.palette <- list("ColorBrewer.seq"=c("Blues","BuGn","BuPu","GnBu","Greens",
                         "Greys","Oranges","OrRd","PuBu","PuBuGn",
                         "PuRd","Purples", "RdPu", "Reds","YlGn",
                         "YlGnBu","YlOrBr","YlOrRd"),
     "colorBrewer.div"=c("BrBG","PiYG","PRGn",
                         "PuOr","RdBu","RdGy",
                         "RdYlBu","RdYlGn","Spectral"),
     "viridis"=c("magma","inferno","plasma",
                 "viridis","cividis"))

# Shiny UI----

# Shiny header----
header <- dashboardHeader(
  title = span(
    "ScRNA-seq Data Portal for T cell in Pan-Cancer"
    # span("s", style = "color: #F3746C; font-size: 24px; font-weight: bold"),
    # span("ingle "),
    # span("c", style = "color: #F3746C; font-size: 24px; font-weight: bold"),
    # span("ell RNA-seq "),
    # span("D", style = "color: #F3746C; font-size: 24px; font-weight: bold"),
    # span("ata "),
    # span("V", style = "color: #F3746C; font-size: 24px; font-weight: bold"),
    # span("isualization and "),
    # span("A", style = "color: #F3746C; font-size: 24px; font-weight: bold"),
    # span("nalysis")
  ),
  titleWidth = 550,
  tags$li(
    class = "dropdown",
    extendShinyjs(text = jscode, functions = c("closeWindow")),
    shiny::actionButton(
      inputId = "close",
      label = "Shut down",
      width = "120px",
      icon = icon("window-close"),
      style = "color: #fff; background-color: #337ab7; border-color: #2e6da4"
    )
  )
)

# Shiny sidebar----
sidebar <- dashboardSidebar(
  tags$head(
    includeScript("./www/google-analytics.js"),
    tags$style(
      type = 'text/css',
      ".nav-tabs {font-size: 14px} ",
      ".main-sidebar {font-size: 16px}"
    )
  ),
  sidebarMenu(
    menuItem("Embedding", tabName = "Embedding", icon = icon("spinner")),
    menuItem(
      "Distribution",
      tabName = "Distribution",
      icon = icon("bar-chart-o")
    ),
    menuItem(
      "SignatureGene",
      tabName = "SignatureGene",
      icon = icon("braille")
    ),
    menuItem("Heatmap", tabName = "Heatmap", icon = icon("delicious")),
    # menuItem(
    #   "In-silico FACS",
    #   tabName = "FACS",
    #   icon = icon("fas fa-fingerprint")
    # ),
    # menuItem(
    #   "Metadata",
    #   tabName = "Metadata",
    #   icon = icon("stats", lib = "glyphicon")
    # ),
    #menuItem("DataTable", tabName = "DataTable", icon = icon("table")),
    menuItem("Instructions", tabName = "Instructions", icon = icon("file-alt")),
    menuItem("About", tabName = "About", icon = icon("envelope"))
  )
)

# Shiny body----
body <- dashboardBody(
  useShinyjs(),
  # *Login page----
  shinyauthr::loginUI("login"),
  # *Working page----
  conditionalPanel(condition = "output.Login_logged",
                   fluidRow(
                     column(
                       width = 9,
                       # **Head panel----
                       box(
                         title = "Gene input",
                         width = 12,
                         status = "warning",
                         collapsible = T,
                         collapsed = F,
                         tabBox(
                           id = "GeneInput_panel",
                           side = "left",
                           selected = "Genes",
                           width = 12,
                           tabPanel(
                             "Genes",
                             # textAreaInput(
                             #   inputId = "GeneInput_text",
                             #   label = "Type a gene or geneset:",
                             #   value = "CXCL13"
                             # )
                             selectInput("GeneInput_text",label = NULL,choices = NULL,multiple = T)
                           ),
                           tabPanel(
                             "Saved",
                             textAreaInput(
                               inputId = "GeneInput_saved_text",
                               label = NULL,
                               value = NULL
                             ),
                             selectizeInput(
                               inputId = "GeneInput_saved",
                               label = "Select from the saved geneset:",
                               multiple = TRUE,
                               choice = NULL,
                               selected = NULL
                             )
                           ),
                           tabPanel(
                             "Upload",
                             textAreaInput(
                               inputId = "GeneInput_file_text",
                               label = NULL,
                               value = NULL
                             ),
                             fileInput(
                               inputId = "GeneInput_file",
                               label = "Choose a csv file:",
                               multiple = FALSE
                             ) %>%
                               shinyInput_label_embed(
                                 shiny_iconlink() %>%
                                   bs_embed_popover(
                                     title = "Note:",
                                     placement = "left",
                                     content = "There should be a column named as 'Symbol'."
                                   )
                               )
                           )
                         ),
                         shiny::actionButton(
                           inputId = "GeneInput_submit",
                           label = "Submit",
                           width = "120px",
                           icon = icon("paper-plane"),
                           style = "color: #fff; background-color: #337ab7; border-color: #2e6da4"
                         )
                       ),
                       # **Plot region----
                       box(
                         status = "warning",
                         width = 12,
                         tabItems(
                           # ****Embedding Plot----
                           tabItem(
                             tabName = "Embedding",
                             box(
                               status = NULL,
                               width = 12,
                               dropdownButton(
                                 selectInput(
                                   inputId = "Embedding_gene_splitBy",
                                   label = "Split by",
                                   choices = c("None"),
                                   selected = "None"
                                 ),
                                 selectInput(
                                   inputId = "Embedding_used1",
                                   label = "Embedding used",
                                   choices = NULL,
                                   selected = NULL
                                 ),
                                 selectInput(
                                   inputId = "Embedding_GenePar",
                                   label = "Multi gene",
                                   ##choices = c("Seperate" = TRUE, "Mean" = FALSE),
                                   choices = c("Seperate", "Mean"),
                                   selected = "Seperate"
                                 ),
                                 numericInput(
                                   inputId = "PlotSize_width",
                                   label = "Plot width (px)",
                                   value = 600,
                                   step = 10
                                 ),
                                 numericInput(
                                   inputId = "PlotSize_height",
                                   label = "Plot height (px)",
                                   value = 360,
                                   step = 10
                                 ),
                                 numericInput(
                                   inputId = "PlotPar_ncol",
                                   label = "Col number",
                                   value = 1
                                 ),
                                 sliderInput(
                                   inputId = "Embedding_dotsize1",
                                   label = "Dot size",
                                   value = 2,
                                   min = 0,
                                   max = 3,
                                   step = 0.2
                                 ),
                                 selectInput(
                                   inputId = "Embedding_colorpanel",
                                   label = "Color profile",
                                   choices = choice.palette,
                                   selected = "YlOrRd"
                                 ),
                                 circle = TRUE,
                                 status = "danger",
                                 icon = icon("spinner"),
                                 width = "250px",
                                 tooltip = tooltipOptions(title = "Click to see inputs!")
                               ),
                               uiOutput("Embedding_geneplot.ui"),
                               downloadButton(
                                 outputId = 'Embedding_gene_download',
                                 label = 'Download',
                                 width = "120px",
                                 style = "color: #fff; background-color: #337ab7; border-color: #2e6da4"
                               )
                             ),
                             box(
                               status = NULL,
                               width = 12,
                               dropdownButton(
                                 selectInput(
                                   inputId = "Embedding_colorby",
                                   label = "Color by",
                                   choices = NULL,
                                   selected = NULL,
                                   multiple = T
                                 ),
                                 selectInput(
                                   inputId = "Embedding_metaInfo_splitBy",
                                   label = "Split by",
                                   choices = c("None"),
                                   selected = "None"
                                 ),
                                 numericInput(
                                   inputId = "PlotSize_width2",
                                   label = "Plot width (px)",
                                   value = 650,
                                   step = 10
                                 ),
                                 numericInput(
                                   inputId = "PlotSize_height2",
                                   label = "Plot height (px)",
                                   value = 360,
                                   step = 10
                                 ),
                                 numericInput(
                                   inputId = "PlotPar_ncol2",
                                   label = "Col number",
                                   value = 1
                                 ),
                                 sliderInput(
                                   inputId = "Embedding_dotsize2",
                                   label = "Dot size",
                                   value = 1.4,
                                   min = 0,
                                   max = 3,
                                   step = 0.2
                                 ),
                                 sliderInput(
                                   inputId = "Embedding_metaInfo_labelSize",
                                   label = "Label size",
                                   value = 3.5,
                                   min = 0,
                                   max = 10,
                                   step = 0.5
                                 ),
                                 circle = TRUE,
                                 status = "danger",
                                 icon = icon("spinner"),
                                 width = "250px",
                                 tooltip = tooltipOptions(title = "Click to see inputs!")
                               ),
                               uiOutput("Embedding_metaplot.ui"),
                               downloadButton(
                                 outputId = 'Embedding_metaInfo_download',
                                 label = 'Download',
                                 width = "120px",
                                 style = "color: #fff; background-color: #337ab7; border-color: #2e6da4"
                               )
                             )
                             #verbatimTextOutput("EnvirementAA"),
                             #verbatimTextOutput("EnvirementBB"),
                             #verbatimTextOutput("EnvirementCC")
                           ),
                           # ****Distribution Plot----
                           tabItem(
                             tabName = "Distribution",
                             dropdownButton(
                               selectInput(
                                 inputId = "Distribution_groupby",
                                 label = "Group by",
                                 choices = NULL,
                                 selected = NULL
                               ),
                               selectInput(
                                 inputId = "Distribution_splitBy",
                                 label = "Split by",
                                 choices = c("None"),
                                 selected = "None"
                               ),
                               selectInput(
                                 inputId = "Distribution_GenePar",
                                 label = "Multi gene",
                                 ##choices = c("Seperate" = TRUE, "Mean" = FALSE),
                                 choices = c("Seperate", "Mean"),
                                 selected = "Seperate"
                               ),
                               # pickerInput(
                               #   inputId = "Distribution_groupIn",
                               #   label = "Only show groups in",
                               #   choices = NULL,
                               #   multiple = TRUE,
                               #   options = list(
                               #     `actions-box` = TRUE,
                               #     `live-search` = TRUE
                               #   )
                               # ),
                               numericInput(
                                 inputId = "Distribution_PlotWidth",
                                 label = "Plot width (px)",
                                 value = 650,
                                 step = 10
                               ),
                               numericInput(
                                 inputId = "Distribution_PlotHeight",
                                 label = "Plot height (px)",
                                 value = 300,
                                 step = 10
                               ),
                               numericInput(
                                 inputId = "Distribution_PlotNCol",
                                 label = "Col number",
                                 value = 1
                               ),
                               selectInput(
                                 inputId = "Distribution_colorpanel",
                                 label = "Color profile",
                                 choices = choice.palette,
                                 selected = "YlOrRd"
                               ),
                               # sliderInput(
                               #   inputId = "Distribution_dotsize",
                               #   label = "Dot size",
                               #   value = 0.6,
                               #   min = 0,
                               #   max = 1,
                               #   step = 0.1
                               # ),
                               circle = TRUE,
                               status = "danger",
                               icon = icon("bar-chart-o"),
                               width = "250px",
                               tooltip = tooltipOptions(title = "Click to see inputs!")
                             ),
                             uiOutput("Distribution_plot.ui"),
                             downloadButton(
                               outputId = 'Distribution_download',
                               label = 'Download',
                               width = "120px",
                               style = "color: #fff; background-color: #337ab7; border-color: #2e6da4"
                             )
                           ),
                           # ****Significance Plot----
                           tabItem(
                             tabName = "SignatureGene",
                             h3("Full talbe of the signature gene detection"),
                             DT::dataTableOutput("Significance_group"),
                             tags$br(),
                             # h4("Significance"),
                             # verbatimTextOutput("Significance_statistic"),
                             h3("Selected genes and meta.cluster visulized here:"),
                             tags$br(),
                             dropdownButton(
                               selectInput(
                                 inputId = "SignatureGene_meta.cluster",
                                 label = "Meta-Cluster",
                                 choices = NULL,
                                 multiple = F,
                                 selected = NULL
                               ),
                               selectInput(
                                 inputId = "SignatureGene_groupBy",
                                 label = "Group by",
                                 #choices = c("cancerType","study"),
                                 choices = c("cancerType"),
                                 multiple = F,
                                 selected = "cancerType"
                               ),
                               numericInput(
                                 inputId = "SignatureGene_PlotWidth",
                                 label = "Plot width (px)",
                                 value = 350,
                                 step = 10
                               ),
                               numericInput(
                                 inputId = "SignatureGene_PlotHeight",
                                 label = "Plot height (px)",
                                 value = 500,
                                 step = 10
                               ),
                               numericInput(
                                 inputId = "SignatureGene_PlotNCol",
                                 label = "Col number",
                                 value = 1
                               ),
                               # selectInput(
                               #   inputId = "Significance_groupby",
                               #   label = "Group by",
                               #   choices = c(
                               #     "Global_Cluster",
                               #     "Sub_Cluster",
                               #     "Tissue",
                               #     "Treatment",
                               #     "Day",
                               #     "Sample"
                               #   ),
                               #   selected = "Sub_Cluster"
                               # ),
                               # numericInput(
                               #   inputId = "Significance_percutoff",
                               #   label = "Expression cutoff",
                               #   value = 0
                               # ),
                               # numericInput(
                               #   inputId = "Significance_siglevel",
                               #   label = "Significant level",
                               #   value = 0.05
                               # ),
                               circle = TRUE,
                               status = "danger",
                               icon = icon("braille"),
                               width = "250px",
                               tooltip = tooltipOptions(title = "Click to see inputs!")
                             ),
                             uiOutput("Significance_plot.ui"),
                             downloadButton(
                               outputId = 'Significance_download',
                               label = 'Download',
                               width = "120px",
                               style = "color: #fff; background-color: #337ab7; border-color: #2e6da4"
                             )
                           ),
                           # ****Heatmap Plot----
                           tabItem(
                             tabName = "Heatmap",
                             dropdownButton(
                               selectInput(
                                 inputId = "Heatmap_aveBy",
                                 label = "Average by",
                                 choices = c("meta.cluster","cancerType","dataset"),
                                 selected = "meta.cluster"
                               ),
                               # checkboxGroupInput(inputId = "Heatmap_colorBy",
                               #                    label = "Color by",
                               #                    choices=NULL,
                               #                    selected = NULL,
                               #                    inline = FALSE,
                               #                    width = NULL),
                               # h5("Average the genes"),
                               # checkboxInput(inputId="Heatmap_aveby",
                               #               label="by study",
                               #               value = T, width = NULL),
                               # selectInput(
                               #   inputId = "Heatmap_splitBy",
                               #   label = "Split by",
                               #   choices = c("None"),
                               #   selected = "None"
                               # ),
                               h5("Clustering",style="font-weight: bold"),
                               checkboxInput(inputId="Heatmap_clustering_row",
                                             label="by row",
                                             value = T, width = NULL),
                               checkboxInput(inputId="Heatmap_clustering_col",
                                             label="by column",
                                             value = T, width = NULL),
                               # checkboxInput(inputId="Heatmap_dendro_row",
                               #               label="Dendrogram rows",
                               #               value = T, width = NULL),
                               # checkboxInput(inputId="Heatmap_dendro_col",
                               #               label="Dendrogram columns",
                               #               value = T, width = NULL),
                               numericInput(
                                 inputId = "Heatmap_PlotWidth",
                                 label = "Plot width (px)",
                                 value = 1000,
                                 step = 10
                               ),
                               numericInput(
                                 inputId = "Heatmap_PlotHeight",
                                 label = "Plot height (px)",
                                 value = 300,
                                 step = 10
                               ),
                               # numericInput(
                               #   inputId = "Heatmap_PlotNCol",
                               #   label = "Col number",
                               #   value = 1
                               # ),
                               sliderInput(
                                 inputId = "Heatmap_zMax",
                                 label = "Exp limits",
                                 value = 1.5,
                                 min = 0.5,
                                 max = 2,
                                 step = 0.1
                               ),
                               selectInput(
                                 inputId = "Heatmap_colorpanel",
                                 label = "Color profile",
                                 choices = choice.palette,
                                 selected = "RdBu"
                               ),
                               circle = TRUE,
                               status = "danger",
                               icon = icon("delicious"),
                               width = "250px",
                               tooltip = tooltipOptions(title = "Click to see inputs!")
                             ),
                             uiOutput("Heatmap_plot.ui"),
                             downloadButton(
                               outputId = 'Heatmap_download',
                               label = 'Download',
                               width = "120px",
                               style = "color: #fff; background-color: #337ab7; border-color: #2e6da4"
                             )
                           ),
                           # ****In-silico FACS Plot----
                           tabItem(
                             tabName = "FACS",
                             tabBox(
                               id = "FACS",
                               side = "left",
                               selected = "FACS",
                               width = 12,
                               tabPanel("FACS",
                                        dropdownButton(
                                          numericInput(
                                            inputId = "FACS_xcutoff",
                                            label = "x cutoff",
                                            value = 1
                                          ),
                                          numericInput(
                                            inputId = "FACS_ycutoff",
                                            label = "y cutoff",
                                            value = 1
                                          ),
                                          selectInput(
                                            inputId = "FACS_colorpanel",
                                            label = "Color profile",
                                            choices = c(
                                              "YlOrRd",
                                              "BrBG",
                                              "RdYlBu",
                                              "RdYlGn",
                                              "Spectral",
                                              "YlOrBr",
                                              "YlGnBu",
                                              "Reds",
                                              "Blues",
                                              "OrRd",
                                              "RdBu"
                                            ),
                                            selected = "RdBu"
                                          ),
                                          selectInput(
                                            inputId = "FACS_colorby",
                                            label = "Color by",
                                            choices = c(
                                              "None",
                                              "Global_Cluster",
                                              "Sub_Cluster",
                                              "Tissue",
                                              "Treatment",
                                              "Day",
                                              "Sample"
                                            ),
                                            selected = "None"
                                          ),
                                          sliderTextInput(
                                            inputId = "FACS_dotsize",
                                            label = "Dot size",
                                            choices = c(-1, seq(0, 3, 0.5)),
                                            grid = TRUE,
                                            selected = -1
                                          ),
                                          circle = TRUE,
                                          status = "danger",
                                          icon = icon("fas fa-fingerprint"),
                                          width = "250px",
                                          tooltip = tooltipOptions(title = "Click to see inputs!")
                                        ),
                                        uiOutput("FACS_plot.ui"),
                                        downloadButton(
                                          outputId = 'FACS_download',
                                          label = 'Download',
                                          width = "120px",
                                          style = "color: #fff; background-color: #337ab7; border-color: #2e6da4")
                               ),
                               tabPanel("DEG",
                                        box(width = 12,
                                            column(width = 4,
                                                   selectInput(
                                                     inputId = "FACS_DEG_group1",
                                                     label = "The 1st group",
                                                     choices = c("Group1", "Group2", "Group3", "Group4"),
                                                     selected = "Group1"
                                                   ),
                                                   selectInput(
                                                     inputId = "FACS_DEG_group2",
                                                     label = "The 2nd group",
                                                     choices = c("Group1", "Group2", "Group3", "Group4"),
                                                     selected = "Group2"
                                                   ),
                                                   p(
                                                     class = "text-muted",
                                                     paste(
                                                       "Note: The number of cells in each group will be downsampled to 1000 randomly."
                                                     )
                                                   )
                                            ),
                                            column(width = 4,
                                                   numericInput(
                                                     inputId = "FACS_Volcano_Pvalue_cutoff",
                                                     label = "adj.P.Val cutoff",
                                                     value = 0.05
                                                   ),
                                                   numericInput(
                                                     inputId = "FACS_Volcano_logFC_cutoff",
                                                     label = "logFC cutoff",
                                                     value = 0.5
                                                   ),
                                                   numericInput(
                                                     inputId = "FACS_Volcano_ngenes_labeled",
                                                     label = "Labeled genes",
                                                     value = 25
                                                   )
                                            ),
                                            column(width = 4,
                                                   numericInput(
                                                     inputId = "FACS_Volcano_fontsize",
                                                     label = "Font size",
                                                     value = 15
                                                   ),
                                                   sliderTextInput(
                                                     inputId = "FACS_Volcano_dotsize",
                                                     label = "Dot size",
                                                     choices = seq(0, 10, 0.5),
                                                     grid = TRUE,
                                                     selected = 2
                                                   ),
                                                   shiny::actionButton(
                                                     inputId = "FACS_DEG_submit",
                                                     label = "Calculate",
                                                     width = "120px",
                                                     icon = icon("calculator"),
                                                     style = "color: #fff; background-color: #337ab7; border-color: #2e6da4"
                                                   )
                                            )
                                        ),
                                        box(width = 12,
                                            uiOutput("FACS_volcano_plot.ui"),
                                            downloadButton(
                                              outputId = 'FACS_volcano_download',
                                              label = 'Download',
                                              width = "120px",
                                              style = "color: #fff; background-color: #337ab7; border-color: #2e6da4"
                                            )
                                        ),
                                        box(width = 12,
                                            h3("Differentially expressed genes"),
                                            p(
                                              class = "text-muted",
                                              paste(
                                                "Note: Here only shows 2000 genes with the smallest adj.P.Val. If you want the full gene list, you can download it."
                                              )
                                            ),
                                            DT::dataTableOutput("FACS_DEGDataTable_output"),
                                            downloadButton(
                                              'FACS_DEGDataTable_download',
                                              label = 'Download',
                                              width = "120px",
                                              style = "color: #fff; background-color: #337ab7; border-color: #2e6da4"
                                            )
                                        )
                               )
                             )
                           ),
                           # ****Metadata Plot----
                           tabItem(
                             tabName = "Metadata",
                             dropdownButton(
                               selectInput(
                                 inputId = "Metadata_plottype",
                                 label = "Plot type",
                                 choices = c("barplot","boxplot","boxplot2"),
                                 selected = "barplot"
                               ),
                               selectInput(
                                 inputId = "Metadata_groupby1",
                                 label = "Group by",
                                 choices = c(
                                   "meta.cluster",
                                   "cancerType",
                                   "dataset",
                                   "loc",
                                   "treatment",
                                   "tech"
                                 ),
                                 selected = "meta.cluster"
                               ),
                               selectInput(
                                 inputId = "Metadata_cmp",
                                 label = "Comparison",
                                 choices = c(
                                   "meta.cluster",
                                   "cancerType",
                                   "dataset",
                                   "loc",
                                   "treatment",
                                   "tech"
                                 ),
                                 selected = "cancerType"
                               ),
                               numericInput(
                                 inputId = "Metadata_PlotWidth",
                                 label = "Plot height (px)",
                                 value = 600,
                                 step = 10
                               ),
                               numericInput(
                                 inputId = "Metadata_PlotHeight",
                                 label = "Plot height (px)",
                                 value = 300,
                                 step = 10
                               ),
                               numericInput(
                                 inputId = "Metadata_PlotNCol",
                                 label = "Col number",
                                 value = 1
                               ),
                               # selectInput(
                               #   inputId = "Metadata_quantify",
                               #   label = "Quantified by",
                               #   choices = c("Proportion", "Count"),
                               #   selected = "Proportion"
                               # ),
                               # materialSwitch(
                               #   inputId = "Metadata_flip",
                               #   label = "Coordinates flipped",
                               #   value = TRUE,
                               #   status = "primary",
                               #   right  = TRUE
                               # ),
                               # numericInput(
                               #   inputId = "Metadata_cutoff",
                               #   label = "Proportion shown in pie plot >",
                               #   value = 10
                               # ),
                               circle = TRUE,
                               status = "danger",
                               icon = icon("stats", lib = "glyphicon"),
                               width = "250px",
                               tooltip = tooltipOptions(title = "Click to see inputs!")
                             ),
                             uiOutput("Metadata_plot.ui"),
                             downloadButton(
                               outputId = 'Metadata_download',
                               label = 'Download',
                               width = "120px",
                               style = "color: #fff; background-color: #337ab7; border-color: #2e6da4"
                             )
                           ),
                           # ****DataTable Output----
                           # tabItem(
                           #   tabName = "DataTable",
                           #   DT::dataTableOutput("DataTable_output"),
                           #   downloadButton(
                           #     'DataTable_download',
                           #     label = 'Download',
                           #     width = "120px",
                           #     style = "color: #fff; background-color: #337ab7; border-color: #2e6da4"
                           #   )
                           # ),
                           # ****Instructions Output----
                           tabItem(
                             tabName = "Instructions",
                             box()
                             #tags$iframe(style="height:1200px; width:100%",src="User Manual of scDVA.pdf")
                           ),
                           # ****About Output----
                           tabItem(
                             tabName = "About",
                             h2("scDVA"),
                             HTML(
                               "<p style = 'margin-left:3%'><font size='4'>This data portal is based on framework scDVA (short for single cell RNA-seq data visualization and analyzation).</font></p>"
                             ),
                             HTML(
                               "<p style = 'margin-left:3%'><font size='4'>To use scDVA with your own data, please visit <a href = 'https://github.com/liziyie/scDVA'>scDVA</a> in GitHub.</font></p>"
                             ),
                             HTML(
                               "<p style = 'margin-left:3%'><font size='4'>This framework is developed by Ziyi Li of <a href = 'http://cancer-pku.cn/'>Zemin Zhang's lab</a>, Peking University. If any question about the use of scDVA or find any bug, please contact: <a href = 'mailto:liziyie@pku.edu.cn'>liziyie@pku.edu.cn</a></font></p>"
                             ),
                             h2("Dataset"),
                             HTML(
                               "<p style = 'margin-left:3%'><font size='4'><a href = 'https://www.google.com/'>L, Z. et al.</a> Comprehensive characterization of T cell in Pan-Cancer in single cell level.</font></p>"
                             ),
                             h2("GEPIA2"),
                             HTML(
                               "<p style = 'margin-left:3%'><font size='4'>Please try <a href = 'http://gepia2.cancer-pku.cn/#index'>GEPIA2</a> to analyze the bulk RNA-seq data from the TCGA and GTEx projects.</font></p>"
                             ),
                             h2("Contact us"),
                             HTML(
                               "<p style = 'margin-left:3%'><font size='4'>For any questions about this data portal, please contact us: <a href = 'mailto:cancerpku@pku.edu.cn'>cancerpku@pku.edu.cn</a> </font></p>"
                             )
                           )
                         )
                       ),
                       busyIndicator(text = "Please wait...", wait = 1000)
                     ),
                     # **Right dataset selected panel----
                     column(
                       width = 3,
                       box(
                         title = "Load data",
                         width = NULL,
                         status = "danger",
                         collapsible = T,
                         collapsed = F,
                         shinyTree(
                           "LoadDataTree", checkbox = TRUE, search = TRUE,
                           searchtime = 1000, theme = "proton"
                         ),
                         p(
                           class = "text-muted",
                           paste("Note: please select one or more dataset(s) to load.")
                         ),
                         shiny::actionButton(
                           inputId = "LoadButton",
                           label = "Load",
                           width = 120,
                           icon = icon("upload"),
                           style = "color: #fff; background-color: #337ab7; border-color: #2e6da4"
                         ),
                         shinyalert("LoadDataset_alert", FALSE, auto.close.after = 5)
                       ),
                       box(
                         title = "Select a dataset",
                         width = NULL,
                         status = "warning",
                         collapsible = T,
                         collapsed = F,
                         selectInput(
                           inputId = "SelectData_dataset",
                           label = "Dataset",
                           choices = "SampleData"
                         )
                       ),
                       box(
                         title = "Subset the dataset",
                         width = NULL,
                         status = "warning",
                         collapsible = T,
                         collapsed = F,
                         pickerInput(
                           inputId = "SubsetData_meta.cluster",
                           label = "Meta-Cluster",
                           choices = NULL,
                           multiple = TRUE,
                           options = list(
                             `actions-box` = TRUE,
                             `live-search` = TRUE,
                             size = 8
                           )
                         ),
                         pickerInput(
                           inputId = "SubsetData_cancerType",
                           label = "Cancer Type",
                           choices = NULL,
                           multiple = TRUE,
                           options = list(
                             `actions-box` = TRUE,
                             `live-search` = TRUE,
                             size = 8
                           )
                         ),
                         pickerInput(
                           inputId = "SubsetData_study",
                           label = "Study",
                           choices = NULL,
                           multiple = TRUE,
                           options = list(
                             `actions-box` = TRUE,
                             `live-search` = TRUE,
                             size = 8
                           )
                         ),
                         pickerInput(
                           inputId = "SubsetData_tissue",
                           label = "Tissue",
                           choices = NULL,
                           multiple = TRUE,
                           options = list(
                             `actions-box` = TRUE,
                             `live-search` = TRUE,
                             size = 8
                           )
                         ),
                         p(
                           class = "text-muted",
                           paste(
                             "Note: Please click the submit button in 'Gene Input' box after you subset the dataset."
                           )
                         )
                       )
                     )
                   ))
)


# Define UI----
##ui <- dashboardPage(header, sidebar, body, use_bs_popover())
ui <- dashboardPage(header, sidebar, body)
ui
