
#’ termTOgene
#'
#' Run ivTerm using graphic interface.
#' The user interface contains three tabs: "Upload File", "Data Selection", and "Visualization".
#'
#' @usage termTOgene(shiny.maxRequestSize = 100 * 1024^2, launch.browser = T)
#'
#' @param shiny.maxRequestSize The maximum size of the uploaded file.
#' @param launch.browser If `TRUE`, launch browser.
#'
#' @examples
#' library(ivTerm)
#' termTOgene()
#'
#' @importFrom shiny shinyApp tagList navbarPage tabPanel sidebarLayout sidebarPanel mainPanel fluidRow
#' column h3 h4 h5 h6 br hr actionButton fileInput selectInput uiOutput numericInput checkboxInput textInput
#' reactiveValues reactive observe req validate need observeEvent renderUI updateSelectInput tabsetPanel
#' sliderInput tableOutput showModal modalDialog a div
#' @importFrom ggplot2 ggplot geom_text theme scale_y_continuous scale_x_continuous aes labs theme_classic
#' geom_bar coord_flip geom_segment expansion element_text element_blank element_rect element_line theme_bw
#' scale_x_discrete coord_polar facet_grid scale_color_manual scale_fill_manual scale_color_gradientn
#' scale_fill_gradientn scale_y_discrete geom_polygon geom_point
#' @importFrom ggiraph geom_bar_interactive geom_point_interactive geom_tile_interactive geom_density_interactive
#' geom_histogram_interactive geom_boxplot_interactive geom_jitter_interactive renderGirafe girafeOutput
#' girafe_options girafe opts_selection opts_hover
#' @importFrom DT dataTableOutput renderDataTable datatable JS
#' @importFrom shinyjs useShinyjs hidden hide show toggleState
#' @importFrom tools file_ext
#' @importFrom ggnetwork ggnetwork
#' @importFrom RCurl getURL
#' @importFrom igraph graph_from_data_frame
#' @importFrom scales hue_pal
#' @importFrom XML xmlToDataFrame
#' @importFrom colourpicker colourInput
#' @importFrom esquisse palettePicker
#'
#' @export
#'

termTOgene <- function(shiny.maxRequestSize = 100 * 1024^2, launch.browser = T){
  if (interactive()) {
    options(shiny.maxRequestSize = shiny.maxRequestSize)
    shinyApp(
      ui <-  tagList(
        shinyjs::useShinyjs(),
        navbarPage(
        "ivTerm",
        tabPanel(
          "File Upload",
          sidebarLayout(
            sidebarPanel(
              width = 4,
              h3("Load Demo"),
              actionButton("file_demo1", "Load Demo 1 (Li JH, et al.)"),
              actionButton("file_demo2", "Load Demo 2 (Zhou Z, et al.)"),
              hr(),
              h3("Upload File"),
              h5("The file accepts .txt and .csv formats. TXT files should be separated by tabs."),
              h6(paste0("This file should contain at least term ids (the column named 'term_id') and the genes annotated to each term (the column named 'gene'). ",
                        "The column named 'group' is used to compare of multiple datasets or groups. ",
                        "The term id is used to uniquely identify the functional term.")),
              fluidRow(
                column(width = 8, fileInput("file_term", "Please upload term data: ", accept = c(".csv", ".tsv", ".txt"))),
                column(width = 4, selectInput("genes_split", "Separator for multiple gene:", c(";", "/", ",", "space" = " "), ","))
              ),
              h6("This file should contain gene labels (the column named 'gene') matching those used in term data and some information associated with genes."),
              fileInput("file_gene", "Please upload gene data: ", accept = c(".csv", ".tsv", ".txt"))
            ),
            mainPanel(
              width = 8,
              h4("After the data is uploaded and checked, it will be displayed in the table below, and the result tabs will appear in the top menu."),
              hr(),
              h4("The data of terms:"),
              DT::dataTableOutput("upload_term_table"),
              h4("The data of genes:"),
              DT::dataTableOutput("upload_gene_table")
            )

          )
        ),
        tabPanel(
          "Data Selection",
          h3("1. Choose group(s)"),
          uiOutput("ui_choose_group"),
          hr(),
          h3("2. Select rows from table:"),
          fluidRow(
            column(width = 6,
                   h4("All data:"),
                   DT::dataTableOutput("choose_term_table")),
            column(width = 6,
                   h4("Selected data:"),
                   DT::dataTableOutput("select_term_table"))
          ),
          hr()
          ),
        tabPanel(
          "Visualization",
          tabsetPanel(
            id = "tabs", type = "pills",
            tabPanel(
              "Barplot", br(),
              sidebarLayout(
                sidebarPanel(
                  width = 3,
                  fluidRow(
                    column(width = 6,
                          selectInput("barplot_x", "X", NULL),
                          selectInput("barplot_color", "Color", NULL)),
                    column(width = 6,
                           selectInput("barplot_y", "Y", NULL),
                           selectInput("barplot_side", "Side", NULL))
                  ),
                  a(id = "barplot_toogle", "Show/hide more options"),
                  div(id = "barplot_args",
                  fluidRow(column(width = 6,
                                  numericInput("barplot_height", "Height", value = 500),
                                  textInput("barplot_x_title", "X title", ""),
                                  numericInput("barplot_x_text_size", "X text size", 8),
                                  colourpicker::colourInput("barplot_bar_color", "Bar color", "gray"),
                                  textInput("barplot_lg_title", "Legend title", ""),
                                  selectInput("barplot_lg_pos", "Legend position", c("right", "top", "bottom", "left")),
                                  checkboxInput("barplot_side_rev", "Reverse", FALSE)
                                  ),
                           column(width = 6,
                                  numericInput("barplot_width", "Width", value = 1000),
                                  numericInput("barplot_x_title_size", "X title size", 10),
                                  numericInput("barplot_y_text_size", "Y text size", 8),
                                  uiOutput("ui_barplot_palette"),
                                  numericInput("barplot_lg_title_size", "Legend title size", 10),
                                  numericInput("barplot_lg_text_size", "Legend text size", 8)
                                  ))
                  )
                ),
                mainPanel(
                  width = 9,
                  uiOutput("ui_barplot")
                )
              )
            ),
            tabPanel(
              "Bubble Chart", br(),
              sidebarLayout(
                sidebarPanel(
                  width = 3,
                  fluidRow(
                    column(width = 6,
                           selectInput("bubble_x", "X", NULL),
                           selectInput("bubble_color", "Color", NULL)
                           ),
                    column(width = 6,
                           selectInput("bubble_y", "Y", NULL),
                           selectInput("bubble_size", "Size", NULL)
                           )
                  ),
                  a(id = "bubble_toogle", "Show/hide more options"),
                  div(id = "bubble_args",
                  fluidRow(column(width = 6,
                                  numericInput("bubble_height", "Height", value = 500),
                                  textInput("bubble_x_title", "X title", ""),
                                  numericInput("bubble_x_title_size", "X title size", 10),
                                  numericInput("bubble_x_text_size", "X text size", 8),
                                  colourpicker::colourInput("bubble_point_color", "Bubble color", "gray"),
                                  textInput("bubble_color_title", "Color title", NULL),
                                  numericInput("bubble_title_size", "Legend title size", 10),
                                  selectInput("bubble_lg_pos", "Legend Position", c("right", "top", "bottom", "left")),
                                  numericInput("bubble_point_size", "Point Size", 5)
                                  ),
                           column(width = 6,
                                  numericInput("bubble_width", "Width", value = 1000),
                                  textInput("bubble_y_title", "Y title", ""),
                                  numericInput("bubble_y_title_size", "Y title size", 10),
                                  numericInput("bubble_y_text_size", "Y text size", 8),
                                  uiOutput("ui_bubble_palette"),
                                  textInput("bubble_size_title", "Size title", NULL),
                                  numericInput("bubble_text_size", "Legend text size", 8),
                                  sliderInput("bubble_point_alpha", "Point alpha", 0, 1, 1, .05)
                                  ))
                  )
                ),
                mainPanel(
                  width = 9,
                  uiOutput("ui_bubble")
                )
              )
            ),
            tabPanel(
              "Lollipop Chart", br(),
              sidebarLayout(
                sidebarPanel(
                  width = 3,
                  fluidRow(
                    column(width = 6,
                           selectInput("lollipop_x", "X", NULL),
                           selectInput("lollipop_color", "Color", NULL),
                           selectInput("lollipop_side", "Side", NULL)
                           ),
                    column(width = 6,
                           selectInput("lollipop_y", "Y", NULL),
                           selectInput("lollipop_size", "Size", NULL)
                           )
                  ),
                  a(id = "lollipop_toogle", "Show/hide more options"),
                  div(id = "lollipop_args",
                  fluidRow(column(width = 6,
                                  numericInput("lollipop_height", "Height", value = 500),
                                  textInput("lollipop_x_title", "X title", ""),
                                  numericInput("lollipop_x_text_size", "X text size", 8),
                                  colourpicker::colourInput("lollipop_point_color", "Point color", "black"),
                                  textInput("lollipop_color_title", "Color title", ""),
                                  numericInput("lollipop_lg_title_size", "Legend title size", 10),
                                  selectInput("lollipop_lg_pos", "Legend position", c("right", "top", "bottom", "left")),
                                  colourpicker::colourInput("lollipop_line_color", "Line color", "gray"),
                                  checkboxInput("lollipop_side_rev", "Reverse", FALSE)),
                           column(width = 6,
                                  numericInput("lollipop_width", "Width", value = 1000),
                                  numericInput("lollipop_x_title_size", "X title size", 10),
                                  numericInput("lollipop_y_text_size", "Y text size", 8),
                                  uiOutput("ui_lollipop_palette"),
                                  textInput("lollipop_size_title", "Size title", ""),
                                  numericInput("lollipop_lg_text_size", "Legend text size", 10),
                                  numericInput("lollipop_point_size", "Point size", 2),
                                  sliderInput("lollipop_line_width", "Line width", 0, .2, .05, .01)

                                  ))
                  )
                ),
                mainPanel(
                  width = 9,
                  uiOutput("ui_lollipop")
                )
              )
            ),
            tabPanel(
              "Network", br(),
              sidebarLayout(
                sidebarPanel(
                  width = 3,
                  fluidRow(
                    column(width = 6,
                           selectInput("network_color", "Color", NULL),
                           numericInput("network_top_n", "Show top N", 10, min = 1)
                           ),
                    column(width = 6,
                           selectInput("network_size", "Size", NULL))
                  ),
                  a(id = "network_toogle", "Show/hide more options"),
                  div(id = "network_args",
                  fluidRow(
                    column(width = 6,
                           numericInput("network_height", "Height", 500),
                           colourpicker::colourInput("network_point_color", "Point color", "black"),
                           textInput("network_color_title", "Color title", ""),
                           numericInput("network_lg_title_size", "Legend title size", 10),
                           selectInput("network_lg_pos", "Legend position", c("right", "top", "bottom", "left")),
                           numericInput("network_point_size", "Point size", 5)
                    ),
                    column(width = 6,
                           numericInput("network_width", "Width", 500),
                           uiOutput("ui_network_palette"),
                           textInput("network_size_title", "Size title", ""),
                           numericInput("network_lg_text_size", "Legend text size", 8),
                           numericInput("network_line_width", "Line width", 1)
                    )))
                ),
                mainPanel(
                  width = 9,
                  uiOutput("ui_network")
                )
              )
            ),
            tabPanel(
              "Complex Barplot", br(),
              sidebarLayout(
                sidebarPanel(
                  width = 3,
                  selectInput("complexbar_y", "Y (select from term_data)", NULL),
                  selectInput("complexbar_color", "Color (select from gene_data)", NULL),
                  fluidRow(
                    column(width = 9, selectInput("complexbar_order", "Order by (select from gene_data)", NULL)),
                    column(width = 3, checkboxInput("complexbar_decs", "decreasing", TRUE))
                  ),
                  selectInput("complexbar_split", "Split by (select from gene_data)", NULL),
                  numericInput("complexbar_topn", "Show top N", 10, min = 1),
                  a(id = "complexbar_toogle", "Show/hide more options"),
                  div(id = "complexbar_args",
                  fluidRow(
                    column(width = 6, numericInput("complexbar_height", "Height", 500),
                           textInput("complexbar_x_title", "X title", "Count"),
                           numericInput("complexbar_x_text_size", "X Text size", 8),
                           uiOutput("ui_complexbar_palette"),
                           numericInput("complexbar_lg_title_size", "Legend title size", 10),
                           selectInput("complexbar_lg_pos", "Legend position", c("right", "top", "bottom", "left"))
                           ),
                    column(width = 6, numericInput("complexbar_width", "Width", 1000),
                           numericInput("complexbar_x_title_size", "X Title size", 10),
                           numericInput("complexbar_y_text_size", "Y text size", 8),
                           textInput("complexbar_color_title", "Color title", ""),
                           numericInput("complexbar_lg_text_size", "Legend text size", 8),
                           numericInput("complexbar_split_size", "Split title size", 10)
                           )
                  ))
                ),
                mainPanel(
                  width = 9,
                  uiOutput("ui_complexbar")
                )
              )
            ),
            tabPanel(
              "Heatmap", br(),
              sidebarLayout(
                sidebarPanel(
                  width = 3,
                  fluidRow(
                    column(width = 6,
                           selectInput("heatmap_y", "Y", NULL)
                           ),
                    column(width = 6,
                           selectInput("heatmap_color", "Color", NULL))
                  ),
                  a(id = "heatmap_toogle", "Show/hide more options"),
                  div(id = "heatmap_args",
                  fluidRow(
                    column(width = 6,
                           numericInput("heatmap_height", "Height", 500),
                           uiOutput("ui_heatmap_palette"),
                           numericInput("heatmap_rowname_size", "Row names size", 10, min = 0),
                           textInput("heatmap_color_title", "Color title", ""),
                           numericInput("heatmap_lg_text_size", "Legend text size", 8)
                    ),
                    column(width = 6,
                           numericInput("heatmap_width", "Width", 500),
                           numericInput("heatmap_colname_size", "Column names size", 10, min = 0),
                           sliderInput("heatmap_colname_angle", "Column names angle", 90, min = 0, max = 270, step = 90),
                           numericInput("heatmap_lg_title_size", "Legend title size", 10),
                           selectInput("heatmap_lg_pos", "Legend position", c("right", "top", "bottom", "left"))
                    )))
                ),
                mainPanel(
                  width = 9,
                  uiOutput("ui_heatmap")
                )
              )
            ),
            tabPanel(
              "Dot Plot", br(),
              sidebarLayout(
                sidebarPanel(
                  width = 3,
                  fluidRow(
                    column(width = 6, selectInput("dotplot_y", "Y", NULL),
                           selectInput("dotplot_size", "Size", NULL)),
                    column(width = 6, selectInput("dotplot_color", "Color", NULL))
                  ),
                  a(id = "dotplot_toogle", "Show/hide more options"),
                  div(id = "dotplot_args",
                  fluidRow(
                    column(width = 6,
                           numericInput("dotplot_height", "Height", 500),
                           uiOutput("ui_dotplot_palette"),
                           numericInput("dotplot_rowname_size", "Row names size", 10, min = 0),
                           textInput("dotplot_color_title", "Color title", ""),
                           numericInput("dotplot_lg_title_size", "Legend title size", 10),
                           selectInput("dotplot_lg_pos", "Legend position", c("right", "top", "bottom", "left"))
                    ),
                    column(width = 6,
                           numericInput("dotplot_width", "Width", 500),
                           numericInput("dotplot_colname_size", "Column names size", 10, min = 0),
                           sliderInput("dotplot_colname_angle", "Column names angle", 90, min = 0, max = 270, step = 90),
                           textInput("dotplot_size_title", "Size title", ""),
                           numericInput("dotplot_lg_text_size", "Legend text size", 8)
                    )))
                ),
                mainPanel(
                  width = 9,
                  uiOutput("ui_dotplot")
                )
              )
            )
          ),
        h3("More"),
        tabsetPanel(
          tabPanel(
            "Detail",
            tableOutput("term_detail")
          ),
          tabPanel(
            "Term Information",
            DT::dataTableOutput("term_info")
          ),
          tabPanel(
            "Gene",
            br(),
            fluidRow(
              column(
                width = 5,
                DT::dataTableOutput("gene_data")
                ),
              column(
                width = 1,
                selectInput("gene_chart_type", "Chart:",
                            c(" " = " ",
                              "Pie Chart" = "pie",
                              "Histogram" = "hist",
                              "Density" = "density",
                              "Bar Plot" = "bar",
                              "Scatter Plot" = "scatter",
                              "Box Chart" = "box",
                              "Violin" = "violin",
                              "Heatmap" = "heat",
                              "Dumbbell" = "dumbbell")),
                uiOutput("ui_gene_param")
              ),
              column(
                width = 5,
                uiOutput("ui_gene_chart")
              )
            )
          )
        )
      ))),
      server = function(input, output, session) {

        v_data <- reactiveValues(term_data = NULL, gene_data = NULL)

        upload_term_data <- reactive({
          req(input$file_term)
          ext <- tools::file_ext(input$file_term$datapath)
          validate(need(ext %in%  c("csv", "txt", "tsv"), "Invalid file; Please upload a .csv or a .txt file"))
          if(ext == "csv"){
            read.csv(input$file_term$datapath, header = T, stringsAsFactors = F)
          } else {
            read.table(input$file_term$datapath, header = T, sep = "\t", stringsAsFactors = F)
          }
        })

        upload_gene_data <- reactive({
          req(input$file_gene)
          ext <- tools::file_ext(input$file_gene$datapath)
          validate(need(ext %in%  c("csv", "txt", "tsv"), "Invalid file; Please upload a .csv or a .txt file"))
          if(ext == "csv"){
            read.csv(input$file_gene$datapath, header = T, stringsAsFactors = F)
          } else {
            read.table(input$file_gene$datapath, header = T, sep = "\t", stringsAsFactors = F)
          }
        })

        observeEvent(input$file_term, {
          req(upload_term_data())
          v_data$term_data <- upload_term_data()
        })

        observeEvent(input$file_gene, {
          req(upload_gene_data())
          v_data$gene_data <- upload_gene_data()
        })

        observeEvent(input$file_demo1, {
          data("demo_igc")
          updateSelectInput(session, "genes_split", "Separator for multiple gene:", c(";", "/", ",", "space" = " "), ",")
          v_data$term_data <- demo_igc$term_data
          v_data$gene_data <- demo_igc$gene_data
        })

        observeEvent(input$file_demo2, {
          data("demo_coral")
          updateSelectInput(session, "genes_split", "Separator for multiple gene:", c(";", "/", ",", "space" = " "), " ")
          v_data$term_data <- demo_coral$term_data
          v_data$gene_data <- demo_coral$gene_data
        })


        input_gene_data <- reactive({
          req(v_data$gene_data)
          gene_data <- v_data$gene_data
          if(!("gene" %in% colnames(gene_data))){
            showModal(modalDialog("The gene data should contain a column named 'gene'.", easyClose = TRUE, footer = NULL))
            NULL
          } else if (any(duplicated(gene_data[, "gene"]))) {
            showModal(modalDialog("The column named 'gene' should not contain duplicated values.", easyClose = TRUE, footer = NULL))
            NULL
          } else {
            gene_data
          }
        })

        input_term_data <- reactive({
          req(v_data$term_data)
          term_data <- v_data$term_data
          if(!all(c("term_id", "gene") %in% colnames(term_data))){
            showModal(modalDialog("The term data should contain a column named 'term_id' and a column named 'gene'.", easyClose = TRUE, footer = NULL))
            NULL
          } else if (!("group" %in% colnames(term_data)) & any(duplicated(term_data[, "term_id"]))) {
            showModal(modalDialog("The column named 'term_id' should not contain duplicated values.", easyClose = TRUE, footer = NULL))
            NULL
          } else if (("group" %in% colnames(term_data))){
            if(any(duplicated(term_data[, c("term_id", "group")]))){
              showModal(modalDialog("The column named 'term_id' should not contain duplicated values in each group.", easyClose = TRUE, footer = NULL))
              NULL
            } else {
              term_data
            }
          } else {
            term_data
          }
        })

        output$upload_term_table <- DT::renderDataTable({
          req(input_term_data())
          DT::datatable(input_term_data(), selection = "none",
                        options = list(scrollX  = T))
        })

        output$upload_gene_table <- DT::renderDataTable({
          req(input_gene_data())
          DT::datatable(input_gene_data(), selection = "none",
                        options = list(scrollX  = T))
        })


        output$ui_choose_group <- renderUI({
          req(input_term_data())

          term_data <- input_term_data()

          if("group" %in% colnames(term_data)){
            selectInput("selected_group", "Group:", unique(term_data$group), multiple = T)
          } else {
             h5("Can not find column named 'group', so skip this step.")
          }
        })

        choose_term_data <- reactive({
          req(input_term_data())
          term_data <- input_term_data()

          if("group" %in% colnames(term_data)){
            req(input$selected_group)
            res <- subset(term_data, group %in% input$selected_group)
            if(length(input$selected_group) == 1){
              res <- res[, -which("group" == colnames(term_data))]
            }
            res
          } else {
            term_data
          }
        })

        output$choose_term_table <- DT::renderDataTable({
          req(choose_term_data())
          term_data <- choose_term_data()
          #term_data$gene <- paste0(substring(term_data$gene, 1, 10), "...")
          term_data <- subset(term_data, select = -gene)
          DT::datatable(term_data,
                        extensions = c("FixedHeader"),
                        rownames = FALSE,
                        options = list(
                          fixedHeader = TRUE,
                          scrollX = TRUE
                        ))

        })

        select_term_data <- reactive({
          req(choose_term_data())
          req(input$choose_term_table_rows_selected)

          choose_term_data()[input$choose_term_table_rows_selected, ]
         })

        output$select_term_table <- DT::renderDataTable({
          req(select_term_data())
          term_data <- select_term_data()
          #term_data$gene <- paste0(substring(term_data$gene, 1, 10), "...")
          term_data <- subset(term_data, select = -gene)
          DT::datatable(term_data,
                        extensions = "FixedHeader",
                        options= list(fixedHeader = TRUE, scrollX = TRUE),
                        rownames = F, selection = "none")
        })


        ####  p_data

        p_gene_data <- reactive({
          req(p_term_data())
          req(input_gene_data())
          input_gene_data()
        })


        p_term_data <- reactive({
          req(select_term_data())
          select_term_data()
        })

        observeEvent(p_term_data(), {
          if("group" %in% colnames(p_term_data())){
            hideTab(inputId = "tabs", target = "Barplot")
            hideTab(inputId = "tabs", target = "Bubble Chart")
            hideTab(inputId = "tabs", target = "Lollipop Chart")
            hideTab(inputId = "tabs", target = "Network")
            hideTab(inputId = "tabs", target = "Complex Barplot")
            showTab(inputId = "tabs", target = "Heatmap")
            showTab(inputId = "tabs", target = "Dot Plot")
            updateTabsetPanel(session, "tabs", selected = "Heatmap")
          } else {
            showTab(inputId = "tabs", target = "Barplot")
            showTab(inputId = "tabs", target = "Bubble Chart")
            showTab(inputId = "tabs", target = "Lollipop Chart")
            showTab(inputId = "tabs", target = "Network")
            showTab(inputId = "tabs", target = "Complex Barplot")
            hideTab(inputId = "tabs", target = "Heatmap")
            hideTab(inputId = "tabs", target = "Dot Plot")
            updateTabsetPanel(session, "tabs", selected = "Barplot")
          }
        })

        observe({
          shinyjs::onclick("barplot_toogle", shinyjs::toggle(id = "barplot_args", anim = TRUE))
          shinyjs::onclick("bubble_toogle", shinyjs::toggle(id = "bubble_args", anim = TRUE))
          shinyjs::onclick("lollipop_toogle", shinyjs::toggle(id = "lollipop_args", anim = TRUE))
          shinyjs::onclick("network_toogle", shinyjs::toggle(id = "network_args", anim = TRUE))
          shinyjs::onclick("complexbar_toogle", shinyjs::toggle(id = "complexbar_args", anim = TRUE))
          shinyjs::onclick("heatmap_toogle", shinyjs::toggle(id = "heatmap_args", anim = TRUE))
          shinyjs::onclick("dotplot_toogle", shinyjs::toggle(id = "dotplot_args", anim = TRUE))
        })

        #####  barplot

        observeEvent(p_term_data(), {
          req(p_term_data())
          term_data <- p_term_data()
          columns <- colnames(term_data)
          column_n <- columns[unlist(lapply(as.list(term_data),is.numeric))]
          column_c <- columns[unlist(lapply(as.list(term_data),is.character))]
          column_c_term <- column_c[column_c %in% c("term_id", "term_name")]
          column_side <- columns[unlist(lapply(as.list(term_data),function(x){length(unique(x)) == 2}))]
          updateSelectInput(session, "barplot_y", "Y", column_c_term)
          updateSelectInput(session, "barplot_x", "X", column_n)
          updateSelectInput(session, "barplot_color", "Color",
                            structure(c(" ", columns), names = c("-", columns)))
          updateSelectInput(session, "barplot_side", "Side",
                            structure(c(" ", column_side), names = c("-", column_side)))
        })

        output$ui_barplot_palette <- renderUI({
          req(p_term_data())
          term_data <- p_term_data()
          req(input$barplot_color)

          if(input$barplot_color != " "){
            if(is.numeric(term_data[[input$barplot_color]])){
              esquisse::palettePicker("barplot_bar_palette", "Bar palette", getseqcolor())
            } else {
              color_n <- length(unique(term_data[[input$barplot_color]]))
              esquisse::palettePicker("barplot_bar_palette", "Bar palette", getqualcolor(color_n))
            }
          } else {
            esquisse::palettePicker("barplot_bar_palette", "Bar palette", NULL)
          }
        })

        observeEvent(input$barplot_x, {
          updateTextInput(session, "barplot_x_title", "X title", input$barplot_x)
        })

        observeEvent(input$barplot_color, {
            updateTextInput(session, "barplot_lg_title", "Color title", input$barplot_color)
        })

        output$ui_barplot <- renderUI({
          girafeOutput("barplot", height = input$barplot_height, width = input$barplot_width)
        })

        observe({
          shinyjs::toggleState(id = "barplot_bar_color", condition = input$barplot_color == " ")
          shinyjs::toggleState(id = "barplot_bar_palette", condition = input$barplot_color != " ")
          shinyjs::toggleState(id = "barplot_side_rev", condition = input$barplot_side != " ")
          shinyjs::toggleState(id = "barplot_lg_title", condition = input$barplot_color != " ")
        })

        output$barplot <- renderGirafe({
          validate(need(input$barplot_height > 0, "The height should be a positive number."))
          validate(need(input$barplot_width > 0, "The width should be a positive number."))
          req(input$barplot_x)
          req(input$barplot_color)
          req(input$barplot_side)
          req(p_term_data())

          term_data <- p_term_data()

          if(input$barplot_side == " "){
            p <- ggplot(term_data, aes( x = reorder(.data[[input$barplot_y]], .data[[input$barplot_x]]),
                                        y = .data[[input$barplot_x]],
                                        tooltip = .data[[input$barplot_x]],
                                        data_id = term_id) )
          } else {
            sides <- unique(term_data[[input$barplot_side]])
            x_limits <- c(-max(term_data[[input$barplot_x]]), max(term_data[[input$barplot_x]]))
            x_breaks <- pretty(x_limits)
            x_side <- ifelse(input$barplot_side_rev, 2, 1)
            p <- ggplot(term_data, aes( x = reorder(term_id, .data[[input$barplot_x]]),
                                        y = ifelse(.data[[input$barplot_side]] == sides[x_side],
                                                   .data[[input$barplot_x]],
                                                   -.data[[input$barplot_x]]),
                                        data_id = term_id,
                                        tooltip = .data[[input$barplot_x]]) )
          }

          if(input$barplot_color == " "){
            p <- p +
              geom_bar_interactive(stat = "identity", fill = input$barplot_bar_color)
          } else {
            req(input$barplot_bar_palette)
            if(is.numeric(term_data[[input$barplot_color]]) & input$barplot_bar_palette %in% names(getseqcolor())){
              p <- p +
                geom_bar_interactive(aes(fill = .data[[input$barplot_color]]), stat = "identity") +
                scale_fill_gradientn(colors = getseqcolor()[[input$barplot_bar_palette]])
            } else if(is.character(term_data[[input$barplot_color]]) &
                      input$barplot_bar_palette %in% names(getqualcolor(1))) {
              color_n <- length(unique(term_data[[input$barplot_color]]))
              p <- p +
                geom_bar_interactive(aes(fill = .data[[input$barplot_color]]), stat = "identity") +
                scale_fill_manual(values = getqualcolor(color_n)[[input$barplot_bar_palette]])
            }
          }

          if(input$barplot_side == " "){
            p <- p +
              scale_y_continuous(expand = expansion(mult = c(0,.05))) +
              theme(axis.text.y = element_text(size = input$barplot_y_text_size))
          } else {
            p <- p +
              geom_text(aes(label = .data[[input$barplot_y]], y = ifelse(.data[[input$barplot_side]] == sides[x_side],
                                                        x_limits[2] * -.01, x_limits[2] * .01),
                            hjust = ifelse(.data[[input$barplot_side]] == sides[x_side], 1, 0)),
                        size = input$barplot_y_text_size * 0.376) +
              scale_y_continuous(breaks = x_breaks,
                                 labels = abs(x_breaks),
                                 limits = x_limits) +
              theme(axis.text.y = element_blank())
          }

          p <- p +
            coord_flip() +
            labs(y = input$barplot_x_title, fill = input$barplot_lg_title) +
            theme(axis.line.y = element_blank(),
                  axis.title.y = element_blank(),
                  axis.ticks.y = element_blank(),
                  axis.line.x = element_line(),
                  axis.text.x = element_text(size = input$barplot_x_text_size),
                  axis.title.x = element_text(size = input$barplot_x_title_size),
                  legend.title = element_text(size = input$barplot_lg_title_size),
                  legend.text = element_text(size = input$barplot_lg_text_size),
                  legend.position = input$barplot_lg_pos,
                  panel.background = element_rect(fill = "white"),
                  panel.grid = element_blank())

          x <- girafe(ggobj = p, height_svg = input$barplot_height/100, width_svg = input$barplot_width/100)
          set_girafe(x)
        })

        ##### bubble

        observeEvent(p_term_data(), {
          term_data <- p_term_data()
          columns <- colnames(term_data)
          column_n <- columns[unlist(lapply(as.list(term_data),is.numeric))]
          updateSelectInput(session, "bubble_x", "X", column_n)
          updateSelectInput(session, "bubble_y", "Y", column_n)
          updateSelectInput(session, "bubble_color", "Color",
                            structure(c(" ", columns), names = c("-", columns)))
          updateSelectInput(session, "bubble_size", "Size",
                            structure(c(" ", column_n), names = c("-", column_n)))

        })

        output$ui_bubble_palette <- renderUI({
          req(p_term_data())
          term_data <- p_term_data()
          req(input$bubble_color)

          if(input$bubble_color != " "){
            if(is.numeric(term_data[[input$bubble_color]])){
              esquisse::palettePicker("bubble_point_palette", "Point palette", getseqcolor())
            } else {
              color_n <- length(unique(term_data[[input$bubble_color]]))
              esquisse::palettePicker("bubble_point_palette", "Point palette", getqualcolor(color_n))
            }
          } else {
            esquisse::palettePicker("bubble_point_palette", "Point palette", NULL)
          }
        })

        observeEvent(input$bubble_x, {
          updateTextInput(session, "bubble_x_title", "X title", input$bubble_x)
        })

        observeEvent(input$bubble_y, {
          updateTextInput(session, "bubble_y_title", "Y title", input$bubble_y)
        })

        observeEvent(input$bubble_color, {
          updateTextInput(session, "bubble_color_title", "Color title", input$bubble_color)
        })

        observeEvent(input$bubble_size, {
          updateTextInput(session, "bubble_size_title", "Size title", input$bubble_size)
        })

        output$ui_bubble <- renderUI({
          girafeOutput("bubble", height = input$bubble_height, width = input$bubble_width)
        })

        observe({
          shinyjs::toggleState(id = "bubble_point_color", condition = input$bubble_color == " ")
          shinyjs::toggleState(id = "bubble_point_palette", condition = input$bubble_color != " ")
          shinyjs::toggleState(id = "bubble_color_title", condition = input$bubble_color != " ")
          shinyjs::toggleState(id = "bubble_size_title", condition = input$bubble_size != " ")
          shinyjs::toggleState(id = "bubble_point_size", condition = input$bubble_size == " ")
        })

        output$bubble <- renderGirafe({
          validate(need(input$bubble_height > 0, "The height should be a positive number."))
          validate(need(input$bubble_width > 0, "The width should be a positive number."))
          req(input$bubble_x)
          req(input$bubble_y)
          req(input$bubble_color)
          req(input$bubble_size)
          req(p_term_data())

          term_data <- p_term_data()

          p <- ggplot(term_data, aes( x = .data[[input$bubble_x]],
                                      y = .data[[input$bubble_y]],
                                      data_id = term_id,
                                      tooltip = paste0("x:", .data[[input$bubble_x]],
                                                       "\ny:", .data[[input$bubble_y]])) )

          if(input$bubble_color == " "){
            if(input$bubble_size == " "){
              p <- p + geom_point_interactive(color = input$bubble_point_color, alpha = input$bubble_point_alpha,
                                              size = input$bubble_point_size)
            } else {
              p <- p + geom_point_interactive(aes(size = .data[[input$bubble_size]]),
                                              color = input$bubble_point_color, alpha = input$bubble_point_alpha)
            }
          } else {
            req(input$bubble_point_palette)
            if(input$bubble_size == " "){
              if(is.numeric(term_data[[input$bubble_color]]) & input$bubble_point_palette %in% names(getseqcolor())){
                p <- p +
                  geom_point_interactive(aes(color = .data[[input$bubble_color]]),
                                         alpha = input$bubble_point_alpha, size = input$bubble_point_size) +
                  scale_color_gradientn(colors = getseqcolor()[[input$bubble_point_palette]])
              } else if(is.character(term_data[[input$bubble_color]]) &
                        input$bubble_point_palette %in% names(getqualcolor(1))) {
                color_n <- length(unique(term_data[[input$bubble_color]]))
                p <- p +
                  geom_point_interactive(aes(color = .data[[input$bubble_color]]),
                                         alpha = input$bubble_point_alpha, size = input$bubble_point_size) +
                  scale_color_manual(values = getqualcolor(color_n)[[input$bubble_point_palette]])
              }
            } else {
              if(is.numeric(term_data[[input$bubble_color]]) & input$bubble_point_palette %in% names(getseqcolor())){
                p <- p +
                  geom_point_interactive(aes(color = .data[[input$bubble_color]],
                                             size = .data[[input$bubble_size]]),
                                         alpha = input$bubble_point_alpha) +
                  scale_color_gradientn(colors = getseqcolor()[[input$bubble_point_palette]])
              } else if(is.character(term_data[[input$bubble_color]]) &
                        input$bubble_point_palette %in% names(getqualcolor(1))) {
                color_n <- length(unique(term_data[[input$bubble_color]]))
                p <- p +
                  geom_point_interactive(aes(color = .data[[input$bubble_color]],
                                             size = .data[[input$bubble_size]]),
                                         alpha = input$bubble_point_alpha) +
                  scale_color_manual(values = getqualcolor(color_n)[[input$bubble_point_palette]])
              }
            }
          }

          p <- p  +
            theme_classic() +
            labs(x = input$bubble_x_title, y = input$bubble_y_title,
                 color = input$bubble_color_title, size = input$bubble_size_title) +
            theme(axis.text.x = element_text(size = input$bubble_x_text_size),
                  axis.title.x = element_text(size = input$bubble_x_title_size),
                  axis.text.y = element_text(size = input$bubble_y_text_size),
                  axis.title.y = element_text(size = input$bubble_y_title_size),
                  legend.title = element_text(size = input$bubble_lg_title_size),
                  legend.text = element_text(size = input$bubble_lg_text_size),
                  legend.position = input$bubble_lg_pos)

          x <- girafe(ggobj = p, height_svg = input$bubble_height/100, width_svg = input$bubble_width/100)
          set_girafe(x)
        })

        ##### lollipop

        observeEvent(p_term_data(), {
          term_data <- p_term_data()
          columns <- colnames(term_data)
          column_n <- columns[unlist(lapply(as.list(term_data),is.numeric))]
          column_c <- columns[unlist(lapply(as.list(term_data),is.character))]
          column_c_term <- column_c[column_c %in% c("term_id", "term_name")]
          column_side <- columns[unlist(lapply(as.list(term_data),function(x){length(unique(x)) == 2}))]
          updateSelectInput(session, "lollipop_y", "Y", column_c_term)
          updateSelectInput(session, "lollipop_x", "X", column_n)
          updateSelectInput(session, "lollipop_color", "Color",
                            structure(c(" ", columns), names = c("-", columns)))
          updateSelectInput(session, "lollipop_size", "Size",
                            structure(c(" ", column_n), names = c("-", column_n)))
          updateSelectInput(session, "lollipop_side", "Side",
                            structure(c(" ", column_side), names = c("-", column_side)))
        })

        output$ui_lollipop_palette <- renderUI({
          req(p_term_data())
          term_data <- p_term_data()
          req(input$lollipop_color)

          if(input$lollipop_color != " "){
            if(is.numeric(term_data[[input$lollipop_color]])){
              esquisse::palettePicker("lollipop_point_palette", "Point palette", getseqcolor())
            } else {
              color_n <- length(unique(term_data[[input$lollipop_color]]))
              esquisse::palettePicker("lollipop_point_palette", "Point palette", getqualcolor(color_n))
            }
          } else {
            esquisse::palettePicker("lollipop_point_palette", "Point palette", NULL)
          }
        })

        observeEvent(input$lollipop_x, {
          updateTextInput(session, "lollipop_x_title", "X title", input$lollipop_x)
        })

        observeEvent(input$lollipop_color, {
          updateTextInput(session, "lollipop_color_title", "Color title", input$lollipop_color)
        })

        observeEvent(input$lollipop_size, {
          updateTextInput(session, "lollipop_size_title", "Size title", input$lollipop_size)
        })

        output$ui_lollipop <- renderUI({
          girafeOutput("lollipop")
        })

        observe({
          shinyjs::toggleState(id = "lollipop_point_color", condition = input$lollipop_color == " ")
          shinyjs::toggleState(id = "lollipop_point_palette", condition = input$lollipop_color != " ")
          shinyjs::toggleState(id = "lollipop_color_title", condition = input$lollipop_color != " ")
          shinyjs::toggleState(id = "lollipop_size_title", condition = input$lollipop_size != " ")
          shinyjs::toggleState(id = "lollipop_point_size", condition = input$lollipop_size == " ")
          shinyjs::toggleState(id = "lollipop_side_rev", condition = input$lollipop_side != " ")
        })


        output$lollipop <- renderGirafe({
          validate(need(input$lollipop_height > 0, "The height should be a positive number."))
          validate(need(input$lollipop_width > 0, "The width should be a positive number."))
          req(input$lollipop_x)
          req(input$lollipop_color)
          req(input$lollipop_size)
          req(input$lollipop_side)
          req(p_term_data())

          term_data <- p_term_data()

          if (input$lollipop_side == " ") {
            p <- ggplot(term_data, aes( x = reorder(.data[[input$lollipop_y]], .data[[input$lollipop_x]]),
                                        y = .data[[input$lollipop_x]],
                                        tooltip = .data[[input$lollipop_x]],
                                        data_id = term_id) )
          } else {
            sides <- unique(term_data[[input$lollipop_side]])
            x_limits <- c(-max(term_data[[input$lollipop_x]]), max(term_data[[input$lollipop_x]]))
            x_breaks <- pretty(x_limits)
            x_side <- ifelse(input$lollipop_side_rev, 2, 1)
            p <- ggplot(term_data, aes( x = reorder(term_id, .data[[input$lollipop_x]]),
                                        y = ifelse(.data[[input$lollipop_side]] == sides[x_side],
                                                   .data[[input$lollipop_x]],
                                                   -.data[[input$lollipop_x]]),
                                        data_id = term_id,
                                        tooltip = .data[[input$lollipop_x]]) )
          }

          p <- p +
            geom_bar(width = input$lollipop_line_width, stat = "identity", fill = input$lollipop_line_color)

          if(input$lollipop_color == " "){
            if(input$lollipop_size == " "){
              p <- p + geom_point_interactive(color = input$lollipop_point_color, size = input$lollipop_point_size)
            } else {
              p <- p + geom_point_interactive(aes(size = .data[[input$lollipop_size]]), color = input$lollipop_point_color)
            }
          } else {
            req(input$lollipop_point_palette)
            if(input$lollipop_size == " "){
              if(is.numeric(term_data[[input$lollipop_color]]) & input$lollipop_point_palette %in% names(getseqcolor())){
                p <- p + geom_point_interactive(aes(color = .data[[input$lollipop_color]]), size = input$lollipop_point_size) +
                  scale_color_gradientn(colors = getseqcolor()[[input$lollipop_point_palette]])
              } else if(is.character(term_data[[input$lollipop_color]]) &
                        input$lollipop_point_palette %in% names(getqualcolor(1))) {
                color_n <- length(unique(term_data[[input$lollipop_color]]))
                p <- p + geom_point_interactive(aes(color = .data[[input$lollipop_color]]), size = input$lollipop_point_size) +
                  scale_color_manual(values = getqualcolor(color_n)[[input$lollipop_point_palette]])
              }
            } else {
              if(is.numeric(term_data[[input$lollipop_color]]) & input$lollipop_point_palette %in% names(getseqcolor())){
                p <- p + geom_point_interactive(aes(color = .data[[input$lollipop_color]],
                                                    size = .data[[input$lollipop_size]])) +
                  scale_color_gradientn(colors = getseqcolor()[[input$lollipop_point_palette]])
              } else if(is.character(term_data[[input$lollipop_color]]) &
                        input$lollipop_point_palette %in% names(getqualcolor(1))) {
                color_n <- length(unique(term_data[[input$lollipop_color]]))
                p <- p + geom_point_interactive(aes(color = .data[[input$lollipop_color]],
                                                    size = .data[[input$lollipop_size]])) +
                  scale_color_manual(values = getqualcolor(color_n)[[input$lollipop_point_palette]])
              }
            }
          }

          if (input$lollipop_side == " ") {
            p <- p +
              scale_y_continuous(expand = expansion(mult = c(0, .05))) +
              theme(axis.text.y = element_text(size = input$lollipop_y_text_size))
          } else {
            p <- p +
              geom_text(aes(label = .data[[input$lollipop_y]],
                            y = ifelse(.data[[input$lollipop_side]] == sides[x_side],
                                                        x_limits[2] * -.01, x_limits[2] * .01),
                            hjust = ifelse(.data[[input$lollipop_side]] == sides[x_side], 1, 0)),
                        size = input$lollipop_y_text_size * 0.376) +
              scale_y_continuous(breaks = x_breaks,
                                 labels = abs(x_breaks),
                                 limits = x_limits) +
              theme(axis.text.y = element_blank())
          }

          p <- p +
            coord_flip() +
            labs(y = input$lollipop_x_title, color = input$lollipop_color_title, size = input$lollipop_size_title) +
            theme(axis.line.y = element_blank(),
                  axis.title.y = element_blank(),
                  axis.ticks.y = element_blank(),
                  axis.line.x = element_line(),
                  axis.text.x = element_text(size = input$lollipop_x_text_size),
                  axis.title.x = element_text(size = input$lollipop_x_title_size),
                  legend.position = input$lollipop_lg_pos,
                  legend.key = element_blank(),
                  legend.title = element_text(size = input$lollipop_lg_title_size),
                  legend.text = element_text(size = input$lollipop_lg_text_size),
                  panel.background = element_rect(fill = "white"),
                  panel.grid = element_blank())

          x <- girafe(ggobj = p, height_svg = input$lollipop_height/100, width_svg = input$lollipop_width/100)
          set_girafe(x)
        })


        ##### network

        net_link_data <- reactive({
          req(p_term_data())

          term_data <- p_term_data()
          order_index <- order(unlist(lapply(term_data$gene, function(x){
            length(unlist(strsplit(x, input$genes_split)))
          })), decreasing = T)
          term_data <- term_data[order_index, ][1:min(input$network_top_n, nrow(term_data)),]

          link_data <- as.data.frame(t(apply(combn(term_data$term_id, 2), 2, function(x){
            gene_list1 <- unlist(strsplit(subset(term_data, term_id == x[1])$gene, input$genes_split))
            gene_list2 <- unlist(strsplit(subset(term_data, term_id == x[2])$gene, input$genes_split))
            c(x[1], x[2], length(intersect(gene_list1, gene_list2)))
          })))

          link_data$V3 <- as.numeric(link_data$V3)

          link_data
        })

        observeEvent(p_term_data(), {
          term_data <- p_term_data()
          columns <- colnames(term_data)
          column_n <- columns[unlist(lapply(as.list(term_data),is.numeric))]
          updateSelectInput(session, "network_color", "Color",
                            structure(c(" ", columns), names = c("-", columns)))
          updateSelectInput(session, "network_size", "Size",
                            structure(c(" ", column_n), names = c("-", column_n)))

        })

        output$ui_network_palette <- renderUI({
          req(p_term_data())
          term_data <- p_term_data()
          req(input$network_color)

          if(input$network_color != " "){
            if(is.numeric(term_data[[input$network_color]])){
              esquisse::palettePicker("network_point_palette", "Point palette", getseqcolor())
            } else {
              color_n <- length(unique(term_data[[input$network_color]]))
              esquisse::palettePicker("network_point_palette", "Point palette", getqualcolor(color_n))
            }
          } else {
            esquisse::palettePicker("network_point_palette", "Point palette", NULL)
          }
        })

        observeEvent(input$network_color, {
          updateTextInput(session, "network_color_title", "Color title", input$network_color)
        })

        observeEvent(input$network_size, {
          updateTextInput(session, "network_size_title", "Size title", input$network_size)
        })

        output$ui_network <- renderUI({
          girafeOutput("network", height = input$network_height, width = input$network_width)
        })

        observe({
          shinyjs::toggleState(id = "network_point_color", condition = input$network_color == " ")
          shinyjs::toggleState(id = "network_point_palette", condition = input$network_color != " ")
          shinyjs::toggleState(id = "network_color_title", condition = input$network_color != " ")
          shinyjs::toggleState(id = "network_size_title", condition = input$network_size != " ")
          shinyjs::toggleState(id = "network_point_size", condition = input$network_size == " ")
        })

        output$network <- renderGirafe({

          req(p_term_data())
          validate(need(input$network_top_n > 1, "The 'Top N' should be greater than 1. "))

          term_data <- p_term_data()
          link_data <- net_link_data()

          if(sum(link_data$V3) > 0 ){
            link_data <- subset(link_data, V3 > 0)
            net_list <- igraph::graph_from_data_frame(link_data, directed=FALSE, vertices = term_data$term_id)
          } else {
            net_list <- NULL
          }

          validate(need(net_list, "There are no overlapping genes between the current functional terms."))

          net_data <- ggnetwork::ggnetwork(net_list)

          node_data <- merge(term_data,
            unique(net_data[,c("x", "y", "name")]), by.x = "term_id", by.y = "name")

          p <- ggplot(node_data,
                      aes(x = x, y = y)) +
            geom_segment(data = net_data, aes(x = x, y = y, xend = xend, yend = yend, alpha = V3),
                         size = input$network_line_width)

          if(input$network_color == " "){
            if(input$network_size == " "){
              p <- p + geom_point_interactive(aes(data_id = term_id, tooltip = term_id),
                                              color = input$network_point_color, size = input$network_point_size)
            } else {
              p <- p + geom_point_interactive(aes(size = .data[[input$network_size]], data_id = term_id, tooltip = term_id),
                                              color = input$network_point_color)
            }
          } else {
            req(input$network_point_palette)
            if(input$network_size == " "){
              if(is.numeric(term_data[[input$network_color]]) & input$network_point_palette %in% names(getseqcolor())){
                p <- p + geom_point_interactive(aes(color = .data[[input$network_color]], data_id = term_id, tooltip = term_id),
                                                size = input$network_point_size) +
                  scale_color_gradientn(colors = getseqcolor()[[input$network_point_palette]])
              } else if(is.character(term_data[[input$network_color]]) &
                        input$network_point_palette %in% names(getqualcolor(1))) {
                color_n <- length(unique(term_data[[input$network_color]]))
                p <- p + geom_point_interactive(aes(color = .data[[input$network_color]], data_id = term_id, tooltip = term_id),
                                                size = input$network_point_size) +
                  scale_color_manual(values = getqualcolor(color_n)[[input$network_point_palette]])
              }
            } else {
              if(is.numeric(term_data[[input$network_color]]) & input$network_point_palette %in% names(getseqcolor())){
                p <- p + geom_point_interactive(aes(color = .data[[input$network_color]], size = .data[[input$network_size]],
                                                    data_id = term_id, tooltip = term_id)) +
                  scale_color_gradientn(colors = getseqcolor()[[input$network_point_palette]])
              } else if(is.character(term_data[[input$network_color]]) &
                        input$network_point_palette %in% names(getqualcolor(1))) {
                color_n <- length(unique(term_data[[input$network_color]]))
                p <- p + geom_point_interactive(aes(color = .data[[input$network_color]], size = .data[[input$network_size]],
                                                    data_id = term_id, tooltip = term_id)) +
                  scale_color_manual(values = getqualcolor(color_n)[[input$network_point_palette]])
              }
            }
          }

          p <- p +
            labs(color = input$network_color_title, size = input$network_size_title, alpha = "Count") +
            theme(axis.title = element_blank(),
                  axis.text = element_blank(),
                  axis.ticks = element_blank(),
                  axis.line = element_blank(),
                  panel.background = element_blank(),
                  legend.position = input$network_lg_pos,
                  legend.title = element_text(size = input$network_lg_title_size),
                  legend.text = element_text(size = input$network_lg_text_size),
                  legend.key = element_blank())


          x <- girafe(ggobj = p, height_svg = input$network_height/100, width_svg = input$network_width/100)
          set_girafe(x)

        })

        ######  complex Barplot

        observeEvent(p_term_data(), {
          term_data <- p_term_data()
          gene_data <- p_gene_data()

          term_columns <- colnames(term_data)
          term_column_c <- term_columns[unlist(lapply(as.list(term_data),is.character))]
          term_column_c_term <- term_column_c[term_column_c %in% c("term_id", "term_name")]
          #term_column_c_term <-  term_column_c[apply(term_data[, term_column_c], 2, function(x){length(unique(x))}) == nrow(term_data)]
          updateSelectInput(session, "complexbar_y", "Y (select from columns of term_data)",
                            term_column_c_term)
          gene_columns <- colnames(gene_data)
          gene_column_n <- gene_columns[unlist(lapply(as.list(gene_data),is.numeric))]
          gene_column_c <- gene_columns[unlist(lapply(as.list(gene_data),is.character))]
          gene_column_c_group <- gene_column_c[apply(gene_data[, gene_column_c], 2, function(x){length(unique(x))}) < 20]
          updateSelectInput(session, "complexbar_color", "Color (select from columns of gene_data)",
                            structure(c(" ",  c(gene_column_n, gene_column_c_group)),
                                      names = c("-",  c(gene_column_n, gene_column_c_group))))
          updateSelectInput(session, "complexbar_order", "Order by (select from columns of gene_data)",
                            structure(c(" ",gene_column_n), names = c("-", gene_column_n)))
          updateSelectInput(session,"complexbar_split", "Split by (select from columns of gene_data)",
                            structure(c(" ",gene_column_c_group), names = c("-", gene_column_c_group)))

        })

        complexbar_data <- reactive({
          req(p_term_data())
          req(p_gene_data())
          term_data <- p_term_data()
          gene_data <- p_gene_data()
          validate(need(input$complexbar_topn > 0, ""))
          validate(need(input$complexbar_color != " ", ""))
          validate(need(input$complexbar_order != " ", ""))

          order_index <- order(unlist(lapply(term_data$gene, function(x){
            length(unlist(strsplit(x, input$genes_split)))
          })), decreasing = T)
          show_data <- term_data[order_index[1:min(nrow(term_data), input$complexbar_topn)], ]

          p_data <- do.call(rbind,lapply(1:nrow(show_data), function(x){
            genes <- unlist(strsplit(show_data[x, "gene"], input$genes_split))
            curr_data <- gene_data[match(genes, gene_data$gene), ]

            if(input$complexbar_split != " "){
              group_list <- unique(curr_data[[input$complexbar_split]])
              do.call(rbind, lapply(group_list, function(y){
                curr_group_data <- curr_data[curr_data[[input$complexbar_split]] == y, ]
                data.frame(
                  y = nrow(show_data) - x + 1,
                  term_id = show_data[x, "term_id"],
                  x = 1:nrow(curr_group_data),
                  group = y,
                  value = curr_group_data[order(curr_group_data[[input$complexbar_order]], decreasing = input$complexbar_decs),
                                          input$complexbar_color]
                )
              }))
            } else {
              data.frame(
                y = nrow(show_data) - x + 1,
                term_id = show_data[x, "term_id"],
                x = 1:length(genes),
                value = curr_data[order(curr_data[[input$complexbar_order]], decreasing = input$complexbar_decs), input$complexbar_color]
              )
            }
          }))
          list(show_data = show_data, p_data = p_data)
        })

        output$ui_complexbar_palette <- renderUI({
          req(complexbar_data())
          p_data <- complexbar_data()$p_data
          req(input$complexbar_color)

          if(input$complexbar_color != " "){
            if(is.numeric(p_data$value)){
              esquisse::palettePicker("complexbar_palette", "Palette", getseqcolor())
            } else {
              color_n <- length(unique(p_data$value))
              esquisse::palettePicker("complexbar_palette", "Palette", getqualcolor(color_n))
            }
          } else {
            esquisse::palettePicker("complexbar_palette", "Palette", NULL)
          }
        })

        observeEvent(input$complexbar_color, {
          updateTextInput(session, "complexbar_color_title", "Color title", input$complexbar_color)
        })

        output$ui_complexbar <- renderUI({
          girafeOutput("complexbar", height = input$complexbar_height, width = input$complexbar_width)
        })

        output$complexbar <- renderGirafe({
          validate(need(input$complexbar_color != " ", "Please select 'Color' in the left panel."))
          validate(need(input$complexbar_order != " ", "Please select 'Order' in the left panel."))
          validate(need(input$complexbar_topn > 0, ""))
          req(p_term_data())
          req(p_gene_data())
          req(complexbar_data())
          p_data <- complexbar_data()$p_data
          show_data <- complexbar_data()$show_data

          p <- ggplot(p_data, aes(data_id = term_id, tooltip = term_id))

          req(input$complexbar_palette)
          if(is.numeric(p_data$value) & input$complexbar_palette %in% names(getseqcolor())){
            p <- p + geom_tile_interactive(aes(x = x, y = y, fill = value, height = .7)) +
              scale_fill_gradientn(colors = getseqcolor()[[input$complexbar_palette]])
          } else if(is.character(p_data$value) & input$complexbar_palette %in% names(getqualcolor(1))) {
            color_n <- length(unique(p_data$value))
            p <- p + geom_tile_interactive(aes(x = x, y = y, fill = value, height = .7)) +
              scale_fill_manual(values = getqualcolor(color_n)[[input$complexbar_palette]])
          }

           p <- p +
            scale_x_continuous(expand = expansion(mult=c(0, .05))) +
            scale_y_continuous(breaks = 1:nrow(show_data), labels = rev(show_data[, input$complexbar_y])) +
            theme_classic() +
            labs(x = input$complexbar_x_title, fill = input$complexbar_color_title) +
          theme(axis.line.y = element_blank(),
                axis.ticks.y = element_blank(),
                axis.title.y = element_blank(),
                legend.position = input$complexbar_lg_pos,
                legend.title = element_text(size = input$complexbar_lg_title_size),
                legend.text = element_text(size = input$complexbar_lg_text_size),
                strip.text = element_text(size = input$complexbar_split_size),
                axis.text.x = element_text(size = input$complexbar_x_text_size),
                axis.text.y = element_text(size = input$complexbar_y_text_size),
                axis.title.x = element_text(size = input$complexbar_x_title_size))
          if(input$complexbar_split != " "){
            p <- p +
              facet_grid(.~group, scales = "free", space = "free") +
              theme(strip.background = element_blank())
          }

          x <- girafe(ggobj = p, height_svg = input$complexbar_height/100, width_svg = input$complexbar_width/100)
          set_girafe(x)
        })


        ##### heatmap

        observeEvent(p_term_data(), {
          term_data <- p_term_data()
          columns <- colnames(term_data)
          column_c <- columns[unlist(lapply(as.list(term_data), is.character))]
          column_n <- columns[unlist(lapply(as.list(term_data), is.numeric))]
          column_c_term <-  column_c[column_c %in% c("term_id", "term_name")]

          updateSelectInput(session, "heatmap_y", "Y", column_c_term)
          updateSelectInput(session, "heatmap_color", "Color", column_n)
        })

        output$ui_heatmap_palette <- renderUI({
          req(p_term_data())
          term_data <- p_term_data()
          req(input$heatmap_color)

          if(input$heatmap_color != " "){
            if(is.numeric(term_data[[input$heatmap_color]])){
              esquisse::palettePicker("heatmap_palette", "Point palette", getseqcolor())
            } else {
              color_n <- length(unique(term_data[[input$heatmap_color]]))
              esquisse::palettePicker("heatmap_palette", "Point palette", getqualcolor(color_n))
            }
          } else {
            esquisse::palettePicker("heatmap_palette", "Point palette", NULL)
          }
        })

        observeEvent(input$heatmap_color, {
          updateTextInput(session, "heatmap_color_title", "Color title", input$heatmap_color)
        })

        output$ui_heatmap <- renderUI({
          girafeOutput("heatmap", height = input$heatmap_height, width = input$heatmap_width)
        })

        output$heatmap <- renderGirafe({
          req(p_term_data())
          validate(need(input$heatmap_color, ""))
          term_data <- p_term_data()

          p <- ggplot(term_data, aes(group, .data[[input$heatmap_y]],
                                     tooltip = .data[[input$heatmap_color]], data_id = paste0(group, "^", term_id)))

          req(input$heatmap_palette)
          if(is.numeric(term_data[[input$heatmap_color]]) & input$heatmap_palette %in% names(getseqcolor())){
            p <- p + geom_tile_interactive(aes(fill = .data[[input$heatmap_color]]), color = "white") +
              scale_fill_gradientn(colors = getseqcolor()[[input$heatmap_palette]])
          } else if(is.character(term_data[[input$heatmap_color]]) &
                    input$heatmap_palette %in% names(getqualcolor(1))) {
            color_n <- length(unique(term_data[[input$heatmap_color]]))
            p <- p + geom_tile_interactive(aes(fill = .data[[input$heatmap_color]]), color = "white") +
              scale_fill_manual(values = getqualcolor(color_n)[[input$heatmap_palette]])
          }

          p <- p +
            theme_bw() +
            labs(fill = input$heatmap_color_title) +
            scale_x_discrete(expand = expansion(mult = 0)) +
            scale_y_discrete(expand = expansion(mult = 0)) +
            theme(axis.ticks = element_blank(),
                  panel.grid = element_blank(),
                  axis.title = element_blank(),
                  axis.text.x = element_text(size = input$heatmap_colname_size,
                                             angle = input$heatmap_colname_angle),
                  axis.text.y = element_text(size = input$heatmap_rowname_size),
                  legend.position = input$heatmap_lg_pos,
                  legend.title = element_text(size = input$heatmap_lg_title_size),
                  legend.text = element_text(size = input$heatmap_lg_text_size))

          x <- girafe(ggobj = p, height_svg = input$heatmap_height/100, width_svg = input$heatmap_width/100)
          set_girafe(x)

        })


        ##### dot plot

        observeEvent(p_term_data(), {
          term_data <- p_term_data()
          columns <- colnames(term_data)
          column_c <- columns[unlist(lapply(as.list(term_data), is.character))]
          column_n <- columns[unlist(lapply(as.list(term_data), is.numeric))]
          column_c_term <-  column_c[column_c %in% c("term_id", "term_name")]

          updateSelectInput(session, "dotplot_y", "Y", column_c_term)
          updateSelectInput(session, "dotplot_color", "Color", column_n)
          updateSelectInput(session, "dotplot_size", "Size", column_n)

        })

        output$ui_dotplot_palette <- renderUI({
          req(p_term_data())
          term_data <- p_term_data()
          req(input$dotplot_color)

          if(input$dotplot_color != " "){
            if(is.numeric(term_data[[input$dotplot_color]])){
              esquisse::palettePicker("dotplot_palette", "Point palette", getseqcolor())
            } else {
              color_n <- length(unique(term_data[[input$dotplot_color]]))
              esquisse::palettePicker("dotplot_palette", "Point palette", getqualcolor(color_n))
            }
          } else {
            esquisse::palettePicker("dotplot_palette", "Point palette", NULL)
          }
        })

        observeEvent(input$dotplot_color, {
          updateTextInput(session, "dotplot_color_title", "Color title", input$dotplot_color)
        })

        observeEvent(input$dotplot_size, {
          updateTextInput(session, "dotplot_size_title", "Size title", input$dotplot_size)
        })

        output$ui_dotplot <- renderUI({
          girafeOutput("dotplot", height = input$dotplot_height, width = input$dotplot_width)
        })

        output$dotplot <- renderGirafe({
          req(p_term_data())
          validate(need(input$dotplot_color, ""))
          validate(need(input$dotplot_size, ""))
          term_data <- p_term_data()

          p <- ggplot(term_data, aes(group, .data[[input$dotplot_y]],
                                     tooltip = paste0(input$dotplot_color, .data[[input$dotplot_color]],
                                                      "\n", input$dotplot_size, .data[[input$dotplot_size]]),
                                     data_id = paste0(group, "^", term_id)))

          req(input$dotplot_palette)
          if(is.numeric(term_data[[input$dotplot_color]]) & input$dotplot_palette %in% names(getseqcolor())){
            p <- p + geom_point_interactive(aes(color = .data[[input$dotplot_color]], size = .data[[input$dotplot_size]]))  +
              scale_color_gradientn(colors = getseqcolor()[[input$dotplot_palette]])
          } else if(is.character(term_data[[input$dotplot_color]]) &
                    input$dotplot_palette %in% names(getqualcolor(1))) {
            color_n <- length(unique(term_data[[input$dotplot_color]]))
            p <- p + geom_point_interactive(aes(color = .data[[input$dotplot_color]], size = .data[[input$dotplot_size]]))  +
              scale_color_manual(values = getqualcolor(color_n)[[input$dotplot_palette]])
          }

          p <- p +
            theme_bw() +
            labs(color = input$dotplot_color_title, size = input$dotplot_size_title) +
            theme(panel.grid = element_blank(),
                  axis.ticks = element_blank(),
                  axis.title = element_blank(),
              axis.text.x = element_text(
                size = input$dotplot_colname_size, angle = input$dotplot_colname_angle),
                  axis.text.y = element_text(size = input$dotplot_rowname_size),
              legend.position = input$dotplot_lg_pos,
              legend.title = element_text(size = input$dotplot_lg_title_size),
              legend.text = element_text(size = input$dotplot_lg_text_size))
          x <- girafe(ggobj = p, height_svg = input$dotplot_height/100, width_svg = input$dotplot_width/100)
          set_girafe(x)
        })

        ##############################################################  TERM

        v <- reactiveValues(group = FALSE, selected_term_id = NULL, selected_group = NULL)

        observeEvent(input$barplot_selected, {
          req(input$barplot_selected)
          v$selected_term_id <- input$barplot_selected
          v$group <- FALSE
        })

        observeEvent(input$bubble_selected, {
          req(input$bubble_selected)
          v$selected_term_id <- input$bubble_selected
          v$group <- FALSE
        })

        observeEvent(input$lollipop_selected, {
          req(input$lollipop_selected)
          v$selected_term_id <- input$lollipop_selected
          v$group <- FALSE
        })

        observeEvent(input$network_selected, {
          req(input$network_selected)
          v$selected_term_id <- input$network_selected
          v$group <- FALSE
        })

        observeEvent(input$complexbar_selected, {
          req(input$complexbar_selected)
          v$selected_term_id <- input$complexbar_selected
          v$group <- FALSE
        })

        observeEvent(input$heatmap_selected, {
          req(input$heatmap_selected)
          click <- unlist(strsplit(input$heatmap_selected, "\\^"))
          v$selected_term_id <- click[2]
          v$selected_group <- click[1]
          v$group <- TRUE
        })

        observeEvent(input$dotplot_selected, {
          req(input$dotplot_selected)
          click <- unlist(strsplit(input$dotplot_selected, "\\^"))
          v$selected_term_id <- click[2]
          v$selected_group <- click[1]
          v$group <- TRUE
        })

        click_term_data <- eventReactive(
          list(input$barplot_selected, input$bubble_selected, input$lollipop_selected, input$network_selected,
               input$complexbar_selected, input$heatmap_selected, input$dotplot_selected), {
          req(v$selected_term_id)

          term_data <- p_term_data()

          selected_term <- v$selected_term_id

          if(v$group){
            data <- subset(term_data, term_id == selected_term & group == v$selected_group)
          } else {
            data <- subset(term_data, term_id == selected_term)
          }

          data.frame(
            column = colnames(data),
            detail = as.vector(t(data)),
            row.names = colnames(data)
          )

        })

        output$term_detail <- renderTable({
          req(click_term_data())
          subset(click_term_data(), column != "gene")
        })


        termID <- reactive({
          req(v$selected_term_id)
          id <- v$selected_term_id
          if (setequal(grep("^GO:\\d{7}$", id), 1)){
            info <- list(source = "GO", id = id)
          }else if (setequal(grep("^[a-z]{2,4}\\d{5}$", id), 1)){
            info <- list(source = "KEGG", id = id)
          }else if (setequal(grep("^WP\\d{1,4}$", id), 1)){
            info <- list(source = "WP", id = id)
          }else if (setequal(grep("^K\\d{5}$", id), 1)){
            info <- list(source = "KO", id = id)
          }else{
            info <- NULL
          }
          return(info)
        })


        output$term_info <- DT::renderDataTable({

          if (is.null(termID())) return(NULL)
          source <- termID()$source
          id <- termID()$id
          if (setequal(source, "GO")) {
            data <- tryCatch({
              go_info <- t(as.data.frame(lapply(RJSONIO::fromJSON(
                RCurl::getURL(paste0("http://api.geneontology.org/api/ontology/term/", id))),
                function(x) { paste0(x, collapse = ", ")})))
              go_table <- data.frame(go_info)
              colnames(go_table) <- "description"
              go_table$type <- rownames(go_table)
              go_table <- go_table[-which(go_table$description == "" | go_table$type == "subsets"), c(2, 1)]
              rbind(go_table[, c("type", "description")],
                    c("more", HTML(paste0("<a href='http://amigo.geneontology.org/amigo/term/", id, "'>AmiGO2</a>"))))
            }, error = function(e) {
              return(NULL)
            })
          } else if (setequal(source, "KO")) {
            data <- tryCatch({
              content <- unlist(strsplit(RCurl::getURL(paste0("http://rest.kegg.jp/get/", id)), "\n"))
              content <- content[grep("^ENTRY|^NAME|^DESCRIPTION|^PATHWAY|^DBLINKS",content)]

              data.frame(
                type = c(regmatches(content, regexpr("ENTRY|NAME|DESCRIPTION|PATHWAY|DBLINKS",content)), "MORE"),
                description = c(unlist(lapply(content, function(x){unlist(strsplit(x, "ENTRY\\s+|NAME\\s+|DESCRIPTION\\s+|PATHWAY\\s+|DBLINKS\\s+"))[2]})),
                                HTML(paste0("<a href='https://www.kegg.jp/dbget-bin/www_bget?", id, "'>KEGG Pathway</a>")))
              )
            }, error = function(e) {
              return(NULL)
            })
          } else if (setequal(source, "KEGG")) {
            data <- tryCatch({
              content <- unlist(strsplit(RCurl::getURL(paste0("http://rest.kegg.jp/get/", id)), "\n"))
              content <- content[grep("^ENTRY|^NAME|^DESCRIPTION|^CLASS|^MODULE|^DBLINKS",content)]

              data.frame(
                type = c(regmatches(content, regexpr("ENTRY|NAME|DESCRIPTION|CLASS|MODULE|DBLINKS",content)), "MORE"),
                description = c(unlist(lapply(content, function(x){unlist(strsplit(x, "ENTRY\\s+|NAME\\s+|DESCRIPTION\\s+|CLASS\\s+|MODULE\\s+|DBLINKS\\s+"))[2]})),
                                     HTML(paste0("<a href='https://www.kegg.jp/dbget-bin/www_bget?", id, "'>KEGG Pathway</a>")))
              )
            }, error = function(e) {
              return(NULL)
            })
          } else if (setequal(source, "WP")) {
            data <- tryCatch({
              url <- paste0("http://webservice.wikipathways.org/getPathwayInfo?pwId=",id)
              wiki_infos <- XML::xmlToDataFrame(RCurl::getURL(url))
              if (wiki_infos[[3]] != '') {
                data.frame(type = c("WPID", "name", "species", "revision", "more"),
                           description = c(wiki_infos[1, "id"], wiki_infos[1, "name"],
                                           wiki_infos[1, "species"], wiki_infos[1, "revision"],
                                           HTML(paste0("<a href='", wiki_infos[[2]], "'>WikiPathways</a>"))))
              } else {
                return(NULL)
              }
            }, error = function(e) {
              return(NULL)
            })
          }
          DT::datatable(data, escape = FALSE, rownames = F, options = list(dom = 't'))
        })

        ############################################################## click data

        click_gene_data <- reactive({
          req(click_term_data())

          curr_term <- click_term_data()
          p_gene_data <- p_gene_data()
          genes <- unlist(strsplit(curr_term["gene", 2], input$genes_split))

          subset(p_gene_data, gene %in% genes)

        })

        output$gene_data <- DT::renderDataTable({
          req(click_gene_data())
          DT::datatable(click_gene_data(),
                        extensions = c("Scroller", "Buttons"),
                        rownames = FALSE,
                        selection = "none",
                        options = list(
                          deferRender = TRUE,
                          scrollX = TRUE,
                          scrollY = 400,
                          scroller = TRUE,
                          dom = 'Bfrtip',buttons="csv"
                        ))
        })

        ###########  GENE CHART

        output$ui_gene_param <- renderUI({
          req(p_gene_data())
          curr_gene_data <- p_gene_data()
          columns <- colnames(curr_gene_data)
          column_n <- columns[unlist(lapply(as.list(curr_gene_data),is.numeric))]
          column_c <- columns[unlist(lapply(as.list(curr_gene_data),is.character))]
          column_c_group <- column_c[apply(curr_gene_data[, column_c], 2, function(x){length(unique(x))}) < 20]
          column_c_gene <- column_c[apply(curr_gene_data[, column_c], 2, function(x){length(unique(x))}) == nrow(curr_gene_data)]
          shinyjs::hidden(tagList(
            selectInput("pie_labels", "Labels", column_c_group),
            selectInput("pie_values", "Values", c("-", column_n)),
            numericInput("pie_height", "Height", 500, min = 0),
            numericInput("pie_width", "Width", 800, min = 0),
            selectInput("hist_x", "X", column_n),
            selectInput("hist_color", "Color", c("-", column_c_group)),
            selectInput("hist_position", "Position", c("identity", "stack", "fill")),
            numericInput("hist_height", "Height", 500, min = 0),
            numericInput("hist_width", "Width", 800, min = 0),
            selectInput("density_x", "X", column_n),
            selectInput("density_color", "Color", c("-", column_c_group)),
            selectInput("density_position", "Position", c("identity", "stack", "fill")),
            numericInput("density_height", "Height", 500, min = 0),
            numericInput("density_width", "Width", 800, min = 0),
            selectInput("bar_x", "X", column_c_gene),
            selectInput("bar_y", "Y", column_n),
            numericInput("bar_height", "Height", 500, min = 0),
            numericInput("bar_width", "Width", 800, min = 0),
            selectInput("scatter_x", "X", columns),
            selectInput("scatter_y", "Y", columns),
            selectInput("scatter_color", "Color", columns),
            selectInput("scatter_size", "Size", column_n),
            numericInput("scatter_height", "Height", 500, min = 0),
            numericInput("scatter_width", "Width", 800, min = 0),
            selectInput("box_y", "Y", column_n),
            selectInput("box_x", "X", c("-", column_c_group)),
            numericInput("box_height", "Height", 500, min = 0),
            numericInput("box_width", "Width", 800, min = 0),
            selectInput("violin_x", "X", c("-", column_c_group)),
            selectInput("violin_y", "Y", column_n),
            numericInput("violin_height", "Height", 500, min = 0),
            numericInput("violin_width", "Width", 800, min = 0),
            selectInput("heat_cols", "Columns", column_n, multiple = T),
            selectInput("heat_row", "Row", column_c_gene),
            numericInput("heat_height", "Height", 500, min = 0),
            numericInput("heat_width", "Width", 800, min = 0),
            selectInput("dumbbell_x1", "X1", column_n),
            selectInput("dumbbell_x2", "X2", column_n),
            selectInput("dumbbell_y", "Y", column_c_gene),
            numericInput("dumbbell_height", "Height", 500, min = 0),
            numericInput("dumbbell_width", "Width", 800, min = 0)
          ))
        })

        observeEvent(input$gene_chart_type, {
          type <- input$gene_chart_type
          shinyjs::hide("pie_labels")
          shinyjs::hide("pie_values")
          shinyjs::hide("pie_height")
          shinyjs::hide("pie_width")
          shinyjs::hide("hist_x")
          shinyjs::hide("hist_color")
          shinyjs::hide("hist_position")
          shinyjs::hide("hist_height")
          shinyjs::hide("hist_width")
          shinyjs::hide("density_x")
          shinyjs::hide("density_color")
          shinyjs::hide("density_position")
          shinyjs::hide("density_height")
          shinyjs::hide("density_width")
          shinyjs::hide("bar_x")
          shinyjs::hide("bar_y")
          shinyjs::hide("bar_height")
          shinyjs::hide("bar_width")
          shinyjs::hide("scatter_x")
          shinyjs::hide("scatter_y")
          shinyjs::hide("scatter_color")
          shinyjs::hide("scatter_size")
          shinyjs::hide("scatter_height")
          shinyjs::hide("scatter_width")
          shinyjs::hide("box_x")
          shinyjs::hide("box_y")
          shinyjs::hide("box_height")
          shinyjs::hide("box_width")
          shinyjs::hide("violin_x")
          shinyjs::hide("violin_y")
          shinyjs::hide("violin_height")
          shinyjs::hide("violin_width")
          shinyjs::hide("heat_cols")
          shinyjs::hide("heat_row")
          shinyjs::hide("heat_height")
          shinyjs::hide("heat_width")
          shinyjs::hide("dumbbell_x1")
          shinyjs::hide("dumbbell_x2")
          shinyjs::hide("dumbbell_y")
          shinyjs::hide("dumbbell_height")
          shinyjs::hide("dumbbell_width")

          if(type == "pie"){
            shinyjs::show("pie_labels")
            shinyjs::show("pie_values")
            shinyjs::show("pie_height")
            shinyjs::show("pie_width")
          } else if (type == "hist") {
            shinyjs::show("hist_x")
            shinyjs::show("hist_color")
            shinyjs::show("hist_position")
            shinyjs::show("hist_height")
            shinyjs::show("hist_width")
          } else if (type == "density") {
            shinyjs::show("density_x")
            shinyjs::show("density_color")
            shinyjs::show("density_position")
            shinyjs::show("density_height")
            shinyjs::show("density_width")
          } else if (type == "bar") {
            shinyjs::show("bar_x")
            shinyjs::show("bar_y")
            shinyjs::show("bar_height")
            shinyjs::show("bar_width")
          } else if (type == "scatter") {
            shinyjs::show("scatter_x")
            shinyjs::show("scatter_y")
            shinyjs::show("scatter_color")
            shinyjs::show("scatter_size")
            shinyjs::show("scatter_height")
            shinyjs::show("scatter_width")
          } else if (type == "box") {
            shinyjs::show("box_x")
            shinyjs::show("box_y")
            shinyjs::show("box_height")
            shinyjs::show("box_width")
          } else if (type == "violin") {
            shinyjs::show("violin_x")
            shinyjs::show("violin_y")
            shinyjs::show("violin_height")
            shinyjs::show("violin_width")
          } else if (type == "heat") {
            shinyjs::show("heat_cols")
            shinyjs::show("heat_row")
            shinyjs::show("heat_height")
            shinyjs::show("heat_width")
          } else if (type == "dumbbell"){
            shinyjs::show("dumbbell_x1")
            shinyjs::show("dumbbell_x2")
            shinyjs::show("dumbbell_y")
            shinyjs::show("dumbbell_height")
            shinyjs::show("dumbbell_width")
          }
        })

        output$ui_gene_chart <- renderUI({
          req(input$gene_chart_type)
          type <- input$gene_chart_type
          if(type == "pie"){
            curr_height <- input$pie_height
            curr_width <- input$pie_width
          } else if (type == "density") {
            curr_height <- input$density_height
            curr_width <- input$density_width
          } else if (type == "hist"){
            curr_height <- input$hist_height
            curr_width <- input$hist_width
          } else if (type == "bar"){
            curr_height <- input$bar_height
            curr_width <- input$bar_width
          } else if (type == "scatter"){
            curr_height <- input$scatter_height
            curr_width <- input$scatter_width
          } else if (type == "violin"){
            curr_height <- input$violin_height
            curr_width <- input$violin_width
          } else if (type == "box"){
            curr_height <- input$box_height
            curr_width <- input$box_width
          } else if (type == "heat"){
            curr_height <- input$heat_height
            curr_width <- input$heat_width
          } else if (type == "dumbbell"){
            curr_height <- input$dumbbell_height
            curr_width <- input$dumbbell_width
          } else {
            curr_height <- "500px"
            curr_width <- "100%"
          }

          girafeOutput("gene_chart", height = curr_height, width = curr_width)
        })

        output$gene_chart <- renderGirafe({

          req(input$gene_chart_type)
          validate(need(nrow(click_gene_data()) > 0, ""))
          curr_data <- click_gene_data()
          type <- input$gene_chart_type

          x <- NULL

          if(type == "pie"){
            if(input$pie_values == "-"){
              p_data <- as.data.frame(table(curr_data[[input$pie_labels]]))
            } else {
              p_data <- data.frame(
                Var1 = unique(curr_data[[input$pie_labels]]),
                Freq = unlist(lapply(unique(curr_data[[input$pie_labels]]),
                              function(x){sum(curr_data[curr_data[[input$pie_labels]] == x, input$pie_values])}))
              )
            }
            p <- ggplot(p_data, aes(tooltip = Freq)) +
              geom_bar_interactive(aes(x = "a", fill = Var1, y = Freq), stat = "identity", position = "stack")+
              coord_polar(theta = 'y') +
              labs(fill = input$pie_labels) +
              theme(axis.ticks = element_blank(),
                                        axis.line = element_blank(),
                                        axis.title = element_blank(),
                                        axis.text = element_blank(),
                                        panel.background = element_blank())
            x <- girafe(ggobj = p, height_svg = input$pie_height/100, width_svg = input$pie_width/100)
          } else if (type == "density"){
            p <- ggplot(curr_data, aes(x = .data[[input$density_x]]))
            if(input$density_color == "-"){
              p <- p +
                geom_density_interactive(fill = "gray", color = "gray",
                                         alpha = .2, position = input$density_position)
            } else {
              p <- p +
                geom_density_interactive(aes(fill = .data[[input$density_color]], color = .data[[input$density_color]]),
                                         alpha = .2, position = input$density_position)
            }
            p <- p +
              theme_classic() +
              labs(y = "Density") +
              scale_x_continuous(expand = expansion(mult = c(0, .05))) +
              scale_y_continuous(expand = expansion(mult = c(0, .05)))

            x <- girafe(ggobj = p, height_svg = input$density_height/100, width_svg = input$density_width/100)
          } else if (type == "hist"){
            p <- ggplot(curr_data, aes(x = .data[[input$hist_x]]))
            if(input$hist_color == "-"){
              p <- p +
                geom_histogram_interactive(fill = "gray", color = "gray",
                                           alpha = .2, bins = 100, position = input$hist_position)
            } else {
              p <- p +
                geom_histogram_interactive(aes(fill = .data[[input$hist_color]], color = .data[[input$hist_color]]),
                                           alpha = .2, bins = 100, position = input$hist_position)
            }
            p <- p +
              theme_classic() +
              scale_x_continuous(expand = expansion(mult = c(0, .05))) +
              scale_y_continuous(expand = expansion(mult = c(0, .05)))

            x <- girafe(ggobj = p, height_svg = input$hist_height/100, width_svg = input$hist_width/100)
          } else if (type == "bar"){

              p <- ggplot(curr_data, aes( x = .data[[input$bar_x]],
                                          y = .data[[input$bar_y]],
                                          tooltip = .data[[input$bar_x]],
                                                data_id = .data[[input$bar_x]] ) ) +
              geom_bar_interactive(stat = "identity", fill = "gray") +
              theme_classic() +
              scale_y_continuous(expand = expansion(mult= c(0, .05))) +
              theme(axis.text.x = element_text(angle = 90))

            x <- girafe(ggobj = p, height_svg = input$bar_height/100, width_svg = input$bar_width/100)
          } else if (type == "scatter") {

            p <- ggplot(curr_data, aes(x = .data[[input$scatter_x]],
                                       y = .data[[input$scatter_y]],
                                       tooltip = gene, data_id = gene))

            if(input$scatter_color == " "){
              if(input$scatter_size == " "){
                p <- p + geom_point_interactive()
              } else {
                p <- p + geom_point_interactive(aes(size = .data[[input$scatter_size]]))
              }
            } else {
              if(input$scatter_size == " "){
                p <- p + geom_point_interactive(aes(color = .data[[input$scatter_color]]))
              } else {
                p <- p + geom_point_interactive(aes(color = .data[[input$scatter_color]],
                                                    size = .data[[input$scatter_size]]))
              }
            }

            p <- p +
              theme_bw() +
              theme(panel.grid = element_blank(),
                    panel.background = element_blank())

            x <- girafe(ggobj = p, height_svg = input$scatter_height/100, width_svg = input$scatter_width/100)

          } else if (type == "box") {

            if(input$box_x == "-"){
              p_data <- data.frame(group = "a", y = curr_data[[input$box_y]])
            } else {
              p_data <- data.frame(group = curr_data[[input$box_x]], y = curr_data[[input$box_y]])
            }

            curr_factor <- factor(p_data$group)
            p_data$x <- as.numeric(curr_factor)
            p_colors <- structure(scales::hue_pal()(length(levels(curr_factor))),
                                  names = levels(curr_factor))

            p <- ggplot(p_data, aes(y = y, color = group, fill = group, group = group, tooltip = y)) +
              geom_boxplot_interactive(
                data = subset(p_data, group %in% names(table(p_data$group)[table(p_data$group) > 1])),
                aes(x = x - .15), width = .4, alpha = .5) +
              geom_jitter_interactive(aes(x = x + .25), width = .1) +
              scale_x_continuous(breaks = unique(p_data$x),
                                 labels = unique(curr_factor)[unique(p_data$x)]) +
              scale_color_manual(values = p_colors) +
              scale_fill_manual(values = p_colors) +
              labs(x = ifelse(input$box_x == "-", "", input$box_x),
                   y = input$box_y) +
              theme_classic() +
              theme(legend.position = "none")
            if(input$box_x == "-"){
              p <- p + theme(axis.title.x = element_blank(),
                             axis.text.x = element_blank(),
                             axis.line.x = element_blank())
            }

            x <- girafe(ggobj = p, height_svg = input$box_height/100, width_svg = input$box_width/100)
          } else if (type == "violin") {

            if(input$violin_x == "-"){
              p_data <- data.frame(loc = density(curr_data[[input$violin_y]])$x, den = density(curr_data[[input$violin_y]])$y)
              p_data$den <- p_data$den / max(p_data$den) /2
              p_data <- subset(p_data, loc >= min(curr_data[[input$violin_y]]) & loc <= max(curr_data[[input$violin_y]]))
              p_data <- rbind(p_data,
                              c(max(curr_data[[input$violin_y]]), 0),
                              c(min(curr_data[[input$violin_y]]), 0))
              violin_color <- "gray"
              p <- ggplot() +
                geom_polygon(data = p_data, aes(x = -den , y = loc), alpha = .5,
                             fill = violin_color, color = violin_color) +
                geom_jitter_interactive(data = curr_data, aes(x = .25, y = .data[[input$violin_y]], tooltip = gene),
                                        width = .125, color = violin_color) +
                scale_x_continuous(limit = c(-.5, .5)) +
                theme_classic() +
                theme(axis.text.x = element_blank(),
                      axis.line.x = element_blank(),
                      axis.ticks.x = element_blank(),
                      axis.title.x = element_blank())

            } else {

              new_data <- curr_data[, c(input$violin_x, input$violin_y, "gene")]
              colnames(new_data) <- c("group", "y", "gene")
              curr_factor <- factor(new_data$group)
              new_data$x <- as.numeric(curr_factor)
              p_colors <- structure(scales::hue_pal()(length(levels(curr_factor))),
                                    names = levels(curr_factor))

              group_list <- levels(factor(new_data$group))
              p_data <- do.call(rbind, lapply(1:length(group_list), function(x){
                curr_group <- group_list[x]
                curr_x_data <- subset(new_data, group == curr_group)
                if(nrow(curr_x_data) == 1) return(NULL)
                den_data <- data.frame(loc = density(curr_x_data$y)$x, den = density(curr_x_data$y)$y)
                den_data <- subset(den_data, loc >= min(curr_x_data$y) & loc <= max(curr_x_data$y))
                data.frame(loc = c(den_data$loc, rev(range(curr_x_data$y))),
                           den = c(den_data$den / max(den_data$den) / 2, 0, 0),
                           group = group_list[x],
                           x = x)
              }))
              p <- ggplot(new_data) +
                geom_polygon(data = p_data, aes(x = -den + x, y = loc, fill = group, color = group), alpha = .5) +
                geom_jitter_interactive(data = new_data, aes(x = x + .25, y = y, color = group, tooltip = gene), width = .125) +
                scale_x_continuous(breaks = 1:length(group_list), labels = group_list) +
                scale_color_manual(values = p_colors) +
                scale_fill_manual(values = p_colors) +
                labs(x = input$violin_x, y = input$violin_y) +
                theme_classic() + theme(legend.position = "none")

            }

            x <- girafe(ggobj = p, height_svg = input$violin_height/100, width_svg = input$violin_width/100)

          } else if (type == "heat") {

            validate(need(length(input$heat_cols) > 1, "Please choose at least two columns."))

            cluster_data <- curr_data[, input$heat_cols]
            rownames(cluster_data) <- curr_data[, input$heat_row]

            row_index <- hclust(dist(cluster_data))$order
            column_index <-  hclust(dist(t(cluster_data)))$order

            p_data <- reshape2::melt(curr_data[, c(input$heat_row, input$heat_cols)], id.var = c(input$heat_row))
            p_data[[1]] <- factor(p_data[[1]], levels = rownames(cluster_data)[row_index])
            p_data[[2]] <- factor(p_data[[2]], levels = colnames(cluster_data)[column_index])

            p <- ggplot(p_data) +
              geom_tile_interactive(aes(x = variable,
                            y = .data[[input$heat_row]], fill = value, tooltip = value)) +
              scale_x_discrete(expand = expansion(mult = 0)) +
              scale_y_discrete(expand = expansion(mult = 0)) +
              theme(panel.background = element_blank(),
                    axis.ticks = element_blank(),
                    axis.title = element_blank(),
                    panel.grid = element_blank())

            x <- girafe(ggobj = p, height_svg = input$heat_height/100, width_svg = input$heat_width/100)
          } else if (type == "dumbbell"){

            p <- ggplot(curr_data) +
              geom_segment(aes(x = .data[[input$dumbbell_x1]], xend = .data[[input$dumbbell_x2]],
                               y = .data[[input$dumbbell_y]], yend = .data[[input$dumbbell_y]]), color = "gray") +
              geom_point(aes(x = .data[[input$dumbbell_x1]], y = .data[[input$dumbbell_y]], color = input$dumbbell_x1)) +
              geom_point(aes(x = .data[[input$dumbbell_x2]], y = .data[[input$dumbbell_y]], color = input$dumbbell_x2)) +
              theme_bw() +
              labs(x = "", color = "") +
              theme(panel.grid = element_blank())

            x <- girafe(ggobj = p, height_svg = input$dumbbell_height/100, width_svg = input$dumbbell_width/100)

          }

          if(!is.null(x)){
            set_girafe(x)
          } else {
            NULL
          }

        })
      },
      options = list(launch.browser = launch.browser)
    )

  }
}




