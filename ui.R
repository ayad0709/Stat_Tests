# ui.R
library(shiny)
library(shinydashboard)
library(shinyWidgets)
library(shinyjs)
library(shinycssloaders)
library(ggplot2)

shinyUI(
  dashboardPage(
    skin = "purple",
    dashboardHeader(
      title = tags$span("Analyse statistique académique", style = "font-weight:600;")
    ),
    dashboardSidebar(
      width = 330,
      tags$style(HTML("
        .sidebar { padding-bottom: 20px; }
        .sidebar-title {
          font-size: 14px; font-weight: 700; letter-spacing: .2px;
          margin: 10px 0 6px 0; color: #fff; opacity: .95;
        }
        .sidebar-subtitle {
          font-size: 12px; color: #fff; opacity: .75; margin-bottom: 10px;
        }
        .sidebar .help-block { color: rgba(255,255,255,.8); font-size: 12px; }
        .sidebar hr { border-top: 1px solid rgba(255,255,255,.15); margin: 10px 0; }
      ")),
      tags$div(class = "sidebar-title", icon("sliders-h"), " Contrôles globaux"),
      tags$div(class = "sidebar-subtitle", "Importer, sélectionner, paramétrer"),
      
      radioButtons(
        inputId = "data_source",
        label   = "Source des données",
        choices = c("Importer un fichier" = "upload", "Données de démonstration (datasets)" = "demo"),
        selected = "upload"
      ),
      
      conditionalPanel(
        condition = "input.data_source == 'upload'",
        fileInput(
          inputId = "file",
          label = "Importer un fichier (.csv, .xlsx)",
          accept = c(".csv", ".xlsx")
        )
      ),
      
      conditionalPanel(
        condition = "input.data_source == 'demo'",
        uiOutput("demo_dataset_ui")
      ),
      
      helpText("Importez un fichier CSV ou Excel, ou choisissez un jeu de données de démonstration."),
      uiOutput("sheet_ui"),
      hr(),
      uiOutput("vars_ui"),
      hr(),
      switchInput(
        inputId = "mode_modele",
        label = "interprétation +longue",
        value = FALSE,
        onLabel = "Oui",
        offLabel = "Non"
      ),
      helpText("Mode thèse/mémoire : interprétation plus longue et formulation plus académique.")
    ),
    dashboardBody(
      shinyjs::useShinyjs(),
      withMathJax(),
      tags$head(
        tags$style(HTML("
          .content-wrapper { background: #f5f7fb; }
          .box { border-radius: 10px; box-shadow: 0 2px 10px rgba(0,0,0,.05); }
          .box .box-header { border-top-left-radius: 10px; border-top-right-radius: 10px; }
          .small-note { color: #555; font-size: 12px; }
          .kv-table td { padding: 6px 10px; vertical-align: top; }
          .tests-table { width: 100%; }
          .tests-table th, .tests-table td { vertical-align: top; }
          .tests-table td { padding: 9px 8px; }
          .tests-table th { padding: 9px 8px; }
          .btn-xs { padding: 2px 8px; font-size: 12px; line-height: 1.5; border-radius: 4px; }
          .topnav-tabs .nav {
            position: sticky;
            top: 50px;
            z-index: 1000;
            background: rgba(245,247,251,.98);
            padding: 10px 8px 8px 8px;
            border-bottom: 1px solid #e6e9f2;
            backdrop-filter: blur(6px);
          }
          .topnav-tabs .nav > li > a {
            border-radius: 999px !important;
            padding: 8px 14px !important;
            font-weight: 700;
          }
          .topnav-tabs .nav > li.active > a,
          .topnav-tabs .nav > li.active > a:hover {
            background: #605ca8 !important;
            border-color: #605ca8 !important;
            color: #fff !important;
          }
          .topnav-tabs .tab-content { padding-top: 14px; }
          .pill-icon { margin-right: 8px; }

          .params-grid { display: grid; grid-template-columns: 1fr; gap: 10px; }
          .params-actions {
            display: flex;
            gap: 10px;
            align-items: center;
            justify-content: space-between;
            flex-wrap: wrap;
            padding: 10px 12px;
            background: #ffffff;
            border: 1px solid #e6e9f2;
            border-radius: 12px;
            box-shadow: 0 2px 10px rgba(0,0,0,.04);
            margin-top: 10px;
          }
          .params-actions .btn { font-weight: 800; }
          .params-actions .runhint { color: #666; font-size: 12px; margin: 0; }

          /* Égaliser la hauteur de deux box dans la même ligne */
          .equal-height-row { display: flex; flex-wrap: wrap; }
          .equal-height-row > .col-sm-7,
          .equal-height-row > .col-sm-5 { display: flex; }
          .equal-height-row .equal-height-box {
            width: 100%;
            display: flex;
            flex-direction: column;
          }
          .equal-height-row .equal-height-box .box-body { flex: 1 1 auto; }

          /* Bandeau résultat */
          .result-banner {
            display: flex;
            align-items: center;
            gap: 12px;
            padding: 12px 14px;
            margin: 0 0 12px 0;
            border-radius: 12px;
            color: #fff;
            font-weight: 400;
            letter-spacing: 0;
            font-size: 18px;
            line-height: 1.25;
          }
          .result-banner .result-banner-sub {
            font-weight: 400;
            opacity: .95;
            font-size: 18px;
          }
          .result-banner-icon {
            width: 38px;
            height: 38px;
            border-radius: 12px;
            display: inline-flex;
            align-items: center;
            justify-content: center;
            background: rgba(255,255,255,.18);
          }
          .result-banner-success { background: #00a65a; }
          .result-banner-warning { background: #f39c12; }
          .result-banner-danger  { background: #dd4b39; }

          /* Hypothèses colorées selon décision */
          .hypo-pill {
            display: inline-block;
            padding: 2px 10px;
            border-radius: 999px;
            font-size: 13px;
            margin-right: 8px;
            background: rgba(0,0,0,.08);
          }
          .hypo-green { color: #00a65a; }
          .hypo-red   { color: #dd4b39; }
          .hypo-muted { color: #6c757d; }
          .hypo-block {
            border-left: 4px solid rgba(0,0,0,.08);
            padding-left: 10px;
            margin: 8px 0;
          }

          /* ==============================
             Demo collapsible sections
             ============================== */
          .demo-details {
            background: #fff;
            border: 1px solid #e6e9f2;
            border-radius: 12px;
            box-shadow: 0 2px 10px rgba(0,0,0,.04);
            margin: 10px 0;
            overflow: hidden;
          }
          .demo-details summary {
            list-style: none;
            cursor: pointer;
            padding: 10px 12px;
            font-weight: 800;
            display: flex;
            align-items: center;
            justify-content: space-between;
            user-select: none;
          }
          .demo-details summary::-webkit-details-marker { display: none; }
          .demo-details .demo-sum-left {
            display: inline-flex;
            align-items: center;
            gap: 10px;
          }
          .demo-details .demo-chip {
            display: inline-block;
            padding: 2px 10px;
            border-radius: 999px;
            font-size: 12px;
            font-weight: 800;
            background: rgba(0,0,0,.06);
          }
          .demo-details .demo-chevron {
            transition: transform .15s ease;
            opacity: .9;
          }
          .demo-details[open] .demo-chevron { transform: rotate(180deg); }
          .demo-details .demo-body {
            padding: 10px 12px 12px 12px;
            border-top: 1px solid #eef1f7;
          }
          .demo-q summary { background: rgba(243, 156, 18, .12); }
          .demo-p summary { background: rgba(96, 92, 168, .10); }
          .demo-a summary { background: rgba(0, 166, 90, .10); }
          .demo-details summary:hover { filter: brightness(0.98); }

          /* ==============================
             Move box collapse control LEFT
             + Add padding so it doesn't overlap title
             ============================== */
          .box .box-header .box-tools {
            float: left !important;
            right: auto !important;
            left: 10px !important;
          }
          .box .box-header .box-title {
            padding-left: 30px; /* space for +/- icon */
          }
          .box .box-header { cursor: pointer; }
        ")),
        tags$script(HTML("
          function shinyMathJaxTypeset() {
            if (window.MathJax) {
              if (window.MathJax.Hub && window.MathJax.Hub.Queue) {
                window.MathJax.Hub.Queue(['Typeset', window.MathJax.Hub]);
              } else if (window.MathJax.typesetPromise) {
                window.MathJax.typesetPromise();
              }
            }
          }
          $(document).on('shiny:connected', function(){ shinyMathJaxTypeset(); });
          $(document).on('shiny:value', function(){ shinyMathJaxTypeset(); });
          $(document).on('shown.bs.tab', 'a[data-toggle=\"tab\"]', function(){ shinyMathJaxTypeset(); });

          // Toggle collapse when clicking on the box header (except interactive elements)
          $(document).on('click', '.box .box-header', function(e) {
            if ($(e.target).is('a, button, input, select, textarea, i')) return;
            var $box = $(this).closest('.box');
            var $btn = $box.find('.box-tools [data-widget=\"collapse\"]');
            if ($btn.length) $btn.click();
          });
        "))
      ),
      div(
        class = "topnav-tabs",
        tabsetPanel(
          id = "top_nav",
          type = "pills",
          
          tabPanel(
            title = tagList(tags$span(class = "pill-icon", icon("table")), "Données brutes"),
            value = "raw",
            fluidRow(
              box(
                width = 12, status = "info", solidHeader = TRUE,
                title = tagList(icon("info-circle"), " Résumé des données"),
                collapsible = TRUE,
                collapsed = TRUE,
                tags$p(class = "small-note", "Aperçu et typage automatique. (Affichage limité aux 100 premières lignes.)"),
                withSpinner(uiOutput("data_info_ui"))
              )
            ),
            fluidRow(
              box(
                width = 12, status = "primary", solidHeader = TRUE,
                title = tagList(icon("th"), " Données (aperçu)"),
                withSpinner(tableOutput("raw_table"))
              )
            )
          ),
          
          tabPanel(
            title = tagList(tags$span(class = "pill-icon", icon("chart-bar")), "Statistiques"),
            value = "stats",
            fluidRow(
              box(
                width = 12, status = "info", solidHeader = TRUE,
                title = tagList(icon("info-circle"), " Informations sur les données"),
                withSpinner(uiOutput("demo_info_ui"))
                # withSpinner(uiOutput("data_stats_overview_ui"))
              )
            ),
            fluidRow(
              box(
                width = 12, status = "primary", solidHeader = TRUE,
                title = tagList(icon("calculator"), " Statistiques descriptives"),
                withSpinner(uiOutput("data_stats_ui"))
              )
            )
          ),
          
          tabPanel(
            title = tagList(tags$span(class = "pill-icon", icon("list")), "Tests proposés"),
            value = "proposed",
            fluidRow(
              box(
                width = 12, status = "info", solidHeader = TRUE,
                title = tagList(icon("lightbulb"), " Proposition de tests (par catégories)"),
                tags$p(class = "small-note", "Filtrez, explorez, puis cliquez « Exécuter » sur un test pour lancer directement l’analyse dans « Analyse & résultats »."),
                withSpinner(uiOutput("proposed_ui"))
              )
            )
          ),
          
          tabPanel(
            title = tagList(tags$span(class = "pill-icon", icon("chart-line")), "Analyse & résultats"),
            value = "results",
            
            fluidRow(
              class = "equal-height-row",
              box(
                width = 7, status = "info", solidHeader = TRUE,
                class = "equal-height-box",
                title = tagList(icon("check-circle"), " Choix du test"),
                withSpinner(uiOutput("analysis_test_select_ui")),
                withSpinner(uiOutput("analysis_selected_info_ui")),
                withSpinner(uiOutput("hypotheses_box_ui"))
              ),
              box(
                width = 5, status = "info", solidHeader = TRUE,
                class = "equal-height-box",
                title = tagList(icon("sliders-h"), " Paramètres & exécution"),
                div(
                  class = "params-grid",
                  withSpinner(uiOutput("test_params_ui")),
                  div(
                    class = "params-actions",
                    actionButton(
                      inputId = "run_test",
                      label = "Lancer l’analyse",
                      icon = icon("play"),
                      class = "btn-primary"
                    ),
                    tags$p(
                      class = "runhint",
                      icon("info-circle"),
                      "Exécution uniquement via ce bouton. NA supprimés uniquement sur les variables du test."
                    )
                  )
                )
              )
            ),
            
            fluidRow(
              box(
                width = 12, status = "primary", solidHeader = TRUE,
                title = tagList(icon("table"), " RÉSULTAT"),
                uiOutput("result_test_name_ui"),
                withSpinner(tableOutput("result_table")),
                withSpinner(uiOutput("posthoc_ui")),
                withSpinner(plotOutput("result_plot", height = "360px")),
                withSpinner(plotOutput("result_plot2", height = "360px"))
              )
            ),
            
            fluidRow(
              box(
                width = 12, status = "warning", solidHeader = TRUE,
                title = tagList(icon("graduation-cap"), " Interprétation académique"),
                withSpinner(uiOutput("interpretation_ui"))
              )
            )
          )
        )
      )
    )
  )
)