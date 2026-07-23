##************************##
##  Haplofun: UI function ##
##************************##
## Project: haplofun
## Last modification 06.2026
## Creation: 2023
library(DT)
library(shinyBS) 
library(shinyjs)

######------  ui function ------###### 
ui <- dashboardPage(
  dashboardHeader(title = "Haplofun"),
  
  ######------  side panel ------###### 
  dashboardSidebar(
    
    ####---------------------------  tags ---------------------------####
    tags$head(
      tags$style(HTML("
        .main-header .navbar {
            background-color: #2c3e50 !important; 
        }

        .main-header .logo {
            background-color: #2c3e50 !important; 
            border-bottom: 0 solid transparent;
            color: #ffffff !important; 
            font-weight: bold;
        }

        .main-header .sidebar-toggle,
        .main-header .dropdown-menu a {
            color: #ffffff; 
        }

        .main-sidebar {
            font-size: 14px;
            background-color: #2c3e50 !important; 
        }

        .sidebar-menu li a {
            padding: 10px 15px; 
            border-left: 3px solid transparent;
        }
        
        .sidebar-menu li.active a {
            color: #ffffff;
            background-color: #3498db !important; 
            border-left-color: #f39c12; 
        }
        

        .sidebar .h4-control-group {
            color: #ecf0f1; 
            font-size: 1.1em;
            font-weight: 600;
            padding: 10px 15px 5px 15px;
            margin-top: 10px;
            border-bottom: 1px solid rgba(255, 255, 255, 0.1); 
        }

        .sidebar .form-group {
            margin-bottom: 5px; 
        }

        .sidebar .control-label {
            color: #bdc3c7;
            font-weight: normal;
        }
        
        .sidebar hr {
            color: #ffffff;
            border-top: 1px solid #444;
            margin: 15px 0;
        }

        .sidebar .shiny-input-container {
            padding: 0 15px;
        }
    "))
    ),
    uiOutput("logout_ui"),
    ####---------------------------------------------------------------------####
    
    ####---------------------------  sidebar menu ---------------------------####
    sidebarMenu(
      id = "sidebarTabs",
      menuItem("Data disclaimer", tabName = "home", icon = icon("info-circle")),
      menuItem("Data input", tabName = "dataInput", icon = icon("upload")),
      menuItem("Haplotype analysis", tabName = "haplotypeAnalysis", icon = icon("dna")),
      menuItem("User guide & documentation", tabName = "help", icon = icon("info-circle"))
    ),
    hr(),
    ####---------------------------------------------------------------------####
    
    ####---------------------------  badges ---------------------------####
    tags$div(
      style = "text-align: left; margin-bottom: 5px;",
      tags$a(
        href = "https://github.com/TimHasenbein/Haplofun",
        target = "_blank",
        tags$img(src = "https://img.shields.io/badge/github-repo-blue?logo=github", style = "margin: 10px;")
      ),
      tags$br(),
      tags$img(src = "https://img.shields.io/badge/version-v1.0.0-blue", style = "margin: 10px;")
    ),
    hr(),
    ####---------------------------------------------------------------------####
    
    ####---------------------------  data input side menu ---------------------------####
    conditionalPanel(
      condition = "input.sidebarTabs == 'dataInput'",
      h4("Overview controls"),
      checkboxInput(inputId = "textboxShowMeta",label = "Show VCF headerdata",value = T),
      checkboxInput(inputId = "textboxShowData",label = "Show VCF datalines",value = T)
    ),
    ####---------------------------------------------------------------------####
    
    ####---------------------------  haplotypes side menu ---------------------------####
    conditionalPanel(
      condition = "input.sidebarTabs == 'haplotypeAnalysis'",
      
      # general haplotype controls
      h4("Haplotype parameters"),
      sliderInput(
        inputId = "sliderHaplotypes",
        label = "Number of top haplogroups:",
        min = 1,
        max = 50,
        value = 10,
        step = 1),
      uiOutput("percentage"),
      conditionalPanel(
        condition = "output.isMetadataAvailable",
        checkboxInput(
          inputId = "checkboxMeta",
          label = "Meta-data coloring",
          value = TRUE
        )
      ),
      hr(),
      
      # sub-section: Distance matrix
      conditionalPanel(
        condition = "input.compactPlotSubTabs == 'heatmapVis'",
        h4("Distance matrix controls"),
        selectInput(
          inputId = "selectDistanceMatrix",
          label = "Visualization type",
          choices = list(
            "No Plot" = 1,
            "As Matrix" = 2,
            "As Heatmap" = 3
          ),
          selected = 3
        ),
        checkboxInput(
          inputId = "checkboxCluster",
          label = "Hierarchical clustering",
          value = TRUE
        ),
        conditionalPanel(
          condition = "input.checkboxCluster",
          radioButtons(
            inputId = "radioButtonClusterHeatmap",
            label = "Clustering method",
            choices = list(
              "ward.D" = "ward.D",
              "single" = "single",
              "complete" = "complete",
              "average" = "average",
              "mcquitty" = "mcquitty",
              "median" = "median",
              "centroid" = "centroid"
            )
          )
        ),
        hr(),
      ),
      
      # sub-section: Dendrogram
      conditionalPanel(
        condition = "input.compactPlotSubTabs == 'dendrogramVis'",
        h4("Dendrogram controls"),
        radioButtons(
          inputId = "radioButtonDendrogram",
          label = "Agglomeration method",
          choices = list(
            "ward.D" = "ward.D",
            "single" = "single",
            "complete" = "complete",
            "average" = "average",
            "mcquitty" = "mcquitty",
            "median" = "median",
            "centroid" = "centroid"
          )
        ),
        hr(),
      ),
      
      # sub-section: Network 
      conditionalPanel(
        condition = "input.compactPlotSubTabs == 'networkVis'",
        h4("Haplotype network controls"),
        selectInput(
          inputId = "selectNetwork",
          label = "Network model",
          choices = list(
            "No Plot" = 1,
            "Haplonet" = 2,
            "MSN" = 3,
            "RMST" = 6,
            "MST" = 7
          ),
          selected = 2
        ),
        selectizeInput("snpPosition", "Select SNP for coloring:", choices = NULL),
        selectizeInput(
          inputId = "highlightHaplotypes",
          label = "Select haplogroups to highlight:",
          choices = NULL,
          selected = NULL,
          multiple = TRUE
        ),
        checkboxInput(
          inputId = "checkboxScaleNetwork",
          label = "Scale nodes to frequency"
        ),
        checkboxInput(
          inputId = "checkboxFastPlotHaplonet",
          label = "Use fast plotting option"
        ),
        sliderInput(
          inputId = "sizeNetwork",
          label = "Plot size",
          min = 250,
          max = 1500,
          value = 750
        ),
        sliderInput(
          inputId = "sliderScaleNetwork",
          label = "Node size",
          min = 1,
          max = 20,
          value = 5
        ),
        sliderInput(
          inputId = "sliderLabels",
          label = "Node label size",
          min = 0,
          max = 2,
          step = 0.1,
          value = 0.7
        ),
        sliderInput(
          inputId = "sliderThreshold",
          label = "Threshold for additional edges",
          min = 0,
          max = 10,
          value = 0
        ),
        fluidRow(
          column(
            6,
            radioButtons(
              inputId = "radioButtonEdges",
              label = "Edge weights",
              choices = list(
                "Don't show" = 0,
                "As lines" = 1,
                "As Dots" = 2,
                "As Numbers" = 3
              ),
              selected = 0
            )
          ),
          column(
            6,
            radioButtons(
              inputId = "colorCircles",
              label = "Node color",
              choices = list(
                "Gray" = "grey",
                "Blue" = "#007BFF",
                "Green" = "#1E8449",
                "Red" = "#C23B22"
              ),
              selected = "grey" 
            )
          )
        ),
        hr(),
      ), 
      ####---------------------------------------------------------------------####
      
      ####---------------------------  functional annotation side menu ---------------------------####
      # GWAS controls
      
      conditionalPanel(
        condition = "input.sidebarTabs == 'haplotypeAnalysis' && output.gwasDataSourceAvailable && input.annotation_tabs == 'gwas_tab'",
        h4("GWAS Controls"),
        checkboxInput(
          inputId = "checkboxGwasCluster",
          label = "Cluster GWAS allele mapping",
          value = TRUE
        ),
        checkboxInput(
          inputId = "checkboxGwasNetwork",
          label = "Color network for selected traits"
        ),
        selectizeInput("traitFilter", "", choices = NULL, multiple = TRUE),
        uiOutput("gwas_pval_slider"),
        actionButton(
          inputId = "applyTraitFilter",
          label = "Update GWAS annotation",
          icon = icon("sync")),
        hr(),
      ),
      
      # QTL controls
      conditionalPanel(
        condition = "input.sidebarTabs == 'haplotypeAnalysis'  && output.eQTLFileUploaded && input.annotation_tabs == 'qtl_tab'",
        h4("QTL controls"),
        checkboxInput(
          inputId = "checkboxQtlCluster",
          label = "Cluster QTL allele mapping",
          value = TRUE
        ),
        checkboxInput(
          inputId = "checkboxQtlNetwork",
          label = "Color network for selected QTLs"
        ),
        selectizeInput("qtlFilter", "", choices = NULL, multiple = TRUE),
        sliderInput(
          inputId = "pip_range",
          label = "PIP filter:",
          min = 0,
          max = 1,
          value = c(0)
        ),
        actionButton(
          inputId = "applyQTLFilter",
          label = "Update QTL annotation",
          icon = icon("sync")),
        hr(),
      ),
      
      # Annotation controls
      conditionalPanel(
        condition = "input.sidebarTabs == 'haplotypeAnalysis'  && output.annoFileUploaded && input.annotation_tabs == 'anno_tab'",
        h4("Annotation controls"),
        checkboxInput(
          inputId = "checkboxAnnoCluster",
          label = "Cluster allele mapping",
          value = TRUE
        ),
        selectInput("annoFilter", "Select Element:", choices = NULL),
        actionButton(
          inputId = "applyAnnoFilter",
          label = "Update annotation mapping",
          icon = icon("sync")),
        hr(),
      ),
      
      # Download controls
      h4("Download name"),
      textInput(inputId = "textExportFileName",label = "Base name for download files", value = "haplofun"),
      bsTooltip(id = "textExportFileName",title = "Exported files get their own attachment to the base name here."),
      hr(),
    ) 
  ),
  ####---------------------------------------------------------------------####
  
  
  ######------  main panel  ------######
  dashboardBody(
    shinyjs::useShinyjs(),
    
    ####--------------------------- loading splash ---------------------------####
    tags$div(
      id = "app-loading-overlay",
      class = "app-loading-overlay",
      tags$div(
        class = "app-loading-content",
        tags$div(class = "app-loading-dna", icon("dna")),
        tags$h2("Haplofun"),
        tags$p("Loading haplotype analysis workbench..."),
        tags$div(class = "app-loading-bar")
      )
    ),
    tags$head(
      tags$style(HTML("
                #app-loading-overlay {
                    position: fixed;
                    top: 0; left: 0;
                    width: 100%; height: 100%;
                    z-index: 99999;
                    background: linear-gradient(135deg, #2c3e50 0%, #1a252f 100%);
                    display: flex;
                    align-items: center;
                    justify-content: center;
                }
                .app-loading-content {
                    text-align: center;
                    color: #ffffff;
                }
                .app-loading-dna {
                    font-size: 56px;
                    color: #3498db;
                    margin-bottom: 10px;
                    animation: app-loading-spin 1.8s linear infinite;
                }
                @keyframes app-loading-spin {
                    from { transform: rotate(0deg); }
                    to { transform: rotate(360deg); }
                }
                .app-loading-content h2 {
                    font-weight: 700;
                    letter-spacing: 1px;
                    margin: 10px 0 4px 0;
                }
                .app-loading-content p {
                    color: #bdc3c7;
                    margin-bottom: 20px;
                }
                .app-loading-bar {
                    width: 220px;
                    height: 4px;
                    border-radius: 4px;
                    background: rgba(255,255,255,0.15);
                    overflow: hidden;
                    margin: 0 auto;
                }
                .app-loading-bar::after {
                    content: '';
                    display: block;
                    width: 40%;
                    height: 100%;
                    border-radius: 4px;
                    background: #3498db;
                    animation: app-loading-bar 1.4s ease-in-out infinite;
                }
                @keyframes app-loading-bar {
                    0% { transform: translateX(-120%); }
                    100% { transform: translateX(280%); }
                }
      "))
    ),
    
    ####---------------------------  tags ---------------------------####
    tags$head(
      tags$style(HTML("
                .content-wrapper, .right-side {
                    background-color: #ffffff;
                }
                .box {
                    background-color: #ffffff;
                    border-radius: 8px;
                    border: 1px solid #d0d0d0;
                    box-shadow: 0 1px 2px rgba(0,0,0,0.08);
                }
                .box.box-solid > .box-header,
                .box-header.with-border {
                    padding: 8px 10px;
                    border-radius: 8px 8px 0 0;
                    background-color: #ffffff;
                    color: white;
                    border-bottom: none;
                }
                .box-header h3.box-title {
                    font-weight: 600;
                    font-size: 18px;
                }
                .nav-tabs-custom > .tab-content {
                    background: #ffffff;
                    border-radius: 8px;
                    border: 1px solid #d0d0d0;
                    padding: 15px;
                }
                .nav-tabs-custom > .nav-tabs > li.active > a {
                    background-color: #ffffff !important;
                    border-radius: 8px 8px 0 0;
                    border: 1px solid #d0d0d0;
                }
                .content h2, .content h3, .content h4 {
                    color: #333333;
                    font-weight: 600;
                }

                /* ── Responsive: small laptop (≤1440 px) ── */
                @media (min-width: 768px) and (max-width: 1440px) {
                    .main-sidebar, .left-side          { width: 210px !important; }
                    .content-wrapper, .right-side,
                    .main-footer                       { margin-left: 210px !important; }
                    .main-header .navbar               { margin-left: 210px !important; }
                    .main-header .logo                 { width: 210px !important; }
                }

                /* Prevent horizontal scrollbar from overflowing content */
                .content-wrapper { overflow-x: hidden; }
                .box-body        { overflow-x: auto; min-width: 0; }

                /* All Bootstrap columns: allow shrinking below content width */
                [class*='col-sm-'] { min-width: 0; }

                /* Wrap long text inside narrow columns */
                .checkbox label,
                .shiny-input-container label,
                .control-label,
                .shiny-input-container p {
                    white-space: normal;
                    word-break: break-word;
                    overflow-wrap: break-word;
                    max-width: 100%;
                }

                /* Inputs never exceed their container */
                .shiny-input-container,
                input[type='text'],
                input[type='number'],
                .selectize-control {
                    max-width: 100% !important;
                }

                /* Tab navigation: wrap rows of tabs instead of overflowing */
                .nav-tabs {
                    display: flex !important;
                    flex-wrap: wrap !important;
                }
                .nav-tabs > li {
                    float: none !important;
                    flex-shrink: 0;
                }
                .nav-tabs > li > a {
                    white-space: normal;
                    padding: 8px 10px;
                }

                /* Tighter column gutters on medium screens */
                @media (max-width: 1440px) {
                    .row {
                        margin-left:  -7px !important;
                        margin-right: -7px !important;
                    }
                    [class*='col-sm-'] {
                        padding-left:  7px !important;
                        padding-right: 7px !important;
                    }
                }
            "))
    ),
    ####---------------------------------------------------------------------####
    
    ####---------------------------  tab: data disclaimer ---------------------------####
    tabItems(
      tabItem(tabName = "home",
              tags$head(
                tags$style(HTML("
                html, body, .wrapper {
                    height: 100%;
                }
                .wrapper {
                    display: flex;
                    flex-direction: column;
                }
                .content-wrapper, .right-side {
                    flex-grow: 1;
                    min-height: calc(100vh - 50px);
                    padding-bottom: 70px; 
                    font-size: 15px;
                }
                .app-footer {
                    width: 100%;
                    position: fixed;
                    bottom: 0;
                    background-color: white;
                    box-shadow: 0 -2px 5px rgba(0,0,0,0.05); 
                    z-index: 1000;
                    text-align: center;  
                    left: 0 !important;
                }

                .jumbotron-welcome {
                    background-color: #ffffff;
                    padding: 30px 20px;
                    margin-bottom: 20px;
                    border-radius: 5px;
                    box-shadow: 0 4px 8px rgba(0,0,0,0.05);
                    text-align: center;
                }
                .box-body ul li {
                    margin-bottom: 5px;
                }
                h4 {
                    margin-bottom: 10px;
                }"))
              ),

              # header part
              div(class = "jumbotron-welcome",
                  tags$img(src = "https://raw.githubusercontent.com/TimHasenbein/PegasShiny/main/data/haplofun_logo_2.png", 
                           height = "300px", 
                           alt = "haplofun Logo",
                           style = "margin-bottom: 0px;"),
                  tags$b(h4("Generate, visualize, and annotate genetic haplogroup networks.", 
                            style = "color: #333; margin-top: 5px;")),
                  p(class = "lead", "Welcome to Haplofun, a web server for interactive haplotype analysis with integrated functional annotation."),
              ),
              
              # middle part
              fluidRow(
                column(6,
                       h3(icon("cogs"), "Core functionalities"),
                       box(
                         width = 12,
                         title = "Haplofun features",
                         status = "primary",
                         tags$ul(
                           tags$li(HTML("<strong>VCF compatibility:</strong> Designed to process standard VCF (variant call format) files.")),
                           tags$li(HTML("<strong>Haplonetwork generation:</strong> Calculates haplogroups and networks from your data.")),
                           tags$li(HTML("<strong>Functional annotation:</strong> Haplotype-level annotation of functional data, such as GWAS variants or QTLs.")),
                           tags$li(HTML("<strong>Visualization:</strong> Interactive visualization of haplogroups, networks, and the functional annotation."))
                         ),
                         p("This website is free and open to all users. Upload and analysis of sensitive personal information requires a login.", 
                           style = "color: #007BFF; font-weight: 600; padding-top: 4px;"), 
                       ),
                       h3(icon("book"), "Citation"),
                       box(
                         width = 12,
                         title = "How to cite Haplofun",
                         status = "primary",
                         tags$blockquote(
                           "Hasenbein TP., Bartels L., Stolze R., Wohlers I. (2026). ", tags$em("Haplofun: Interactive haplotype analysis with integrated functional annotation.")," JOURNAL. DOI: [DOI]."
                         )
                       )
                ),
                column(6,
                       h3("Data access & privacy declaration"), 
                       box(
                        width = 12,
                         h4("Which data type would you like to upload?", style = "margin-bottom: 20px;"),
                         
                         radioButtons("data_choice", label = NULL,
                                      choices = c("Public data (no login required)",
                                                  "Private data (login/registration required)")),
                         tags$ul(style = "padding-left: 20px;", 
                                 tags$li(
                                   HTML("This application does not permanently store uploaded VCF files. Processing occurs exclusively in the working memory (RAM) during your active session.")
                                 ),
                                 tags$li(
                                   HTML("You are responsible for adhering to all laws regarding the uploaded data.")
                                 )
                         ),
                         actionButton("access_button", " Continue access", 
                                      icon = icon("arrow-circle-right"), 
                                      style = "color: #fff; background-color: #007BFF; border-color: #0056B3; margin-bottom: 15px;")
                       )
              ),
              ),
              tags$div(
                id = "privacy_banner",
                style = "background-color:#f0f4f8; color:#333333; padding:10px; border-radius:5px; border:1px solid #d0d7de; margin-bottom:10px; font-size:14px;",
                span("This app is hosted on shinyapps.io. Technically necessary session cookies are set by the hosting platform and analytics cookies may also be used. These cookies are not controlled by the app operator.")
              ),
              
              # footer
              tags$footer(
                tags$style(HTML("
                .app-footer {
                font-size: 0.85em; color: #888; text-align: center; padding-top: 10px; padding-bottom: 5px; border-top: 1px solid #eee; width: 100%;
                                }")),
                class = "app-footer",
                p(
                  "This web server is licensed under ",
                  tags$a(href = "https://choosealicense.com/licenses/mit/", target = "_blank", "MIT"),
                  " | ",
                  actionLink(inputId = "goToHelpTab", label = "User guide & documentation"),
                  " | ",
                  HTML(paste0(" &copy; ", format(Sys.Date(), "%Y"), " Biomolecular Data Science in Pneumology, Research Center Borstel, Leibniz Lung Center, Germany. All Rights Reserved.")),
                  style = "font-size: 1.2em"
                )
              )
      ),
      ####---------------------------------------------------------------------####
      
      ####---------------------------  tab: data input ---------------------------####
      tabItem(
        tabName = "dataInput",
        h3(uiOutput("upload_header")),
        
        # upper left panel
        fluidRow(
          column(6,
                 box(
                   width = 12,
                   title = "Session Status",
                   status = "primary",
                   solidHeader = FALSE,
                   fluidRow(
                     column(8,
                            p(strong("Current mode:")),
                            verbatimTextOutput("session_mode_display"),
                            p(strong("Login status:")),
                            verbatimTextOutput("login_status_display"),
                            status = "primary",
                            solidHeader = FALSE, 
                            fileInput(
                              inputId = "file",
                              label = h5("Upload VCF file"),
                              accept = c(".vcf",".gz"),
                              buttonLabel = "Browse"
                            ),
                            tags$script(HTML("document.querySelector('#file').addEventListener('change', function(e) {
                            const file = e.target.files[0];
                            const limit = 500 * 1024;  
                            if (file && file.size > limit) {
                            alert('File is too large! Please choose a smaller file.');
                            e.target.value = '';
                            }
                            });")),
                            wellPanel(
                              style = "border-color: #007BFF; background-color: #E9F4FF; color: #007BFF;",
                              p(
                                strong("Note: "), 
                                "The web server is designed for VCF files with up to ", 
                                code("1,000 samples and variants"), 
                                " and a maximum size of ", 
                                code("500 kb"), 
                                ". Exceeding these thresholds requires running the local version via GitHub."
                              )
                            ),
                            uiOutput("filePanel"),
                     ),
                     column(4,
                            h4("Test files:"),
                            p(strong("Human data:")),
                            checkboxInput(inputId = "vcfStored_1",label = HTML('1000G data (<em>IL23R</em> locus, hg38)  <a href="https://www.sciencedirect.com/science/article/pii/S2352396425000350?via%3Dihub" target="_blank" style="margin-left: 5px;" title="Zur Quelle"> <i class="fa fa-external-link-alt"></i>  </a>'),value = FALSE),
                            bsTooltip("vcfStored_human", "Use 1000G data of the <em>IL23R</em> locus (hg38)"),
                            p(strong("Bacterial data:")),
                            checkboxInput(inputId = "vcfStored_3",label = HTML('<em>H. influenzae</em> (<em>ftsI</em> locus)  <a href="https://link.springer.com/article/10.1186/s13073-024-01406-4" target="_blank" style="margin-left: 5px;" title="Zur Quelle"> <i class="fa fa-external-link-alt"></i>  </a>'),value = FALSE),
                            bsTooltip("vcfStored_3", "<em>ftsI</em> locus (Haemophilus influenzae)"),
                            hr(),
                            actionButton(
                              inputId = "run",
                              label = "Start analysis",
                              icon = icon("play"),
                              width = "100%",
                              style = "color: #fff; background-color: #007BFF; border-color: #0056B3; margin-bottom: 15px;" 
                            ),
                     )
                   ) 
                 )
          ), 
          
          # upper right panel
          column(6,  
                 box(
                   width = 12,
                   title = "Additional files",
                   status = "primary",
                   solidHeader = FALSE, 
                   fluidRow(
                     column(8,
                            fileInput(inputId = "fileSubInfo", label = h5("Upload sample information (.tsv, .txt)"), accept = c(".tsv", ".txt"), buttonLabel = "Browse"),
                            bsTooltip("fileSubInfo","Upload sample information meta-data for meta-data-specific coloring"),
                            fileInput(inputId = "GwasInfo", label = h5("Upload GWAS information (.csv)"), accept = ".csv", buttonLabel = "Browse"),
                            bsTooltip("GwasInfo","Upload GWAS information as described in the help page."),
                            fileInput(inputId = "eQTLInfo",label = h5("Upload QTL information (.csv)"), accept = ".csv", buttonLabel = "Browse"),
                            bsTooltip("eQTLInfo","Upload QTL information as described in the help page."),
                            fileInput(inputId = "annoInfo",label = h5("Upload annotation information (.csv)"), accept = ".csv", buttonLabel = "Browse"),
                            bsTooltip("annoInfo","Upload annotation information as described in the help page.")
                     ),
                     column(4,
                            h4("Pre-stored files:"),
                            p(strong("Sample information data:")),
                            checkboxInput(inputId = "meta1000G",label = HTML('1000G data (Continent) <a href="https://www.internationalgenome.org/" target="_blank" style="margin-left: 5px;" title="Zur Quelle"> <i class="fa fa-external-link-alt"></i>  </a>'),value = FALSE),
                            bsTooltip("meta1000G",'Use meta-data sample information for the 1000G data (<em>IL23R</em> data, geographic region) <a href="https://www.internationalgenome.org/" target="_blank" style="margin-left: 5px;" title="Zur Quelle"> <i class="fa fa-external-link-alt"></i>  </a>'),
                            checkboxInput(inputId = "metahinf",label = HTML('<em>H. influenzae</em> (AMR resistance) <a href="https://link.springer.com/article/10.1186/s13073-024-01406-4" target="_blank" style="margin-left: 5px;" title="Zur Quelle"> <i class="fa fa-external-link-alt"></i>  </a>'),value = FALSE),
                            bsTooltip("metahinf","Use sample information for the <em>H. influenzae</em> data about antimicrobial resistance"),
                            p(strong("Human data:")),
                            checkboxInput(inputId = "gwasStored",label = HTML('GWAS catalog (hg38)  <a href="https://www.ebi.ac.uk/gwas/" target="_blank" style="margin-left: 5px;" title="Zur Quelle"> <i class="fa fa-external-link-alt"></i>  </a>'),value = FALSE),
                            bsTooltip("gwasStored","Use pre-stored GWAS data from the GWAS catalog (hg38)"),
                            checkboxInput(inputId = "eqtlStored",label = HTML('eQTL data (hg38)  <a href="https://www.gtexportal.org/home/downloads/adult-gtex/qtl" target="_blank" style="margin-left: 5px;" title="Zur Quelle"> <i class="fa fa-external-link-alt"></i>  </a>'),value = FALSE),
                            bsTooltip("eqtlStored","Use pre-stored eQTL data from GTEx_v10_SuSiE_eQTL (hg38, pip > 0.5)"),
                            checkboxInput(inputId = "annoStored1",label = HTML('cCRE annotation (chr1-8, hg38)  <a href="https://genome.ucsc.edu/cgi-bin/hgTrackUi?db=mm10&g=encodeCcreCombined" target="_blank" style="margin-left: 5px;" title="Zur Quelle"> <i class="fa fa-external-link-alt"></i>  </a>'),value = FALSE),
                            bsTooltip("annoStored1","Use pre-stored ENCODE candidate <em>cis</em>-regulatory elements data (chr1-8, hg38)"),
                            checkboxInput(inputId = "annoStored2",label = HTML('cCRE annotation (chr9-23, hg38)  <a href="https://genome.ucsc.edu/cgi-bin/hgTrackUi?db=mm10&g=encodeCcreCombined" target="_blank" style="margin-left: 5px;" title="Zur Quelle"> <i class="fa fa-external-link-alt"></i>  </a>'),value = FALSE),
                            bsTooltip("annoStored2","Use pre-stored ENCODE candidate <em>cis</em>-regulatory elements data (chr9-23, hg38)"),
                            p(strong("Bacterial data:")),
                            checkboxInput(inputId = "hinfGwasStored",label = HTML('<em>H. influenzae</em> GWAS (AMR)  <a href="https://link.springer.com/article/10.1186/s13073-024-01406-4" target="_blank" style="margin-left: 5px;" title="Zur Quelle"> <i class="fa fa-external-link-alt"></i>  </a>'),value = FALSE),
                            bsTooltip("hinfGwasStored","GWAS pre-stored GWAS data on antimicrobial resistance"),
                            hr(),
                            h4("Select genome build for IGV"),
                            selectInput(
                              inputId = "igvGenome",
                              label = NULL,
                              choices = NULL, 
                              selected = "hg38" 
                            )
                     )
                   )
                 ) 
          ) 
        ), 
        
        # lower panel
        tags$hr(style = "border-top: 1px solid #d0d0d0;"), 
        h3(icon("database"),"Data overview:"),
        
        # data summary
        fluidRow(
          column(3, 
                 box(
                   width = 12,
                   title = "Data summary",
                   status = "primary",
                   solidHeader = FALSE,
                   shinycssloaders::withSpinner(uiOutput("SummaryStats", type = 1, color = "#007BFF"))
                 ) 
          ), 
          
          # igv
          column(9, 
                 box(
                   width = 12,
                   title = "Integrative genomics viewer",
                   status = "primary",
                   solidHeader = FALSE,
                   igvShiny::igvShinyOutput("igv_viewer", height="40vh"),
                 ) 
          ) 
        ),
        
        # vcf header
        fluidRow(
          column(3,
                 conditionalPanel(
                   condition = "input.textboxShowMeta",
                   box(
                     width = 12,
                     title = "VCF header table",
                     status = "primary",
                     solidHeader = FALSE, 
                     shinycssloaders::withSpinner(DT::dataTableOutput("plotMetadata"), color = "#007BFF")
                   )
                 )
          ),
          
          # vcf data
          column(9,
                 conditionalPanel(
                   condition = "input.textboxShowData",
                   box(
                     width = 12,
                     title = "VCF data lines",
                     status = "primary",
                     solidHeader = FALSE, 
                     shinycssloaders::withSpinner(DT::dataTableOutput("plotVcfData"), color = "#007BFF")
                   )
                 )
          )
        ),
        
        # footer
        tags$footer(
          tags$style(HTML("
          .app-footer {
            font-size: 0.85em; 
            color: #888; 
            text-align: center;
            padding-top: 10px; 
            padding-bottom: 5px;
            border-top: 1px solid #eee;
            width: 100%;
          }")),
          class = "app-footer",
          p(
            "This web server is licensed under ",
            tags$a(href = "https://choosealicense.com/licenses/mit/", target = "_blank", "MIT"),
            " | ",
            actionLink(inputId = "goToHelpTab", label = "User guide & documentation"),
            " | ",
            HTML(paste0(" &copy; ", format(Sys.Date(), "%Y"), " Biomolecular Data Science in Pneumology, Research Center Borstel, Leibniz Lung Center, Germany. All Rights Reserved.")),
            style = "font-size: 1.2em"
          )
        )
      ),
      ####---------------------------------------------------------------------####
      
      ####---------------------------  tab: haplotype visualization ---------------------------####
      tabItem(
        title = "Haplotype Analysis & Visualization",
        tabName = "haplotypeAnalysis",
        conditionalPanel(
          condition = "output.inputSelected != 'true'",
          div(
            style = "padding: 20px; border: 1px solid #ddd; border-radius: 5px; background-color: #f8f8f8; text-align: center;",
            h3(icon("triangle-exclamation"), "Data required"),
            p("Please upload your data to start the haplotype analysis.")
          )
        ),
        
        ####------  Core results  ------####
        conditionalPanel(
          condition = "output.inputSelected == 'true'",
          h3(icon("square-poll-vertical"),"Core haplotype results and filters"),
          fluidRow(
            
            # data filtering
            column(7,
                   box(
                     title = "Data filtering",
                     status = "primary",
                     solidHeader = FALSE,
                     width = 12,
                     fluidRow(
                       column(7,
                              h4("Current data summary"),
                              uiOutput("dynamicSummaryStats"),
                              uiOutput("performanceWarning")
                       ),
                       column(5,
                              h4("Set filters"),
                              fluidRow(
                                column(6, textInput("chromFilter", "Chromosome", value = NULL)),
                                column(6, numericInput("mafFilterInit", "MAF",
                                                       value = 0, min = 0, max = 0.5, step = 0.001)),
                              ),
                              fluidRow(
                                column(6, numericInput("startPos", "Start (bp)", value = NULL)),
                                column(6, numericInput("endPos", "End (bp)", value = NULL))
                              ),
                              fluidRow(
                                column(6, actionButton(
                                  inputId = "applyFilters",
                                  label = "Apply Filters",
                                  icon = icon("play"),
                                  width = "100%",
                                  style = "color: #fff; background-color: #007BFF; border-color: #0056B3; margin-bottom: 15px;" 
                                ),
                                bsTooltip("applyFilters","Apply filters")
                                ),
                                column(6, actionButton(
                                  inputId = "resetFilters",
                                  label = "Reset",
                                  icon = icon("undo"),
                                  width = "100%",
                                  style = "color: black; background-color: lightgrey; border-color: #2e6da4; margin-bottom: 15px;"
                                ),
                                bsTooltip("resetFilters","Reset all filters")
                                )
                              ),
                       )
                     )
                   ),
                   
                   # Genotype data table
                   box(
                     title = tagList(tags$span("Genotype data table")),
                     status = "primary",
                     solidHeader = FALSE,
                     width = 12,
                     tabsetPanel(
                       id = "compactTablesSubTabs",
                       type = "tabs",
                       
                       # haplogroup frequency table
                       tabPanel(
                         title = "Haplogroup frequency table",
                         value = "hapFreqTab",
                         tags$div(
                           downloadButton(outputId = "buttonExportHaplotypeTable",icon = icon("download"), label = "",style = "float: right; z-index: 9999; margin-top: -60px; color: black; background-color: #f0f0f0; border-color: #2e6da4;"),
                           bsTooltip("buttonExportHaplotypeTable","Download haplotype table")), 
                         shinycssloaders::withSpinner(DT::dataTableOutput("plotHaplotypeTable", height = "380px"), color = "#007BFF"),
                       ),
                       
                       # haplogroup sample table
                       tabPanel(
                         title = "Samples per haplogroup",
                         value = "hapSamTab",
                         tags$div(
                           downloadButton(outputId = "buttonExportSampleHaplo",icon = icon("download"), label = "",style = "float: right; z-index: 9999; margin-top: -60px; color: black; background-color: #f0f0f0; border-color: #2e6da4;"),
                           bsTooltip("buttonExportSampleHaplo","Download list of samples per haplogroup")),
                         shinycssloaders::withSpinner(DT::dataTableOutput("plotSampleTable", height = "380px"), color = "#007BFF"),
                       ),
                     )
                   )
            ), 
            
            # haplotype visualizations
            column(5,
                   box(
                     title = "Haplotype visualizations",
                     status = "primary",
                     solidHeader = FALSE,
                     width = 12,
                     height = 920, 
                     tabsetPanel(
                       id = "compactPlotSubTabs",
                       type = "tabs",
                       
                       # bar plot
                       tabPanel(
                         title = "Frequencies",
                         value = "barVis",
                         downloadButton(outputId = "buttonExportHaplotypeFreqBarPlot",icon = icon("download"), label = "",style = "float: right; z-index: 9999; margin-top: -60px; color: black; background-color: #f0f0f0; border-color: #2e6da4;"),
                         bsTooltip("buttonExportHaplotypeFreqBarPlot","Download plot"),
                         shinycssloaders::withSpinner(plotOutput("plotHaplotypeBars", height = "740px"), color = "#007BFF")
                       ),
                       
                       # distance matrix
                       tabPanel(
                         title = "Distance matrix",
                         value = "heatmapVis",
                         conditionalPanel(
                           condition = "input.selectDistanceMatrix == 2",
                           downloadButton(outputId = "buttonExportDistanceMatrix",icon = icon("download"), label = "",style = "float: right; z-index: 9999; margin-top: -60px; color: black; background-color: #f0f0f0; border-color: #2e6da4;"),
                           bsTooltip("buttonExportDistanceMatrix","Download distance matrix"),
                           div(
                             style = "overflow-x: scroll; overflow-y: scroll; max-height: 740px; max-width: 100%;",
                             shinycssloaders::withSpinner(tableOutput("plotDistanceMatrixAsMatrix"), color = "#007BFF")
                           )
                         ),
                         conditionalPanel(
                           condition = "input.selectDistanceMatrix == 3",
                           downloadButton(outputId = "buttonExportHeatmapPDF",icon = icon("download"), label = "",style = "float: right; z-index: 9999; margin-top: -60px; color: black; background-color: #f0f0f0; border-color: #2e6da4;"),
                           bsTooltip("buttonExportHeatmapPDF","Download"),
                           shinycssloaders::withSpinner(plotly::plotlyOutput("plotDistanceMatrixAsHeatmap",height = "740px"), color = "#007BFF")
                         )
                       ),
                       
                       # network
                       tabPanel(
                         title = "Network",
                         value = "networkVis",
                         conditionalPanel(
                           condition = "input.selectNetwork != 1",
                           tags$div(
                             style = "float: right; margin-right: 10px; margin-top: -60px; margin-bottom: 5px;", 
                             downloadButton(
                               outputId = "buttonExportHaplonetPDF",
                               icon = icon("download"),
                               label = "",
                               style = "color: black; background-color: #f0f0f0; border-color: #2e6da4; margin-left: 5px;z-index: 9999;"
                             ),
                             bsTooltip("buttonExportHaplonetPDF", "Download Plot as PDF"),
                           ),
                           tags$div(
                             style = "overflow-x: auto; overflow-y: auto; max-height: 700px;",
                             shinycssloaders::withSpinner(plotOutput("plotNetwork", height = "auto"), color = "#007BFF"),
                             shinycssloaders::withSpinner(tableOutput("tableNetwork")),
                           )
                         )
                       ),
                       
                       # pca
                       tabPanel(
                         title = "PCA",
                         value = "pcaVis",
                         downloadButton(outputId = "buttonPcaPlotPDF",icon = icon("download"), label = "",style = "float: right; z-index: 9999; margin-top: -60px; color: black; background-color: #f0f0f0; border-color: #2e6da4;"),
                         bsTooltip("buttonPcaPlotPDF","Download"),
                         shinycssloaders::withSpinner(plotly::plotlyOutput("finalPcaPlot", height = "740px"), color = "#007BFF")
                       ),
                       
                       # dendrogram
                       tabPanel(
                         title = "Dendrogram",
                         value = "dendrogramVis",
                         downloadButton(outputId = "buttonExportDendroPDF",icon = icon("download"), label = "",style = "float: right; z-index: 9999; margin-top: -60px; color: black; background-color: #f0f0f0; border-color: #2e6da4;"),
                         bsTooltip("buttonExportDendroPDF","Download"),
                         shinycssloaders::withSpinner(plotOutput("plotDendrogram", height = "740px"), color = "#007BFF")
                       ),
                       
                       # allele heatmap
                       tabPanel(
                         title = "Allele heatmap",
                         value = "AlleleheatmapVis",
                         downloadButton(outputId = "buttonExportAlleleHeatmapPDF",icon = icon("download"), label = "",style = "float: right; z-index: 9999; margin-top: -60px; color: black; background-color: #f0f0f0; border-color: #2e6da4;"),
                         bsTooltip("buttonExportAlleleHeatmapPDF","Download"),
                         shinycssloaders::withSpinner(plotly::plotlyOutput("plotAlleleHeatmap", height = "740px"), color = "#007BFF")
                       )
                     ) 
                   ) 
            ) 
          ), 
          
          ####------  Advanced filtering  ------####
          tags$hr(style = "border-top: 1px solid #d0d0d0;"),
          h3(icon("filter"),"Advanced filtering options"),
          fluidRow(
            
            # ld pruning
            column(10,
                   box(
                     title = "Linkage disequilibrium (LD) pruning",
                     status = "primary",
                     solidHeader = FALSE, 
                     width = 12,
                     checkboxInput("enableLdPruning", "Enable LD visualization and pruning controls", FALSE),
                     conditionalPanel(
                       condition = "input.enableLdPruning == true",
                       fluidRow(
                         column(3,
                                numericInput("ldThreshold", "LD r2 Threshold",
                                             min = 0, max = 1.0, value = 0.8)
                         ),
                         column(9,
                                tags$br(),
                                actionButton(
                                  inputId = "applyLdPruning",
                                  label = "Apply LD pruning",
                                  icon = icon("scissors"),
                                  style = "color: #fff; background-color: #007BFF; border-color: #0056B3; margin-bottom: 15px;" 
                                )
                         )
                       ),
                       hr(),
                       h4("Linkage disequilibrium heatmaps"),
                       fluidRow(
                         column(6,
                                h5("Before pruning"),
                                downloadButton(outputId = "buttonLdHeatmapBefore", icon = icon("download"), label = "", style = "float: right; position: relative; z-index: 9999; margin-top: -40px; color: black; background-color: #f0f0f0; border-color: #007BFF;"),
                                bsTooltip("buttonLdHeatmapBefore", "Download"),
                                shinycssloaders::withSpinner(plotOutput("ldHeatmapBefore", height = "600px"), color = "#007BFF")
                         ),
                         column(6,
                                h5("After pruning"),
                                conditionalPanel(
                                  condition = "output.ldPrunedVcfStoreIsNull == false",
                                  downloadButton(outputId = "buttonLdHeatmapAfter", icon = icon("download"), label = "", style = "float: right; position: relative; z-index: 9999; margin-top: -40px; color: black; background-color: #f0f0f0; border-color: #007BFF;"),
                                  bsTooltip("buttonLdHeatmapAfter", "Download"),
                                  shinycssloaders::withSpinner(plotOutput("ldHeatmapAfter", height = "600px"), color = "#007BFF")
                                )
                         )
                       ) 
                     ) 
                   ) 
            ), 
            
            # random sampling
            column(2,
                   box(
                     title = "Random SNP sampling",
                     status = "primary",
                     solidHeader = FALSE,
                     width = 12,
                     checkboxInput("enableSnpsampling",
                                   "Perform random SNP sub-sampling on current data",
                                   FALSE),
                     conditionalPanel(
                       condition = "input.enableSnpsampling == true",
                       numericInput("snpSampleSize", "Number of SNPs",
                                    value = 100,
                                    min = 1,
                                    max = 1000000),
                       actionButton(
                         inputId = "applySnpsampling",
                         label = "Apply random sampling",
                         icon = icon("dice"),
                         width = "100%",
                         style = "color: #fff; background-color: #007BFF; border-color: #0056B3; margin-bottom: 15px;"
                       )
                     )
                   )
            )
          ), 
          
          ####------  functional annotation  ------####
          tags$hr(style = "border-top: 2px solid #d0d0d0;"), 
          h3(icon("dna"), "Functional annotation results"),
          
          ####------  GWAS  ------####
          conditionalPanel(
            condition = "output.gwasDataSourceAvailable || output.eQTLFileUploaded || output.annoFileUploaded",
            div(id = "annotation_tabs_container",
                tabBox(
                  id = "annotation_tabs",
                  width = 12, 
                  side = "left", 
                  tabPanel(
                    title = span("GWAS annotation", 
                                 style = "background-color: #FF9E99; 
                        color: black; 
                        padding: 8px 15px; 
                        border-radius: 5px 5px 0 0;
                        display: inline-block;
                        margin: -10px;"),
                    value = "gwas_tab",
                    conditionalPanel(
                      condition = "output.gwasDataSourceAvailable",
                      h4("GWAS annotation"),
                      p("This section displays the overlap between haplogroups and risk- or trait-associated variants."),
                      
                      # upper left panel
                      fluidRow(
                        column(6,
                               box(
                                 width = 12,
                                 title = tagList("GWAS variants in haplogroups",
                                                 downloadButton(outputId = "buttonGwasSnpTable", icon = icon("download"), label = "",
                                                                style = "position: absolute; right: 10px; top: 7px; color: black; background-color: #f0f0f0; border-color: #8b0000; padding: 3px 8px;")),
                                 status = "danger",
                                 solidHeader = FALSE,
                                 bsTooltip("buttonGwasSnpTable","Download"),
                                 shinycssloaders::withSpinner(DT::dataTableOutput("gwasSnpTable", height="450px"), color = "#DC3545")
                               )
                        ),
                        
                        # upper right panel
                        column(6,
                               box(
                                 width = 12,
                                 title = tagList("GWAS allele mapping",
                                                 downloadButton(outputId = "buttonGwasRiskHeatmap", icon = icon("download"), label = "",
                                                                style = "position: absolute; right: 10px; top: 7px; color: black; background-color: #f0f0f0; border-color: #8b0000; padding: 3px 8px;")),
                                 status = "danger",
                                 solidHeader = FALSE,
                                 bsTooltip("buttonGwasRiskHeatmap","Download"),
                                 shinycssloaders::withSpinner(plotly::plotlyOutput("plotGwasRiskHeatmap", height="450px"), color = "#DC3545"),
                                 htmlOutput("warningTextGwas")
                               )
                        )
                      ),
                      
                      # lower left panel
                      fluidRow(
                        column(6,
                               box(
                                 width = 12,
                                 title = tagList("Haplogroup allele table (GWAS)",
                                                 downloadButton(outputId = "buttonGwasHaplotypeTable", icon = icon("download"), label = "",
                                                                style = "position: absolute; right: 10px; top: 7px; color: black; background-color: #f0f0f0; border-color: #8b0000; padding: 3px 8px;")),
                                 status = "danger",
                                 solidHeader = FALSE,
                                 bsTooltip("buttonGwasHaplotypeTable","Download"),
                                 shinycssloaders::withSpinner(DT::dataTableOutput("gwasHaplotypeTable", height="450px"), color = "#DC3545")
                               )
                        ),
                        
                        # lower right panel
                        column(6,
                               box(
                                 width = 12,
                                 title = tagList("GWAS effect sizes",
                                                 downloadButton(outputId = "buttonGwasLolli", icon = icon("download"), label = "",
                                                                style = "position: absolute; right: 10px; top: 7px; color: black; background-color: #f0f0f0; border-color: #8b0000; padding: 3px 8px;")),
                                 status = "danger",
                                 solidHeader = FALSE,
                                 bsTooltip("buttonGwasLolli","Download"),
                                 shinycssloaders::withSpinner(plotly::plotlyOutput("plotGwasLolli", height = "430px"), color = "#DC3545")
                               )
                        )
                      )
                    )
                  ),
                  
                  ####------  QTL  ------####
                  tabPanel(
                    title = span("QTL annotation", 
                                 style = "background-color: #D8F5DD; 
                        color: black; 
                        padding: 8px 15px; 
                        border-radius: 5px 5px 0 0;
                        display: inline-block;
                        margin: -10px;"),
                    value = "qtl_tab",
                    conditionalPanel(
                      condition = "output.eQTLFileUploaded",
                      h4("QTL annotation"),
                      p("This section displays the overlap between haplogroups and quantitative trait loci (QTLs)."),
                      
                      # upper left panel
                      fluidRow(
                        column(6,
                               box(
                                 width = 12,
                                 title = tagList("QTL variants in haplogroups",
                                                 downloadButton(outputId = "buttoneQTLSnpTable", icon = icon("download"), label = "",
                                                                style = "position: absolute; right: 10px; top: 7px; color: black; background-color: #f0f0f0; border-color: #28A745; padding: 3px 8px;")),
                                 status = "success",
                                 solidHeader = FALSE,
                                 bsTooltip("buttoneQTLSnpTable","Download"),
                                 shinycssloaders::withSpinner(DT::dataTableOutput("eQTLSnpTable", height="450px"), color = "#28A745")
                               )
                        ),
                        
                        # upper right panel
                        column(6,
                               box(
                                 width = 12,
                                 title = tagList("QTL allele mapping",
                                                 downloadButton(outputId = "buttonploteQTLRiskHeatmap", icon = icon("download"), label = "",
                                                                style = "position: absolute; right: 10px; top: 7px; color: black; background-color: #f0f0f0; border-color: #28A745; padding: 3px 8px;")),
                                 status = "success",
                                 solidHeader = FALSE,
                                 bsTooltip("buttonploteQTLRiskHeatmap","Download"),
                                 shinycssloaders::withSpinner(plotly::plotlyOutput("ploteQTLRiskHeatmap", height = "450px"), color = "#28A745"),
                                 htmlOutput("warningTexteQTL")
                               )
                        )
                      ),
                      
                      # lower left panel
                      fluidRow(
                        column(6,
                               box(
                                 width = 12,
                                 title = tagList("Haplogroup allele table (QTLs)",
                                                 downloadButton(outputId = "buttonploteqtlHaplotypeTable", icon = icon("download"), label = "",
                                                                style = "position: absolute; right: 10px; top: 7px; color: black; background-color: #f0f0f0; border-color: #28A745; padding: 3px 8px;")),
                                 status = "success",
                                 solidHeader = FALSE,
                                 bsTooltip("buttonploteqtlHaplotypeTable","Download"),
                                 shinycssloaders::withSpinner(DT::dataTableOutput("eqtlHaplotypeTable", height="450px"), color = "#28A745")
                               )
                        ),
                        
                        # lower right panel
                        column(6,
                               box(
                                 width = 12,
                                 title = tagList("QTL effect sizes",
                                                 downloadButton(outputId = "buttonplotEqtlsLolli", icon = icon("download"), label = "",
                                                                style = "position: absolute; right: 10px; top: 7px; color: black; background-color: #f0f0f0; border-color: #28A745; padding: 3px 8px;")),
                                 status = "success",
                                 solidHeader = FALSE,
                                 bsTooltip("buttonplotEqtlsLolli","Download"),
                                 shinycssloaders::withSpinner(plotly::plotlyOutput("plotEqtlsLolli", height = "430px"), color = "#28A745")
                               )
                        )
                      )
                    )
                  ),
                  
                  ####------  genomic annotation  ------####
                  tabPanel(
                    title = span("Genomic annotation", 
                                 style = "background-color: #F2D394; 
                        color: black; 
                        padding: 8px 15px; 
                        border-radius: 5px 5px 0 0;
                        display: inline-block;
                        margin: -10px;"),
                    value = "anno_tab",
                    conditionalPanel(
                      condition = "output.annoFileUploaded",
                      h4("Genomic annotation"),
                      p("This section displays the overlap between haplogroups and the genomic annotation."),
                      
                      # upper left panel
                      fluidRow(
                        column(6,
                               box(
                                 width = 12,
                                 title = tagList("Variants overlapping annotation",
                                                 downloadButton(outputId = "buttonannoSnpTable", icon = icon("download"), label = "",
                                                                style = "position: absolute; right: 10px; top: 7px; color: black; background-color: #f0f0f0; border-color: #FFC107; padding: 3px 8px;")),
                                 status = "warning",
                                 solidHeader = FALSE,
                                 bsTooltip("buttonannoSnpTable","Download"),
                                 shinycssloaders::withSpinner(DT::dataTableOutput("annoSnpTable", height="450px"), color = "#FFC107")
                               )
                        ),
                        
                        # upper right panel
                        column(6,
                               box(
                                 width = 12,
                                 title = tagList("Annotation mapping",
                                                 downloadButton(outputId = "buttonplotAnnoHeatmap", icon = icon("download"), label = "",
                                                                style = "position: absolute; right: 10px; top: 7px; color: black; background-color: #f0f0f0; border-color: #FFC107; padding: 3px 8px;")),
                                 status = "warning",
                                 solidHeader = FALSE,
                                 bsTooltip("buttonplotAnnoHeatmap","Download"),
                                 shinycssloaders::withSpinner(plotly::plotlyOutput("plotAnnoHeatmap", height = "450px"), color = "#FFC107"),
                                 htmlOutput("warningTextAnno")
                               )
                        )
                      ),
                      
                      # lower left panel
                      fluidRow(
                        column(6,
                               box(
                                 width = 12,
                                 title = tagList("Haplogroup allele table (annotation)",
                                                 downloadButton(outputId = "buttonannoHaplotypeTable", icon = icon("download"), label = "",
                                                                style = "position: absolute; right: 10px; top: 7px; color: black; background-color: #f0f0f0; border-color: #FFC107; padding: 3px 8px;")),
                                 status = "warning",
                                 solidHeader = FALSE,
                                 bsTooltip("buttonannoHaplotypeTable","Download"),
                                 shinycssloaders::withSpinner(DT::dataTableOutput("annoHaplotypeTable", height="450px"), color = "#FFC107")
                               )
                        ),
                        
                        # lower right panel
                        column(6,
                               box(
                                 width = 12,
                                 title = tagList("VCF variants overlapping annotation features",
                                                 downloadButton(outputId = "buttonannoPlot", icon = icon("download"), label = "",
                                                                style = "position: absolute; right: 10px; top: 7px; color: black; background-color: #f0f0f0; border-color: #FFC107; padding: 3px 8px;")),
                                 status = "warning",
                                 solidHeader = FALSE,
                                 bsTooltip("buttonannoPlot","Download"),
                                 shinycssloaders::withSpinner(plotly::plotlyOutput("annoPlot", height = "430px"), color = "#FFC107")
                               )
                        )
                      )
                    )
                  )
                ),
                tags$hr(style = "border-top: 1px solid #d0d0d0;")
            ) 
          )
        ),
        
        # footer
        tags$footer(
          tags$style(HTML("
          .app-footer {
            font-size: 0.85em;
            color: #888; 
            text-align: center;
            padding-top: 10px; 
            padding-bottom: 5px;
            border-top: 1px solid #eee;
            width: 100%;
          }
        ")),
          class = "app-footer",
          p(
            "This web server is licensed under ",
            tags$a(href = "https://choosealicense.com/licenses/mit/", target = "_blank", "MIT"),
            " | ",
            actionLink(inputId = "goToHelpTab", label = "User guide & documentation"),
            " | ",
            HTML(paste0(" &copy; ", format(Sys.Date(), "%Y"), " Biomolecular Data Science in Pneumology, Research Center Borstel, Leibniz Lung Center, Germany. All Rights Reserved.")),
            style = "font-size: 1.2em"
          )
        )
      ),
      ####---------------------------------------------------------------------####
      
      ####---------------------------  tab: help ---------------------------####
      tabItem(
        tabName = "help",
        tags$head(
          tags$style(HTML("
    .content-wrapper { min-height: calc(100vh - 50px) !important; padding-bottom: 70px !important; }
    .app-footer { }

    .jumbotron-welcome-nar {
        background-color: #ffffff; 
        padding: 20px 30px; 
        margin-bottom: 30px;
        border-radius: 8px;
        box-shadow: 0 4px 8px rgba(0,0,0,0.1); 
    }
    .jumbotron-welcome-nar h4 {
        color: #1a1a1a; 
        margin-top: 15px;
        font-weight: 600;
    }
    .tab-content { 
        padding: 15px;
    }
    .box-body ul li {
        margin-bottom: 8px;  
    }
    .content-wrapper p, .content-wrapper ul li, .tab-content p, .tab-content ul li {
        font-size: 16px; 
        line-height: 1.65; 
    }
    .jumbotron-welcome-nar .lead {
    font-size: 22px; 
    }
    "))
        ),
        div(class = "jumbotron-welcome-nar",
            
            # header
            fluidRow(
              column(9,
                     tags$b(h2("Haplofun: Interactive haplotype analysis with integrated functional annotation.", 
                               style = "color: #1a1a1a; margin-top: 0;")),
                     p(class = "lead", "A robust tool for the generation, visualization, and annotation of genetic haplotypes from VCF data."),
                     tags$a(href = "#citation-section", tags$strong(icon("external-link-alt"), "Publication link (upon publication)"), style = "margin-top: 2px; display: inline-block;")
              ),
              column(3,
                     tags$img(src = "https://raw.githubusercontent.com/TimHasenbein/PegasShiny/main/data/haplofun_logo_2.png", 
                              height = "150px", width = "auto",
                              alt = "haplofun Logo",
                              style = "display: block; margin-left: auto; margin-right: 0;") 
              )
            )
        ),
        tags$hr(style = "border-top: 1px solid #d0d0d0;"),
        
        # lower panel
        h3("User guide & documentation"),
        tabBox(
          title =  NULL,
          width = 12,
          id = "doc_tabs",
          
          # data input tab
          tabPanel(
            title = "1. Data input & compliance", 
            icon = icon("upload"),
            p("To facilitate the application usage, please refer to the following video walkthroughs and written documentation:"),
            tags$div(
              tags$ul(
                tags$li(tags$strong("Video walkthroughs:"), " Step-by-step visual guides to quickly familiarize yourself with the application, including data input procedures and primary output visualization."),
                tags$li(tags$strong("Written documentation:"), " Comprehensive documentation with information on app functionality and usage.")
              ),
              style = "border-left: 5px solid #007bff; padding: 5px; margin: 5px 0; background-color: #eaf4ff; font-size: 1em;"
            ),
            p("Note: The sessions expire after 15 minutes of inactivity."),
            hr(style = "border-top: 1px solid #eee; margin: 30px 0;"),
            h2("1. Data input and compliance"),
            p("This section is about the data protection declaration, data-upload, and the use of pre-stored data."),
            fluidRow(
              column(4,
                     h3("1.1 Data security and compliance"),
                     tags$strong("Data protection declaration:"),
                     p(
                       "The Haplofun web server operates on a non-persistence principle. Uploaded VCF files are not permanently stored on the server; processing occurs exclusively in the working memory (RAM) during your active session. ",
                       p("However, users must declare the nature of their data:"),
                       tags$ul(style = "margin-top: 5px;", 
                               tags$li(tags$strong(" Public data:"), " No login required."),
                               tags$li(tags$strong(" Sensitive data:"), " Registration and login required.")
                       ),
                       "Regardless of the data type, the user retains full responsibility for adhering to all relevant data protection laws concerning the uploaded material."
                     )
              ),
              column(8,
                     tags$div(
                       tags$video(
                         id = "video_1",
                         type = "video/mp4",
                         src = "https://raw.githubusercontent.com/TimHasenbein/PegasShiny/main/new_video1.mov", 
                         controls = "controls",
                         style = "width: 100%; max-width: 1500px; display: block; margin: 20px auto; border: 1px solid #ccc;")
                     )
              )
            ),
            hr(style = "border-top: 1px solid #eee; margin: 30px 0;"),
            h3("1.2 Data upload"),
            fluidRow(
              column(4,
                     p(
                       "Upon confirming the data protection declaration, the Haplofun workflow begins. The server accepts genomic variation data conforming to the ", 
                       tags$strong("Variant Call Format (VCF)"), 
                       ", independent of species. Furthermore, meta-data files are accepted for optional functional annotation."
                     ),
                     tags$b(tags$strong(icon("check-circle"), "Required files:")),
                     div(class = "well", style = "background-color: #f7f7f7; border-left: 5px solid #007bff; padding: 15px;",
                         tags$ul(
                           tags$li(tags$strong("VCF file (vcf or vcf.gz):"), "The main data file. Must include sample genotype information (GT field). The file size limit is ", tags$strong("500 kb"), " for the public server. For the web-server VCF files should contain < 1,000 variant and < 1,000 samples. For larger files, pre-filtering with bcftools is required. Multiallelic variants must be represented as comma-separated values within a single line rather than split across multiple bi-allelic records."),
                         )
                     ),
                     tags$b(tags$strong(icon("check-circle"), "Optional files:")),
                     div(class = "well", style = "background-color: #f7f7f7; border-left: 5px solid #007bff; padding: 15px;",
                         tags$ul(
                           tags$li(tags$strong("Sample information (.tsv):"), "A two-column tab delimited file containing sample attributes (e.g., population, phenotype). The first column contains the sample IDs, while the second column holds the meta-data (eg. geographic location, trait) for the respective sample in column 1. This file is used to color the frequency bar and network plot."),
                           tags$li(tags$strong("GWAS information (.csv):"), 'A Genome-Wide Association Study (GWAS) file for mapping trait-associated single nucleotide polymorphisms (SNPs) to haplogroups. Required columns are comma separated and adhere to the format: "chromosome", "start", "end", "name", "score", "strand", followed by columns for "trait", "risk allele", "pval", and "effect_size".'),
                           tags$li(tags$strong("QTL information (.csv):"), 'A Quantitative Trait Loci (QTL) file for mapping QTLs to haplogroups. Required columns are comma separated and adhere to the following format: "chromosome", "start", "end", "name (here REF/Effect allele)", "score", "strand", followed by columns for "tissue", "pip", "effect size (allelic fold change)", and "target gene"'),
                           tags$li(tags$strong("Annotation information (.csv):"), 'Annotation file for mapping genomic features to haplogroups. Required columns are comma separated and include: "chromosome", "start", "end", "name", "score", "strand')
                         )
                     )
              ),
              column(8,
                     tags$div(
                       tags$video(
                         id = "video_2",
                         type = "video/mp4",
                         src = "https://raw.githubusercontent.com/TimHasenbein/PegasShiny/main/new_video2.mov", 
                         controls = "controls",
                         style = "width: 100%; max-width: 1500px; display: block; margin: 20px auto; border: 1px solid #ccc;"),
                     )
              )
            ),
            hr(style = "border-top: 1px solid #eee; margin: 30px 0;"),
            h3("1.3 Accessing pre-stored data sets"),
            p("Besides your own data, you can access public data sets for functional annotation. Currently, a limited number of pre-stored files are accessible, which will be expanded in the future. "),
            fluidRow(
              column(6, 
                     p(tags$strong("Pre-stored VCF Files:")),
                     tags$ul(style = "margin-top: 5px;", 
                             tags$li(tags$strong("1000G data (", tags$em("IL23R"), " locus, hg38):"), "1000 Genomes project data, filtered for variants of the ", tags$em("IL23R"), " gene locus (chr1:66952344-67374700)."),
                             tags$li(tags$strong(tags$em("H. influenzae"), " (", tags$em("ftsI"), " locus):"), "Example VCF data for the bacterial ", tags$em("ftsI"), " gene locus (1688312-1690102).")
                     ),
                     
                     
              ),
              column(6,
                     p(tags$strong("Pre-stored annotation files:")),
                     tags$ul(style = "margin-top: 5px;", 
                             tags$li(tags$strong("GWAS catalog (hg38):"), "Pre-processed human GWAS data from the GWAS catalog used for risk allele mapping (hg38)."),
                             tags$li(tags$strong("eQTL Data (hg38):"), "Pre-processed human expression Quantitative Trait Loci (eQTL) data for annotation (from GTEx_v10_SuSiE_eQTL, hg38, pip > 0.5)."),
                             tags$li(tags$strong("cCREs Annotation (ENCODE):"), "Candidate ", tags$em("cis"), "-regulatory elements annotation file from the UCSC genome browser (ENCODE)."),
                             tags$li(tags$strong(tags$em("H. influenzae"), " (", tags$em("ftsI"), " locus) :"), "GWAS data from ",tags$em("H. influenzae"), " GWAS for antimicrobial resistance (Diricks, M., Petersen, S., Bartels, L. et al., Genome Med 2024)."),
                     )
              )
            ),
            hr(style = "border-top: 1px solid #eee; margin: 30px 0;"),
            h3("1.4 Pre-Analysis data inspection"),
            fluidRow(
              column(4,
                     p("This section provides an inital overview of the uploaded data."),
                     tags$ul(
                       tags$li(tags$strong("Data summary: "), "Data overview of the VCF file, showing the genomic range with data entries, as well as the number of initial samples, variants, haplotypes and distinct haplogroups."),
                       tags$li(tags$strong("Integrative genomics viewer: "), "IGV browser for VCF visualization. The reference annotation can be selected in the ",tags$strong("Select Genome Build for IGV "), "tab."),
                       tags$li(tags$strong("VCF header table: "), "Displays the VCF's header lines, including the position, variant ID, alleles, quality, and filters."),
                       tags$li(tags$strong("VCF data lines: "), "Presents the variant records.")
                     )
              ),
              column(8,
                     tags$div(
                       tags$video(
                         id = "video_3",
                         type = "video/mp4",
                         src = "https://raw.githubusercontent.com/TimHasenbein/PegasShiny/main/new_video3.mov", 
                         controls = "controls",
                         style = "width: 100%; max-width: 1500px; display: block; margin: 20px auto; border: 1px solid #ccc;"),
                     )
              )
            )
          ),
          
          # haplotype network tab
          tabPanel(
            title = "2. Haplotype network & visualization",
            icon = icon("project-diagram"),
            
            h2("2. Core haplotype analysis and visualization"),
            p("This section facilitates the primary analysis of core haplotypes, providing both quantitative data and comprehensive visualization. The interface is organized into three distinct, interactive components: ",tags$strong("Data Filtering and SNP Sub-sampling "), "the ",tags$strong("Haplotype Frequency Table "), "and the ",tags$strong("Haplotype Visualizations.")),
            
            hr(style = "border-top: 1px solid #d0d0d0; margin: 30px 0;"),
            h3("2.1 Data filtering"),
            p("This component provides a real-time summary of the loaded dataset and offers options for dynamic data filtering"),
            fluidRow(
              column(4,
                     tags$strong("Current data summary"),
                     p("A continously updated overview reflecting the current filtered data state:"),
                     tags$ul(
                       tags$li("Total number of ", tags$strong("samples")),
                       tags$li("Total number of ", tags$strong("variants (SNPs)")),
                       tags$li("Total number of ", tags$strong("haplotypes")),
                       tags$li("Total number of ", tags$strong("haplogroups"))
                     ),
                     tags$strong(icon("filter"), "Filtering options:"),
                     p("Here, the dataset can be filtered. The data summary dynamically update following the application of any filter."),
                     tags$ul(
                       tags$li(tags$strong("Genomic location:"), "Restrict the analysis to a specific chromosomal region defined by a start and end coordinate."),
                       tags$li(tags$strong("Minor allele frequency (MAF):"), "Exclude rare variants by setting a minimum MAF threshold.")
                     ),
                     hr(style = "border-top: 1px solid #d0d0d0; margin: 30px 0;"),
                     h3("2.2 Haplogroup frequency table"),
                     tags$ul(
                       tags$li(tags$strong("Content:"), "The table lists the frequency of haplogroups and the allele observed at each locus across the defined genomic region."),
                       tags$li(tags$strong("Reactivity:"), "The table content is contingent upon the value set in the 'Number of top haplogroups' selection control and the filtering criteria, prioritizing the most frequent haplogroups (max. 50).")
                     ),
                     hr(style = "border-top: 1px solid #d0d0d0; margin: 30px 0;"),
                     h3("2.3 Samples per haplogroup"),
                     tags$ul(
                       tags$li(tags$strong("Content:"), "The table lists the IDs of samples present in the different haplogroups."),
                       tags$li(tags$strong("Reactivity:"), "The table content is contingent upon the genetic variants selected.")
                     ),
              ),
              column(8,
                     tags$div(
                       tags$video(
                         id = "video_4",
                         type = "video/mp4",
                         src = "https://raw.githubusercontent.com/TimHasenbein/PegasShiny/main/new_video4.mov", 
                         controls = "controls",
                         style = "width: 100%; max-width: 1500px; display: block; margin: 20px auto; border: 1px solid #ccc;"),
                     )
              )
            ),
            hr(style = "border-top: 1px solid #d0d0d0; margin: 30px 0;"),
            h3("2.4 Advanced filtering options  "),
            p("This section provides advance VCF filtering options, including:"),
            fluidRow(
              column(4,
                     tags$ul(
                       tags$li(tags$strong("Linkage disequillibrium (LD) pruning: "), "Filtering option for subsetting the data based on LD pruning. LD is calculated for the variants of your data set. Multiallelic variants cannot be processed reliably due to limitations of the pegas package. For variants that are in LD above the r2 threshold, the first variant per pair is retained when LD pruning is applied."),
                       tags$li(tags$strong("Random SNP sub-sampling: "), "Randomly selects the specified number of variants from the current filtered data set. This function is used primarily to assess the robustness and variability of the inferred haplotype network. By comparing networks generated from multiple random subsets of SNPs, users gain insight into the stability of the genetic relationships determined by the predefined variant set.")
                     )
              ),
              column(8,
                     tags$div(
                       tags$video(
                         id = "video_11",
                         type = "video/mp4",
                         src = "https://raw.githubusercontent.com/TimHasenbein/PegasShiny/main/new_video5.mov", 
                         controls = "controls",
                         style = "width: 100%; max-width: 1500px; display: block; margin: 20px auto; border: 1px solid #ccc;"),
                     )
              )
            ),
            hr(style = "border-top: 1px solid #d0d0d0; margin: 30px 0;"),
            h3("2.5 Haplotype visualizations"),
            p("This section provides six interactive plots derived from the VCF input data, enabling a structural analysis of the haplotypes. All plots are dynamically linked to the 'Number of top haplotypes' control in the sidebar. Furthermore, the sidebar content is context-dependent, dynamically presenting plot-specific configuration parameters upon selection."),
            fluidRow(
              column(4,
                     tags$strong(icon("palette"), "Available visualization types:"),
                     tags$ul(
                       tags$li(tags$strong("Frequencies (bar plot): "), "Bar plot showing the frequencies of samples per haplogroup."),
                       tags$li(tags$strong("Distance matrix (heatmap): "), "The plot shows the distance matrix, calculated by the Hamming distance for the selected number of haplogroups. Furthermore, you can specify if clustering of the matrix should be performed and if so you can select one of the following clustering methods: ",tags$em("ward.D, single, complete, average, mcquitty, median, centroid.")),
                       
                       tags$li(tags$strong("Network: "), "This tab shows the network plot for the selected number of haplogroups and is computed using the pegas package. Four different network model methods can be selected: ",tags$em("Haplonet, Minimum Spanning Network (MSN), Randomized Minimum Spanning Tree (RMST), and Minimum Spanning Tree (MST)"),". In addition, fast plotting options can be selected for larger data sets.",
                               tags$ul(style="margin-top: 5px;",
                                       tags$li(tags$strong("Allele/haplogroup coloring: "), "Individual SNPs can be selected for allele coloring, as well as individual haplogroups for highlighting them in the network."),
                                       tags$li(tags$strong("Frequency: "), "Nodes can be scaled for their frequency."),
                                       tags$li(tags$strong("Plotting options: "), "Options for controlling the network plot size including the total plot size, the scale of the nodes and the node labels. Moreover, the color of the network can be changed."),
                                       tags$li(tags$strong("Number of edges: "), "A parameter can be set as threshold for additional edges to display."),
                                       tags$li(tags$strong("Edge weight: "), "The weight of the edges (number of genetic differences) can be set as lines, dots or numbers.")
                               )
                       ),
                       tags$li(tags$strong("PCA (principal component analysis): "),"Shown is a Principal component analysis plot showing the PC 1 and 2 for the selected number of haplogroups. The size of the individual dots is scaled to the frequency of samples in that particular haplogroup."),
                       tags$li(tags$strong("Dendrogram (hierarchical clustering): "),"Dendrogram showing the hierarchical clustering of the selected haplotypes. Different agglomeration methods are available including: ",tags$em("ward.D, single, complete, average, mcquitty, median, centroid.")),
                       tags$li(tags$strong("Allele heatmap (visual alignment): "),"Showing the respective alleles per haplogroup, depicted by the different colors.")
                     ),
                     p("All plots can be downloaded as PDF files."),
              ),
              column(8,
                     tabsetPanel(
                       id = "plotVideos",
                       type = "tabs",
                       tabPanel(
                         title = "Frequencies",
                         tags$div(
                           tags$video(
                             id = "video_5",
                             type = "video/mp4",
                             src = "https://raw.githubusercontent.com/TimHasenbein/PegasShiny/main/new_video6.mov", 
                             controls = "controls",
                             style = "width: 100%; max-width: 1500px; display: block; margin: 20px auto; border: 1px solid #ccc;")
                         )
                       ),
                       tabPanel(
                         title = "Distance matrix",
                         tags$div(
                           tags$video(
                             id = "video_6",
                             type = "video/mp4",
                             src = "https://raw.githubusercontent.com/TimHasenbein/PegasShiny/main/new_video7.mov", 
                             controls = "controls",
                             style = "width: 100%; max-width: 1500px; display: block; margin: 20px auto; border: 1px solid #ccc;")
                         )
                       ),
                       tabPanel(
                         title = "Network",
                         tags$div(
                           tags$video(
                             id = "video_7",
                             type = "video/mp4",
                             src = "https://raw.githubusercontent.com/TimHasenbein/PegasShiny/main/new_video8.mov", 
                             controls = "controls",
                             style = "width: 100%; max-width: 1500px; display: block; margin: 20px auto; border: 1px solid #ccc;")
                         )
                       ),
                       tabPanel(
                         title = "PCA",
                         tags$div(
                           tags$video(
                             id = "video_8",
                             type = "video/mp4",
                             src = "https://raw.githubusercontent.com/TimHasenbein/PegasShiny/main/new_video9.mov", 
                             controls = "controls",
                             style = "width: 100%; max-width: 1500px; display: block; margin: 20px auto; border: 1px solid #ccc;")
                         )
                       ),
                       tabPanel(
                         title = "Dendrogram",
                         tags$div(
                           tags$video(
                             id = "video_9",
                             type = "video/mp4",
                             src = "https://raw.githubusercontent.com/TimHasenbein/PegasShiny/main/new_video10.mov", 
                             controls = "controls",
                             style = "width: 100%; max-width: 1500px; display: block; margin: 20px auto; border: 1px solid #ccc;")
                         )
                       ),
                       tabPanel(
                         title = "Allele heatmap",
                         tags$div(
                           tags$video(
                             id = "video_10",
                             type = "video/mp4",
                             src = "https://raw.githubusercontent.com/TimHasenbein/PegasShiny/main/new_video11.mov", 
                             controls = "controls",
                             style = "width: 100%; max-width: 1500px; display: block; margin: 20px auto; border: 1px solid #ccc;")
                         )
                       )
                     )
              )
            )
          ),
          
          # functional annotation tab
          tabPanel(
            title = "3. Functional annotation",
            icon = icon("dna"),
            h2("3. Functional haplotype annotation"),
            p("This section provides functional annotation of the haplogroups. It was designed to annotate GWAS alleles, QTLs, and genomic elements. However, as long as the input files maintained the specified structure described in ", tags$strong("1.2 Data Upload "), "any genomic element can be annotated."),
            h3("Meta-data annotation"),
            p("The meta-data annotation can be used to enrich the visual analysis of your results. This function allows you to colorize the haplotype frequency bar plots and the genetic network plots according to sample traits, such as geographic location or phenotype. For this feature the structure of the meta-data file must adhere to the format described in Section 1.2 Data upload."),
            hr(style = "border-top: 1px solid #d0d0d0; margin: 30px 0;"),
            h3("3.1 GWAS annotation of haplotypes"),
            fluidRow(
              column(4,
                     p("The GWAS annotation tab panel provides insights into the risk or trait allele mapping across your inferred haplotypes, enabling the identification of trait-relevant haplogroups. The tab is structured into four distinct, interactive panels:"),
                     tags$ul(
                       tags$li(tags$strong("GWAS variants in haplogroups: "), "The table lists all GWAS-associated variants identified within the selected haplogroups. It provides information about the variant's genomic position (Pos), associated trait (Trait), the corresponding risk allele (Risk allele), the statistical significance (P-value), and the effect size (Effect size)."),
                       tags$li(tags$strong("Haplotype allele table (GWAS): "), "The table displays the specific alleles found at the GWAS or trait-associated variant positions for each inferred haplogroup. It serves as a visual alignment, confirming the allelic composition of each haplogroup for trait-related sites."),
                       tags$li(tags$strong("GWAS alleles mapping: "), "The heatmap shows the presence of risk alleles across the inferred haplogroups. Color-coded cells depict whether a risk allele is present in a particular haplogroup. The color itself denotes the associated trait."),
                       tags$li(tags$strong("GWAS effect sizes: "), "The plot visualizes the genomic positions of the GWAS variants against their effect sizes. The size of the nodes corresponds to the statistical significance, while the color denotes the SNP-trait association.")
                     ),
                     p("All tables and plots within this section can be dynamically filtered for a specific SNP-trait pair of interest. The network can be colored according to the selected associations, with the node size proportional to the number of present associations."),
              ),
              column(8,
                     tags$div(
                       tags$video(
                         id = "video_12",
                         type = "video/mp4",
                         src = "https://raw.githubusercontent.com/TimHasenbein/PegasShiny/main/new_video12.mov", 
                         controls = "controls",
                         style = "width: 100%; max-width: 1500px; display: block; margin: 20px auto; border: 1px solid #ccc;")
                     )
              )
            ),
            hr(style = "border-top: 1px solid #d0d0d0; margin: 30px 0;"),
            h3("3.2 QTL annotation of haplotypes"),
            fluidRow(
              column(4,
                     p("The QTL annotation tab panel provides insights into the QTL allele mapping across your inferred haplogroups, enabling the identification of regulatory-associated haplogroups. The tab is structured into four distinct, interactive panels:"),
                     tags$ul(
                       tags$li(tags$strong("QTL variants in haplogroups: "), "The table lists all QTL-associated variants identified within the selected haplogroups. It provides information about the variant's genomic position (Pos), the corresponding tissue and alleles, the posterior inclusion probability (PIP), and the effect sizes."),
                       tags$li(tags$strong("Haplogroup allele table (QTLs): "), "The table displays the specific alleles found at the QTL positions for each haplogroup."),
                       tags$li(tags$strong("QTL allele mapping: "), "The heatmap shows the presence of QTL alleles across the haplogroups. Color-coded cells depict whether an effect allele is present in a particular haplogroup. The color itself denotes the associated target gene per allele."),
                       tags$li(tags$strong("QTL effect sizes: "), "The plot visualizes the genomic positions of the QTL variants against their effect sizes. The color of the lollipop nodes corresponds to the target gene per allele, while the size of the nodes reflects the PIP of QTL variants.")
                     ),
                     p("All tables and plots within this section can be dynamically filtered for QTL-target pairs. The network can be colored according to the selected pairs, with the node size proportional to the number of present QTLs."),
              ),
              column(8,
                     tags$div(
                       tags$video(
                         id = "video_13",
                         type = "video/mp4",
                         src = "https://raw.githubusercontent.com/TimHasenbein/PegasShiny/main/new_video13.mov", 
                         controls = "controls",
                         style = "width: 100%; max-width: 1500px; display: block; margin: 20px auto; border: 1px solid #ccc;")
                     )
              )
            ),
            hr(style = "border-top: 1px solid #d0d0d0; margin: 30px 0;"),
            h3("3.3 Genomic annotation of haplogroups"),
            fluidRow(
              column(4,
                     p("The genomic annotation tab panel provides feature mapping of haplogroups, to identifiy if a variant overlaps a genomic feature. The tab is structured into four distinct, interactive panels:"),
                     tags$ul(
                       tags$li(tags$strong("Variants overlapping annotation: "), "The table lists all data variants of the haplogroups that overlap a genomic feature. It provides information about the variant's genomic position (Pos), name (ID), the type of regulatory element that it overlaps, and the position of the regulatory element itself."),
                       tags$li(tags$strong("Haplotype allele table (annotation): "), "The table displays the alleles of the variant that overlaps a genomic feature of the annotation."),
                       tags$li(tags$strong("Annotation mapping: "), "The heatmap shows the allele of a variant that overlaps a genomic feature. The color denotes the bases (A: green; C: blue; G: yellow; T: red)."),
                       tags$li(tags$strong("Count of VCF variants overlapping annotation features: "), "The plot visualizes the quantification of variants that overlap different genomic features. The color represents the distinct features.")
                     ),
                     p("All tables and plots within this section can be dynamically filtered for a specific feature of interest."),
              ),
              column(8,
                     tags$div(
                       tags$video(
                         id = "video_14",
                         type = "video/mp4",
                         src = "https://raw.githubusercontent.com/TimHasenbein/PegasShiny/main/new_video14.mov", 
                         controls = "controls",
                         style = "width: 100%; max-width: 1500px; display: block; margin: 20px auto; border: 1px solid #ccc;")
                     )
              )
            ),
            hr(style = "border-top: 1px solid #d0d0d0; margin: 30px 0;"),
          )
        ),
        tags$hr(style = "border-top: 1px solid #d0d0d0;"),
        
        # center panel
        fluidRow(
          column(width = 8, 
                 h3("Citation"),
                 p("When reporting results derived from the Haplofun web server, please cite the associated publication:"),
                 tags$blockquote(
                   em("Hasenbein TP., Bartels L., Stolze R., Wohlers I. (2026). Haplofun: Interactive haplotype analysis with integrated functional annotation. JOURNAL. DOI: [To be inserted upon publication]"),
                   style = "border-left: 5px solid #007bff; padding: 15px; margin: 15px 0; background-color: #eaf4ff; font-size: 1.1em;"
                 ),
                 p("Until the publication is available, please cite the web server URL and acknowledge the research group.")
          ),
          column(width = 4, 
                 h3("Contact & technical support"),
                 p("For technical support, bugs, or inquiries regarding data submission and collaboration, please contact the development team:"),
                 tags$ul(
                   tags$li(tags$strong("Contact:"), "Prof. Dr. Inken Wohlers"),
                   tags$li(tags$strong("Affiliation:"),"Biomolecular Data Science in Pneumology, Research Center Borstel, Germany"),
                   tags$li(tags$strong("Email:"), tags$a(href = "mailto:shiny-datascience@fz-borstel.de", "shiny-datascience@fz-borstel.de"), icon("envelope"))
                 ),
                 p(tags$a(href = "https://fz-borstel.de/de/forschung-am-fzb/wissenschaft-und-technologie/data-science-in-der-lungenforschung", target = "_blank", icon("external-link-alt"), tags$strong("Visit the lab homepage")))
          )
        ),
        
        # footer
        tags$footer(
          tags$style(HTML("
          .app-footer {
            font-size: 0.85em;
            color: #888; 
            text-align: center;
            padding-top: 10px; 
            padding-bottom: 5px;
            border-top: 1px solid #eee;
            width: 100%;
          }
        ")),
          class = "app-footer",
          p(
            "This web server is licensed under ",
            tags$a(href = "https://choosealicense.com/licenses/mit/", target = "_blank", "MIT"),
            " | ",
            actionLink(inputId = "goToHelpTab", label = "User guide & documentation"),
            " | ",
            HTML(paste0(" &copy; ", format(Sys.Date(), "%Y"), " Biomolecular Data Science in Pneumology, Research Center Borstel, Leibniz Lung Center, Germany. All Rights Reserved.")),
            style = "font-size: 1.2em"
          )
        )
      )
    )
  )
)
####---------------------------------------------------------------------####
