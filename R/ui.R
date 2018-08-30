      
      
      # This is the ui portion of a shiny app shows data from the ucar lab
      
      library(shiny)
      library(shinyjs)
      library(plotly)
      library(DT)
      library(shinythemes)
      library(rintrojs)
      library(shinyBS)
      library(shinyjqui)
      
      
      
      
      
      share <- list(
        title = "Genomic data",
        url = "http://ucarlab.org",
        image = "http://jem.rupress.org/content/early/2017/09/12/jem.20170416",
        description = "The chromatin accessibility signature of human immune aging stems from CD8+ T cells",
        twitter_user="Duygu"
      )
      
      jscode <- "
       shinyjs.disableTab = function(name) {
      var tab = $('.nav li a[data-value=' + name + ']');
      tab.bind('click.tab', function(e) {
      e.preventDefault();
      return false;
      });
      tab.addClass('disabled');
      }
      
      shinyjs.enableTab = function(name) {
      var tab = $('.nav li a[data-value=' + name + ']');
      tab.unbind('click.tab');
      tab.removeClass('disabled');
      }
      "
      
      css <- "
      .nav li a.disabled {
      background-color: #aaa !important;
      color: #333 !important;
      cursor: not-allowed !important;
      border-color: #aaa !important;
      }"
      fluidPage(
        useShinyjs(),
         extendShinyjs(text = jscode),
         inlineCSS(css),
        tags$style(type="text/css",
                   ".shiny-output-error { visibility: hidden; }",
                   ".shiny-output-error:before { visibility: visible;
        content: 'please select one more content to get the output  .'; }"
        ),
        navbarPage(" ", 
                   tabPanel("",id="345",icon = icon("fas fa-home"),
                            
                           
                            div(id = "headerSection", align="center",
                                h1(" UCAR LAB's"),
                               h2( "Human Immune Aging Server"),
                                
                                
                                # author info
                                span(
                                  style = "font-size: 1.2em",
                                  span("Created by "),
                                  a("UCAR LAB ", href = "http://ucarlab.org"),
                                  HTML("&bull;"),
                                  span("Code"),
                                  a("on GitHub", href = "https://github.com/UcarLab"),
                                  HTML("&bull;"),
                                  a("More apps", href = "http://ucarlab.org/?page_id=78"), "by UCAR LAB",
                                  br()
                                  
                                  
                                )
                            ),
                            
                            # show a loading message initially
                            
                            
                            # all content goes here, and is hidden initially until the page fully loads
                            HTML('<img src="images.jpeg" width="1400" height="200">'),# add the image related to chrmatin 
                           
                              
                              
                              mainPanel(
                               width=100,
                                
                                wellPanel(
                                  tabsetPanel(
                                    
                                    tabPanel(
                                      column(1, offset = 1,
                                    
                                      title = "", id = "tableTab_1",style='width: 1200px; height: 1050px', align="center",
                                  
                                      tags$b(helpText(h2("The chromatin accessibility signature of human immune aging stems from CD8+ T cells")),
                                             align="center", ( p(align="justify",("Aging in human is linked to deficiencies in immune responses and increased systemic inflammation. We explored the regulatory programs behind these changes through chromatin accessibility profiling and RNA-seq in PBMCs, purified monocytes, B cells, and T cells. Analysis of 77 samples, from young and elderly donors, revealed a novel and robust aging signature in PBMCs: simultaneous systematic chromatin closing at promoters and enhancers associated with T cell signaling as well as potentially stochastic chromatin opening mostly found at quiescent and repressed sites. Combined analyses of chromatin accessibility and RNA-seq uncovered immune molecules activated/inactivated with aging. We identified the silencing of the IL7R gene and the silencing of genes associated with IL-7 signaling, as potential biomarkers. This signature is borne by memory CD8+ T cells, which exhibited an aging-related loss in binding of NF-κB and STAT factors. Thus, our study provides a unique and comprehensive approach to identifying candidate biomarkers and provides mechanistic insights into aging- associated immunodeficiency.
      
                                                                                  ."))),
                                             br(),
                                             
                                           
                                             span(
                                               
                                               h3("About the Server "),
                                               tags$ol(
                                                 p(h4(" Several interactive options are available for users to visualize our data:")),
                                                 
                                                 
                                                 tags$li(align="justify",("Gene Section: in the gene section a user can select one or more genes from a drop-down menu, after selection of genes a user can view our data in table format which can help a user identify the number of peak locations associated with the selected gene(s).  We have created an option for a user if a user wishes to filter the data by Age criteria, a user can visualize through the plots,  "))),
                                               
                                                       tags$ol(type="a",
                                                       
                                                       tags$li(align="justify",("Heatmap: the heatmap depicts the normalized (z scores) of chromatin profiles for differentially closing/opening peaks across PBMC samples where a Sample ID is specific for each PBMC sample from the young and elderly donors.   ")),
                                                       
                                                       tags$li(align="justify",("Scatter plot:  the scatter plot presents RNA-seq vs ATAC-seq normalized (z scores) for gene expression and chromatin accessibility correlation among subjects.")),
                                                       tags$li(align="justify",("Boxplot:  the boxplot shows chromatin accessibility at promoters and gene expression changes with aging. "))), 
                                                       br(),
                                                       tags$ol(
                                                       tags$li(align="justify",("Annotation Section: in the annotation section a user can select a single, or multiple modules, and wikiPathways from our database through a drop-down menu."))), 
                                                       tags$ol(type="a",
                                                       tags$li(align="justify",("Heatmap: the heatmap displays subject-specific normalized (z scores) chromatin accessibility patterns of peaks annotated to genes in the module/wikipathways co-expression where the module reveals concerted aging-related variation across the cohort. Warmer (cooler) hues represent increased (decreased) chromatin accessibility relative to the cohort mean. "))),
                                                       tags$ol(
                                                       tags$li(align="justify",("Cell Section: In the cell section a user can select genes, modules, or wikipathways from our database through a drop-down menu. "))), 
                                                       tags$ol(type="a",
                                                       tags$li(align="justify",(" Boxplot: the boxplot shows cell type distribution of log2 fold changes for peak annotated to genes that are associated to closing & opening peaks in cell types (PBMC, CD4, CD8).   "))),
                                                       span(
                                                       style = "font-size: 1.3em",
                                                       span("Paper link :"),
                                                       a("  The chromatin accessibility signature of human immune aging stems from CD8+ T cells ", href = "http://jem.rupress.org/content/early/2017/09/12/jem.20170416"),
                                                
                                                       br(),
                                                       span("Lab website link : "),
                                                       a("UCAR LAB", href = "http://ucarlab.org"),
                                                        br(),
                                               
                                                 
                                                        br(),
                                                        br(),
                                                        br()
                                                       
                                          
                                                       
                                                      
                                                        
                                                      ))),
                                     
                                      
                                     
                                      
                                      
                                      DT::dataTableOutput("dataTable_1")
                                      
                                      )
                                    
                                  
                                    
                                     ) )
                                  
                                    )
                                  )
                            
                              
                            
                              ),
                   
                   
                   #########page 2 Gene starts here
                   tabPanel(id="plo2333",
                            
                            title = "Genes",
                            # add custom JS and CSS
                            singleton(
                              tags$head(
                                includeScript(file.path('www', 'message-handler.js')),
                                includeScript(file.path('www', 'helper-script.js')),
                                includeCSS(file.path('www', 'style.css')),
                                tags$link(rel = "shortcut icon", type="image/x-icon", href="https://i0.wp.com/ucarlab.org/wp-content/uploads/2017/10/Untitled1111111.jpg?resize=291%2C356"),
                                # Facebook OpenGraph tags
                                tags$meta(property = "og:title", content = share$title),
                                tags$meta(property = "og:type", content = "website"),
                                tags$meta(property = "og:url", content = share$url),
                                tags$meta(property = "og:image", content = share$image),
                                tags$meta(property = "og:description", content = share$description),
                                
                                # Twitter summary cards
                                tags$meta(name = "twitter:card", content = "summary"),
                                tags$meta(name = "twitter:site", content = paste0("@", share$twitter_user)),
                                tags$meta(name = "twitter:creator", content = paste0("@", share$twitter_user)),
                                tags$meta(name = "twitter:title", content = share$title),
                                tags$meta(name = "twitter:description", content = share$description),
                                tags$meta(name = "twitter:image", content = share$image)
                              )
                            ),
                           
                            
                            # enclose the header in its own section to style it nicer
                            
                            div(id = "headerSection",
                                introjsUI(),
                                
                                h1("Human Immune Aging Server"),
                                
                                
                                # author info
                                span(
                                  style = "font-size: 1.2em",
                                  span("Created by "),
                                  a("UCAR LAB ", href = "http://ucarlab.org"),
                                  HTML("&bull;"),
                                  span("Code"),
                                  a("on GitHub", href = "https://github.com/UcarLab"),
                                  HTML("&bull;"),
                                  a("More apps", href = "http://ucarlab.org/?page_id=78"), "by UCAR LAB"
                                  
                                  
                                ),
                                introBox(
                                  actionButton("help","Need Help",icon("eye"),style =  "color: #fff; background-color: gray; margin-right: 700px; margin-left: 1300px;border-color:aquamarine "),
                                  data.step = 1,
                                  data.intro = "Would You like to take a tour and understand all the features of this application"
                                )
                            ),
                            
                            # show a loading message initially
                            div(
                              id = "loadingContent",
                              h2("Loading...")
                            ),	
                            
                            # all content goes here, and is hidden initially until the page fully loads
                            hidden(div(id = "allContent",
                                       
                                       # sidebar - filters for the data
                                       sidebarLayout(
                                         sidebarPanel(
                                           h3("Filter data", style = "margin-top: 0;"),
                                            strong(span("Ages:")),
                                              textOutput("AgeText"), br(),
                                               introBox(
                                                uiOutput("AgeUi"),
                                              data.step = 4,
                                             data.intro = "here you could select Age range "
                                           ),
                                           br(),
                                           
                                           
                                           actionButton("showData","show data ",icon("paper-plane"),style =  "color: #fff;background-color: gray; margin-right: 100px; margin-left: 270px;border-color:aquamarine"),
                                           # what variables to show
                                           introBox(
                                             uiOutput("variablesUi"),
                                             data.step = 2,
                                             data.intro = "once you select a gene from a dropdown menu of Genes , you can click on this button it will show you , data in a table as well as plot format "
                                           ),
                                           bsTooltip("variablesUi", "The drop down menu lists only genes in the data base that have both an ATAC and RNA score.",
                                                     "right", options = list(container = "body")),
                                           uiOutput("variables1Ui"),
                                           uiOutput("variables2Ui"),
                                           br(),
                                           
                                           
                                           # button to update the data
                                           shiny::hr(),
                                           #actionButton("updateBtn", "Update Data"),
                                          
                                           # footer - where the data was obtained
                                           br(), br(), br(), br(),
                                           p("Data was obtained from ",
                                             a("the ucar lab",
                                               href = "http://ucarlab.org",
                                               target = "_blank")),
                                           a(img(src = "ucar lab.png",width="200", height="60", alt = "ucar lab"),
                                             href = "http://ucarlab.org",
                                             target = "_blank")
                                          
                                           
                                         ),
                                         
                                         
                                         # main panel has two tabs - one to show the data, one to plot it
                                         mainPanel(
                                           
                                           
                                           
                                           
                                           wellPanel(
                                             
                                             tabsetPanel(
                                               id = "resultsTab", type = "tabs",
                                               tabPanel(
                                                 title = "Table", id = "tableTab",
                                                 
                                                 br(),
                                                 downloadButton("downloadData", "Download table",style =  "color: #fff;background-color: gray;border-color:aquamarine"),
                                                 br(), br(),
                                                 
                                                
                                                 br(),
                                                 
                                                 introBox(
                                                   DT::dataTableOutput("dataTable"),
                                                   data.step = 3,
                                                   data.intro = "Here you could see data in a  table format there are two available here long and wide"
                                                 )
                                               
                                          
                                                 ),
                                               
                                               # tab showing the data in table format
                                               
                                               
                                               # tab showing the data as plots
                                             
                                               tabPanel(
                                              
                                           
                                                 title = "ATAC-seq", id = "plotTab",
                                                 br(),
                                                # actionButton("button2","?",icon("eye"),style =  "color: #fff; background-color: gray;border-color:aquamarine "),
                                                 
                                                 downloadButton("downloadPlot", "Save figure",style =  "color: #fff;background-color: gray;border-color:aquamarine"),
                                                 br(), br(),
                                                 bsTooltip("dataPlot", "hover anywhere on a panel to get information related to a plot",
                                                           "right", options = list(container = "body")), 
                                                               
                                                           jqui_resizable( plotOutput("dataPlot")),
                                              
                                                              width=8 
                                               
                                                  ),
                                               
                                               
                                               tabPanel(
                                                 title = "RNA-seq vs ATAC-seq ", id = "plotTab9",
                                                 br(),
                                                 downloadButton("downloadPlot8", "Save figure",style =  "color: #fff;background-color: gray;border-color:aquamarine"),
                                                 br(), br(),
                                                 
                                                 bsTooltip("Rna_AtacPlot_single", "hover anywhere on a panel to get information about plot",
                                                           "right", options = list(container = "body")),    
                                                   
                                                jqui_resizable(plotOutput("Rna_AtacPlot_single")),
                                                 
                                                 br(), br(),
                                                
                                                jqui_resizable(plotOutput("Rna_AtacPlot"))
                                              
                                               ),
                                               
                                               tabPanel(
                                                 title = "Boxplot", id = "plotTab33",
                                                 br(),
                                                 downloadButton("downloadPlot9", "Save figure",style =  "color: #fff;background-color: gray;border-color:aquamarine"),
                                                 br(),br(),
                                                 
                                                  br(), br(),
                                                   jqui_resizable(plotOutput("AtacPlot_1")),
                                                   bsTooltip("AtacPlot_1", "hover anywhere on a panel to get information related to a plot",
                                                           "right", options = list(container = "body")), 
                                                     br(),
                                                     br(),
                                                     br(),
                                                 
                                                 jqui_resizable(   plotOutput("RNAPlot_1"))
                                                 
                                                 
                                               )
                                  
                                                 )
                                                  ) 
                                                    )
                                         
                                                      )
                                                         )
                                   
                                                            )
                                                               ),
                   ###### page 3 starts here Annotaion
                   
                   tabPanel("Annotations",id="933",
                            singleton(
                              tags$head(
                                includeScript(file.path('www', 'message-handler.js')),
                                includeScript(file.path('www', 'helper-script.js')),
                                includeCSS(file.path('www', 'style.css')),
                                tags$link(rel = "shortcut icon", type="image/x-icon", href="https://i0.wp.com/ucarlab.org/wp-content/uploads/2017/10/Untitled1111111.jpg?resize=291%2C356"),
                                # Facebook OpenGraph tags
                                tags$meta(property = "og:title", content = share$title),
                                tags$meta(property = "og:type", content = "website"),
                                tags$meta(property = "og:url", content = share$url),
                                tags$meta(property = "og:image", content = share$image),
                                tags$meta(property = "og:description", content = share$description),
                                
                                # Twitter summary cards
                                tags$meta(name = "twitter:card", content = "summary"),
                                tags$meta(name = "twitter:site", content = paste0("@", share$twitter_user)),
                                tags$meta(name = "twitter:creator", content = paste0("@", share$twitter_user)),
                                tags$meta(name = "twitter:title", content = share$title),
                                tags$meta(name = "twitter:description", content = share$description),
                                tags$meta(name = "twitter:image", content = share$image)
                              )
                            ),
                            div(id = "headerSection",
                                h1("Human Immune Aging Server"),
                                
                                
                                # author info
                                span(
                                  style = "font-size: 1.2em",
                                  span("Created by "),
                                  a("UCAR LAB ", href = "http://ucarlab.org"),
                                  HTML("&bull;"),
                                  span("Code"),
                                  a("on GitHub", href = "https://github.com/UcarLab"),
                                  HTML("&bull;"),
                                  a("More apps", href = "http://ucarlab.org/?page_id=78"), "by UCAR LAB",
                                  br(),
                                  
                                  span("November 21, 2017")
                                )
                            ),
                            
                            # show a loading message initially
                            
                            # all content goes here, and is hidden initially until the page fully loads
                            # sidebar - filters for the data
                            sidebarLayout(
                              sidebarPanel(
                                h5(" ", style = "margin-top: 5px;
                                   
                                   margin-right: 20px;
                                   margin-left: 20px;"),
                                
                                
                                h3("Filter Data ", style = "margin-top: 0;"),
                                br(),
                                
                                actionButton("showData_2","show data ",icon("paper-plane"),style =  "color: #fff;background-color: gray; margin-right: 100px; margin-left: 270px;border-color:aquamarine"),
                                
                                
                                actionButton("btton","",icon("fas fa-unlock-alt"),style =  "color: #fff;background-color: gray; margin-right: 180px; margin-left: 340px;border-color:aquamarine"),
                                
                                uiOutput("variables5Ui"),
                                uiOutput("variables6Ui"),
                                uiOutput("variables69Ui")
                                
                                
                                ),
                              
                              mainPanel(
                                
                                
                                wellPanel(
                                  tabsetPanel(
                                    tabPanel(
                                      title = "Table", id = "tableTab_2",
                                      
                                      br(),
                                      downloadButton("downloadData_2", "Download table"),
                                      br(), br(),
                                      br(),
                                      
                                      DT::dataTableOutput("dataTable_2")
                                      
                                    ),
                                    
                                    
                                    tabPanel(
                                      title = "ATAC-seq    ", id = "plotTab6399",
                                      br(),
                                      downloadButton("downloadPlot393", "Save figure",style =  "color: #fff;background-color: gray;border-color:aquamarine"),
                                      br(), br(),
                                      jqui_resizable( plotOutput("heat_pathways")),
                                      br(),
                                      br(),
                                      br(),
                                      br(),
                                      br(),
                                      br(),
                                      br(),
                                      br(),
                                      br(),
                                      jqui_resizable( plotOutput("ATAC_pca_ui"))
                                      
                                    ),
                                    tabPanel(
                                      title = "RNA-seq    ", id = "plotTab6999",
                                      br(),
                                      downloadButton("downloadPlot396663", "Save figure",style =  "color: #fff;background-color: gray;border-color:aquamarine"),
                                      br(), br(),
                                      jqui_resizable( plotOutput("PCA_RNA_heat_pathways_1")),
                                      br(),
                                      br(),
                                      br(),
                                      br(),
                                      br(),
                                      br(),
                                      br(),
                                      br(),
                                      br(),
                                      jqui_resizable( plotOutput("RNA_pca_ui")),
                                      br()
                                      
                                      
                                    ),
                                    tabPanel(
                                     
                                      title = "Boxplot    ", id = "plotTab63993",
                                      br(),
                                      
                                      
                                      downloadButton("downloadPlot1332", "Save figure",style =  "color: #fff;background-color: gray;border-color:aquamarine"),
                                      br(), br(),
                                      
                                      jqui_resizable(plotOutput("small_box_cell_type_data2")), #boxplot
                                      br(),
                                      br(),
                                      br(),
                                      jqui_resizable(plotOutput("box_cell_pathway")),  # boxplot with facets 
                                      br()
                                      
                                      
                                    )
                                    
                                    
                                      )
                                )
                              )
                              )
                            ),
                   
                   
                   #### page 4 Cell type starts here
                   tabPanel("Cell types",id="100",
                            # author info
                            
                            singleton(
                              tags$head(
                                includeScript(file.path('www', 'message-handler.js')),
                                includeScript(file.path('www', 'helper-script.js')),
                                includeCSS(file.path('www', 'style.css')),
                                tags$link(rel = "shortcut icon", type="image/x-icon", href="https://i0.wp.com/ucarlab.org/wp-content/uploads/2017/10/Untitled1111111.jpg?resize=291%2C356"),
                                # Facebook OpenGraph tags
                                tags$meta(property = "og:title", content = share$title),
                                tags$meta(property = "og:type", content = "website"),
                                tags$meta(property = "og:url", content = share$url),
                                tags$meta(property = "og:image", content = share$image),
                                tags$meta(property = "og:description", content = share$description),
                                
                                # Twitter summary cards
                                tags$meta(name = "twitter:card", content = "summary"),
                                tags$meta(name = "twitter:site", content = paste0("@", share$twitter_user)),
                                tags$meta(name = "twitter:creator", content = paste0("@", share$twitter_user)),
                                tags$meta(name = "twitter:title", content = share$title),
                                tags$meta(name = "twitter:description", content = share$description),
                                tags$meta(name = "twitter:image", content = share$image)
                              )
                            ),
                            div(id = "headerSection",
                                h1("Human Immune Aging Server"),
                                
                                
                                # author info
                                span(
                                  style = "font-size: 1.2em",
                                  span("Created by "),
                                  a("UCAR LAB ", href = "http://ucarlab.org"),
                                  HTML("&bull;"),
                                  span("Code"),
                                  a("on GitHub", href = "https://github.com/UcarLab"),
                                  HTML("&bull;"),
                                  a("More apps", href = "http://ucarlab.org/?page_id=78"), "by UCAR LAB",
                                  br(),
                                  
                                  span("November 21, 2017")
                                )
                            ),
                            sidebarLayout(
                              sidebarPanel(
                                
                                h3("Cell types "),
                                
                                
                                actionButton("showData_9","show data ",icon("paper-plane"),style =  "color: #fff;background-color: gray; margin-right: 100px; margin-left: 270px;border-color:aquamarine"),
                             
                                actionButton("BTTN","",icon  ("fas fa-unlock-alt"),style =  "color: #fff;background-color: gray; margin-right: 100px; margin-left: 270px;border-color:aquamarine"),
                                
                                
                                uiOutput("variables9Ui"),
                                uiOutput("variables99Ui"),
                                uiOutput("variables999Ui"),
                                uiOutput("variables9999Ui")
                                
                              ),
                              
                              
                              mainPanel(
                                
                                
                                wellPanel(
                                  tabsetPanel(
                                    tabPanel(
                                       title = "Table", id = "tableTab_3",
                                       
                                    br(),
                                       downloadButton("downloadData_3", "Download table",style =  "color: #fff;background-color: gray;border-color:aquamarine"),
                                       br(), br(),
                                    
                                       br(),
                                       
                                       DT::dataTableOutput("dataTable_3")
                                       
                                     ),
                                    
                                    
                                    
                                     tabPanel(
                                       
                                       title = "ATAC-seq_", id = "plotTab9333333",
                                       br(),
                                       
                                       
                                       downloadButton("downloadPlot9333333", "Save figure",style =  "color: #fff;background-color: gray;border-color:aquamarine"),
                                       br(), br(),
                                       
                                       
                                       jqui_resizable( plotOutput("box_cell_type_data9333333")),
                                       br(),
                                       br(),
                                       br(),
                                       br(),
                                       br(),
                                       jqui_resizable(plotOutput("box_cell_type_data9333333three")),
                                       br(),
                                       br(),
                                       br(),
                                       br(),
                                       br(),
                                       jqui_resizable( plotOutput("box_cell_type_data9333333three1")),
                                       br(),
                                       br(),
                                       br(),
                                       br(),
                                       br(),
                                       jqui_resizable(   plotOutput("box_cell_type_data9333333three2")),
                                       br(),
                                       br(),
                                       br(),
                                       br(),
                                       br(),
                                       jqui_resizable( plotOutput("box_cell_type_data9333333three3")),
                                       br(),
                                       br(),
                                       br(),
                                       br(),
                                       br()
                                     ),
                                    
                                     tabPanel(
                                        
                                       title = "Boxplot    ", id = "plotTab99999",
                                      br(),
                                        
                                        
                                        downloadButton("downloadPlot3993", "Save figure",style =  "color: #fff;background-color: gray;border-color:aquamarine"),
                                        br(), br(),
                                        
                                        
                                        jqui_resizable(  plotOutput("box_cell_type_data"))
                                        
                                        
                                      )
                                    
                                   
                                    
                                  )
                                  
                                )
                              )
                            )
                            
                            
                   ),
                   tags$style(type = 'text/css', 
                              HTML('.navbar { background-color: aquamarine; font-family:verdana;font-size: 23px}
                                   .navbar-default .navbar-brand{color: white;}
                                   .tab-panel{ background-color: blue; color: aquamarine}
                                   .navbar-default .navbar-nav > .active > a, 
                                   .navbar-default .navbar-nav > .active > a:focus, 
                                   .navbar-default .navbar-nav > .active > a:hover {
                                   color: #555;
                                   background-color: azure;
      
                                   }')
                   )
                   
                              ))
