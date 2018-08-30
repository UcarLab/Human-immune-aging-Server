            
            
            # This is the server portion of a shiny app shows ucar lab data
            
            
            source("helpers.R")  # have the helper functions avaiable
            
            library(shiny)
            library(magrittr)
            library(plyr)
            library(dplyr)
            library(tidyr)
            library(ggplot2)
            library(shinyjs)
            library(DT)
            library(d3heatmap)
            library(ggvis)
            library(plotly)
            library(circlize)
            library(ComplexHeatmap)
            library(shinyBS)
            library(pheatmap)
            library(rintrojs)
            library(shinythemes)
            library(highcharter)
            library(wakefield)
            library(tibble)
            library(FactoMineR)
            library(shinyjqui)
            library(shinyjqui)
            library(rsconnect)
            
            rsconnect::setAccountInfo(name='jagmagma369',
                                      token='A6ACBA6AC89B646B995A42C0A51F718E',
                                      secret='1gezQSF7rmx3Gs0h6byAk2an/h/izb3yzuLTsNmV')
            
                                    
            
                    # Get the raw data
                    cDatRaw <- getData()
                    cDatRaw_pathway <- getData_pathway()
                    #cDatRaw_module <- getData_module()
                    cDatRaw_cell <- getData_cell()
                    # Get the list of colours to use for plotting
                    plotCols <- getPlotCols()
            
                    shinyServer(function(input, output, session) {
              
              
                    observeEvent(input$help,
                    introjs(session, options = list("nextLabel"="Onwards and Upwards"),
                    events = list("oncomplete"='alert("It is over")')))
            
              
              
              
            	
                        	# =========== BUILDING THE INPUTS ===========
            	
            	
            
            # Create select box input to choose variables to show
            	output$variablesUi <- renderUI({
            		selectizeInput("variablesSelect", "Select a Gene :",
            									 unique(as.character(cDatRaw$Gene_name)),
            									 selected = NULL, multiple = TRUE,
            									 options = list(placeholder = "search by Gene_name")) 
            	})	
            	addPopover(session, "variablesUi", "Data", content = paste0("The drop down menu lists only genes in the data base that have both an ATAC and RNA score."), trigger = 'hover')
            
            	output$variables5Ui <- renderUI({
            	  selectizeInput("variablesSelect_5", "   Select  a Module  ",
            	                 unique(as.character(cDatRaw_pathway$Module.Name)),
            	                 selected = NULL, multiple = TRUE,
            	                 options = list(placeholder = "search by Module ")) 
            	})
            	output$variables6Ui <- renderUI({
            	  selectizeInput("variablesSelect_6", "    Select  a Pathway ",
            	                 unique(as.character(cDatRaw_pathway$Pathway_name)),
            	                 selected = NULL, multiple = TRUE,
            	                 options = list(placeholder = "search by Pathway "))  
            	})
            	
            	output$variables69Ui <- renderUI({
            	  selectizeInput("variablesSelect_699", "    Select  a Cell type ",
            	                 unique(as.character(cDatRaw_pathway$Single_cell)),
            	                 selected = NULL, multiple = TRUE,
            	                 options = list(placeholder = "search by cell_type ")) 
            	})
            	
            	
            	
            	
            	output$variables9Ui <- renderUI({
            	  selectizeInput("variablesSelect_9", "    Select  a gene ",
            	                
            	             unique(as.character((cDatRaw_cell %>% filter(Cell_Type=="PBMC"))$Gene_name)),
            	                 selected = NULL, multiple = TRUE,
            	                 options = list(placeholder = "search by gene in cell ")) 
            	})
            	output$variables99Ui <- renderUI({
            	  selectizeInput("variablesSelect_99", "    Select  a module ",
            	                 unique(as.character(cDatRaw_cell$Module.Name)),
            	                 selected = NULL, multiple = TRUE,
            	                 options = list(placeholder = "search by module in cell ")) 
            	})
            	output$variables999Ui <- renderUI({
            	  selectizeInput("variablesSelect_999", "    Select  a pathway ",
            	                 unique(as.character(cDatRaw_cell$Pathways)),
            	                 selected = NULL, multiple = TRUE,
            	                 options = list(placeholder = "search by Pathways in cell ")) 
            	})
            	output$variables9999Ui <- renderUI({
            	  selectizeInput("variablesSelect_9999", "    Select  a direction ",
            	                 unique(as.character(cDatRaw_cell$Sign)),
            	                 selected = NULL, multiple = TRUE,
            	                 options = list(placeholder = "search by Sign in cell "))
            	})
            	
            	
             
            	 value <- reactive(input$variablesSelect_9)
            	 
            	 observeEvent(value(), {
            	   if(length(value())==1){
            	     
            	     js$disableTab('ATAC-seq_')
            	    
               }  
            	   
            	   
            	   if(length(value())>=2){
            	   js$enableTab('ATAC-seq_')
            
            	 }
            	   
            	 })
            	 
            	abc <- reactive(input$variablesSelect_99)
            	 
            	 observeEvent(abc(), {
            	   
            	   js$enableTab('ATAC-seq_')
            	   
            	 })
            	 
            	 jkl <- reactive(input$variablesSelect_999)
            	 
            	 observeEvent(jkl(), {
            	   
            	  
            	   js$enableTab('ATAC-seq_')
            	   
            	   
            	 })
            	
            	lmn<-reactive(input$variablesSelect_5)
            	stu<-reactive(input$variablesSelect_6)
            	xyx<-reactive(input$variablesSelect_699)
            	bbtt<-reactive(input$btton)
            	
            	
            	
            	observeEvent(bbtt(), {
            	  
            	  shinyjs::enable('variablesSelect_5')
            	  shinyjs::enable('variablesSelect_6')
            	  shinyjs::enable('variablesSelect_699')
            	  
            	  
            	})
            	
            	
            	observeEvent(lmn(),{
            	  if(length(lmn())>=1){
            	    shinyjs::disable('variablesSelect_699')
            	    shinyjs::disable('variablesSelect_6')
            	  }
            	  if(length(lmn())==0){
            	    shinyjs::enable('variablesSelect_699')
            	    shinyjs::enable('variablesSelect_6')
            	  }
            	  
            	}
            	)
            	
            	observeEvent(stu(),{
            	  if(length(stu())>=1)
            	    shinyjs::disable('variablesSelect_5')
            	  shinyjs::disable('variablesSelect_699')
            	})
            	
            	observeEvent(xyx(),{
            	  if(length(xyx())>=1)
            	    shinyjs::disable('variablesSelect_5')
            	  shinyjs::disable('variablesSelect_6')
            	})
            	
            	
            	
            	
            	
            	
                      	###################
                      	
                      	lmnO<-reactive(input$variablesSelect_9)
                      	stuO<-reactive(input$variablesSelect_99)
                      	xyxO<-reactive(input$variablesSelect_999)
                      	bbttO<-reactive(input$BTTN)
                      	
                      	###################
            	
            	observeEvent(	bbttO(), {
            	  
            	  shinyjs::enable('variablesSelect_9')
            	  shinyjs::enable('variablesSelect_99')
            	  shinyjs::enable('variablesSelect_999')
            	  
            	  
            	})
            	
                      	###################
                      	observeEvent(lmnO(),{
                      	  if(length(lmnO())>=1){
                      	    shinyjs::disable('variablesSelect_999')
                      	    shinyjs::disable('variablesSelect_99')
                      	  }
                      	  
                      	  
                      	}
                      	)
                      	
                      	observeEvent(stuO(),{
                      	  if(length(stuO())>=1)
                      	    shinyjs::disable('variablesSelect_9')
                      	  shinyjs::disable('variablesSelect_999')
                      	})
                      	
                      	observeEvent(xyxO(),{
                      	  if(length(xyxO())>=1)
                      	    shinyjs::disable('variablesSelect_9')
                      	  shinyjs::disable('variablesSelect_99')
                      	})
            	
            	
            	
            	
            	
            	
            	# Show the Ages selected (because of the bugs in the slider mentioned below)
            	output$AgeText <- renderText({
            		if (is.null(input$Age)) {
            			return(formatAgeText(range(cDatRaw$Age)))
            		}
            		
            		formatAgeText(input$Age)
            	})	
            	
            	# Create slider for selecting Age range
            	
            	output$AgeUi <- renderUI({
            		sliderInput("Age", 
            								label = "",
            								min = min(cDatRaw$Age), max = max(cDatRaw$Age),
            							
            								value = c(22,93),
            								step = 1)
            	})
            	
            	
            	# ============== MANIPULATE THE DATA ================
            
            	# The dataset to show/plot, which is the raw data after filtering based on
            	# the user inputs
                      	cDat <- reactive({
                      		# Add dependency on the update button (only update when button is clicked)
                      		input$updateBtn	
                      		
                      		# If the app isn't fully loaded yet, just return the raw data 
                      		if (!dataValues$appLoaded) {
                      			return(cDatRaw)
                      		}
                      		if (input$showData==0)
                      	
                      		  return("select a gene _name then it will show data")
                      	 
                      	    isolate({ 
                      		data <- cDatRaw
                      		
                      		# Add all the filters to the data based on the user inputs
                      		# wrap in an isolate() so that the data won't update every time an input
                      		# is changed
                      		isolate({
                      			
                      			# Filter age
                      			data %<>%
                      				filter(Age >= input$Age[1] & Age <= input$Age[2])
                      			
                      			# Filter what gene_name to show
                      			if (!is.null(input$variablesSelect)) {
                      				data %<>%
                      					filter(Gene_name %in% input$variablesSelect)
                      			}
                      			
                      		  # Filter what module or cell to show
                      		   if (!is.null(input$variablesSelect_1)) {
                      		     data %<>%
                      		       filter(Module.Name %in% input$variablesSelect_1)
                      		   }
                      		  if (!is.null(input$variablesSelect_5)) {
                      		    data %<>%
                      		      filter(Module.Name %in% input$variablesSelect_5)
                      		  }
                      		  if (!is.null(input$variablesSelect_2)) {
                      		    data %<>%
                      		      filter(Pathway_name %in% input$variablesSelect_2)
                      		  }
                      		
                      			# See if the user wants to show data per  type or all combined
                      			# if (input$showGrouped) {
                      			# 	data %<>%
                      			# 		group_by(Age, Gene_name,peakco) %>%
                      			# 		summarise(values =
                      			# 								ifelse(Gene_name[1] != "OR4F5",
                      			# 											 sum(values),
                      			# 											 mean(values))) %>%
                      			# 		ungroup %>%
                      			# 		data.frame
                      			# }
                      		})
                      
                      		data
                      	})
                      	})
                      	
            	
                  	#data for pathways & Module
                    	cDat2 <- reactive({
                    	  # Add dependency on the update button (only update when button is clicked)
                    	  input$updateBtn	
                    	  
                    	  # If the app isn't fully loaded yet, just return the raw data 
                    	  if (!dataValues$appLoaded) {
                    	    return(cDatRaw_pathway)
                    	  }
                    	  if (input$showData_2==0)
                    	    
                    	    return("select a pathway then it will show data")
                    	  
                    	 
                    	    data <- cDatRaw_pathway 
                    	    
                    	    # Add all the filters to the data based on the user inputs
                    	    # wrap in an isolate() so that the data won't update every time an input
                    	    # is changed
                    	    isolate({
                    	      
                    	      # data %<>%
                    	      #   filter(atac_FDR >= input$atac_FDR[1] &atac_FDR <= input$atac_FDR[2])
                    	      
                    	      if (!is.null(input$variablesSelect_5)) {
                    	        data %<>%
                    	          filter(Module.Name %in% input$variablesSelect_5)
                    	      }
                    	      
                    	      if (!is.null(input$variablesSelect_6)) {
                    	        data %<>%
                    	          filter(Pathway_name %in% input$variablesSelect_6)
                    	      }
                    	      
                    	      if (!is.null(input$variablesSelect_699)) {
                    	        data %<>%
                    	          filter(Single_cell %in% input$variablesSelect_699)
                    	      }
                    	      
                    	    })
                    	    
                    	    data
            	
            	                })
                    	
                    	
                                	cDat3 <- reactive({
                                	  # Add dependency on the update button (only update when button is clicked)
                                	  input$updateBtn	
                                	  
                                	  # If the app isn't fully loaded yet, just return the raw data 
                                	  if (!dataValues$appLoaded) {
                                	    return(cDatRaw_cell)
                                	  }
                                	  if (input$showData_9==0)
                                	    
                                	    return("select a gene then it will show data")
                                	  
                                	  
                                	  data <- cDatRaw_cell 
                                	  
                                	  # Add all the filters to the data based on the user inputs
                                	  # wrap in an isolate() so that the data won't update every time an input
                                	  # is changed
                                	  isolate({
                                	    
                                	    if (!is.null(input$variablesSelect_9)) {
                                	  
                                	     
                                	      PBMC_gene_positive<-unique(as.character((data %>% filter(Cell_Type=="PBMC"))$Gene_name))
                                	      data %<>%filter(Gene_name %in% PBMC_gene_positive) %<>%
                                	      filter(Gene_name %in% input$variablesSelect_9)
                                	    }
                                	    if (!is.null(input$variablesSelect_99)) {
                                	   
                                	      PBMC_module_positive<-unique(as.character((data %>% filter(Cell_Type=="PBMC",FDR<0.05,logFC>0))$Module.Name))
                                	      data %<>%filter(Module.Name %in% PBMC_module_positive) %<>%
                                	        filter(Module.Name %in% input$variablesSelect_99)
                                	    }
                                	    if (!is.null(input$variablesSelect_999)) {
                                	     
                                	      data %<>% 
                                	        filter(Pathways %in% input$variablesSelect_999)
                                	    }
                                	    if (!is.null(input$variablesSelect_9999)) {
                                	     
                                	      pbmc_pos<-unique(as.list((data %>% filter(Cell_Type=="PBMC"))$Sign))
                                	       data %<>%filter(Sign %in% pbmc_pos) %<>%
                              
                                	     filter(Sign %in% input$variablesSelect_9999)
                                	     
                                	    }
                                	  })
                                	  
                                	  data
                                	  
                                	})
                            #The data to show in a table, which is essentially the same data as above
                          	# with all the filters, but formatted differently:
                          	# - Format the numbers to look better in a table
                          	# - Change the data to wide/long format (the filtered data above is long)
                          	cDatTable <- reactive({
                          	  if(input$showData==0)
                          	    return()
                          	  isolate({   
                          		data <- cDat()
                          		# In numeric columns show 2 digits past the decimal and don't show
                          		# decimal if the number is a whole integer
                          		data %<>%
                          			mutate(values = formatC(data$values, format = "fg", digits = 2))		
                          		
                          	  })
                          		data
                          	})
            	                 ######logic for table starts here  ######
            
                          	cDatTable_2 <- reactive({
                          	  if(input$showData_2==0)
                          	    return()
                          	  isolate({   
                          	    data <- cDat2()
                          	   
                          	    data %<>%
                          	      mutate(values = formatC(data$values, format = "fg", digits = 2))		
                          	    
                          	   
                          	  })
                          	  data
                          	})
                          	
            	
            	
                          	cDatTable_3 <- reactive({
                          	  if(input$showData_9==0)
                          	    return()
                          	  isolate({   
                          	    data <- cDat3()
                          	   
                          	    data %<>%
                          	      mutate(values = formatC(data$values, format = "fg"))		
                          
                          	  })
                          	  data
                          	})
            
                          	
                          		# ============= TAB TO SHOW data IN TABLE 	# =============
            
                          	# Show the data1 in a table
                          	output$dataTable_1 <- DT::renderDataTable(
                          	  {
                          	     cDatTable_2()
                          		  # browser()
                          		} , options=list(bSortClasses=TRUE)
                          	
                          	)
                          	
                          	# Allow user to download the data1, simply save as csv
                          	output$downloadData_1 <- downloadHandler(
                          		filename = function() { 
                          			"visualx.csv"
                          		},
                          		
                          		content = function(file) {
                          			write.table(x = cDatTable_2(),
                          									file = file,
                          									quote = FALSE, sep = ",", row.names = FALSE)
                          		}
                          	)	
                          	
                          	#  ============= TAB TO SHOW data IN table1	# =============
                          	output$dataTable <- DT::renderDataTable(
                          	  {
                          	    cDatTable()
                          	    
                          	  } , options=list(bSortClasses=TRUE)
                          	  
                          	)
                          	
                          	# Allow user to download the data1, simply save as csv
                          	output$downloadData <- downloadHandler(
                          	  filename = function() { 
                          	    "visualx.csv"
                          	  },
                          	  
                          	  content = function(file) {
                          	    write.table(x = cDatTable(),
                          	                file = file,
                          	                quote = FALSE, sep = ",", row.names = FALSE)
                          	  }
                          	)	
                          
                          	
                      	
                      	#  ============= TAB TO SHOW data IN table2	# =============
                      	output$dataTable_2 <- DT::renderDataTable(
                      	  {
                      	    cDatTable_2()
                      	  
                      	  } , options=list(bSortClasses=TRUE)
                      	  
                      	)
                      	
                      	# Allow user to download the data1, simply save as csv
                      	output$downloadData_2 <- downloadHandler(
                      	  filename = function() { 
                      	    "visualx.csv"
                      	  },
                      	  
                      	  content = function(file) {
                      	    write.table(x = cDatTable_2(),
                      	                file = file,
                      	                quote = FALSE, sep = ",", row.names = FALSE)
                      	  }
                      	)	
                      	
            	
                      	#  ============= TAB TO SHOW data IN table3	# =============
                      	output$dataTable_3 <- DT::renderDataTable(
                      	  {
                      	    cDatTable_3()
                      	   
                      	  } , options=list(bSortClasses=TRUE)
                      	  
                      	)
                      	
                      	# Allow user to dow-2nload the data1, simply save as csv
                      	output$downloadData_3 <- downloadHandler(
                      	  filename = function() { 
                      	    "visualx.csv"
                      	  },
                      	  
                      	  content = function(file) {
                      	    write.table(x = cDatTable_3(),
                      	                file = file,
                      	                quote = FALSE, sep = ",", row.names = FALSE)
                      	  }
                      	)	
                      	
                      	
            	
            	
            	#============= =============	============= ============= ============= =============         	
                      	#  ============= TAB TO SHOW data IN plot 	# =============
              #============= =============	============= ============= ============= =============         	
            	
            	                      buildPlot <- reactive({
            	  
            	  
                                	  hintjs(session, options = list("hintButtonLabel"="exit"))
                                	  abcd=cDat()
                                	  yy=abcd %>% group_by(peakco,ID) %>% arrange(peakco,-abs(values)) %>% slice(1) %>% ungroup()
                                	  
                                	  annot_df1<-data.frame(yy %>% distinct(ID,TYPE),row.names = "ID")
                                	  annot_df2<-data.frame(yy %>% distinct(peakco,ChromeState),row.names = "peakco")
                                	  
                                	  ha_colum<-HeatmapAnnotation(annot_df1,col = list(TYPE=c("HO"="deeppink3","HY"="darkturquoise")),annotation_legend_param = list(Legend_direction = "horizontol",legend_width = unit(5, "cm"),title = "AGE GROUP", title_gp = gpar(fontsize = 10,fontface="bold"), labels_gp = gpar(fontsize = 10)))
                                	  ha_row<-rowAnnotation(annot_df2,col=list(ChromeState=c("Tx"="green4","Quies"="bisque","Enh"="gold","Tss"="firebrick1","ReprPC"="slategray2","Others"="mediumaquamarine")),annotation_legend_param = list(Legend_direction = "horizontol",legend_width = unit(5, "cm"),title = "ChromeHMMsimple", title_gp = gpar(fontsize = 10,fontface="bold"), labels_gp = gpar(fontsize = 10)))
                                	  h1=Heatmap(reshape2::dcast(yy,peakco~ID,value.var="values") %>% data.frame(.,row.names = "peakco") %>% apply(.,1,function(x) (x-mean(x))/sd(x)) %>% t(), name ="Chromatin accessibility",heatmap_legend_param = list(legend_direction = "horizontal",legend_width = unit(5, "cm"),title_gp = gpar(fontsize = 12,fontface="bold"), labels_gp = gpar(fontsize = 8,fontface="bold")),column_title = " Heatmap",column_title_gp = gpar(fontsize=20,fontface="bold"),top_annotation = ha_colum)
                                	  ht=ha_row+h1
                                	  draw(ht,heatmap_legend_side="bottom",annotation_legend_side="bottom")
            	  
                    	})	
            	
                            	output$dataPlot <-
                            	  renderPlot(
                            	    {
                            	      withProgress(message = 'Calculation in progress',
                            	                   detail = 'This may take a while...', value = 0, {
                            	                     for (i in 1:5) {
                            	                       incProgress(1/5)
                            	                       Sys.sleep(0.25)
                            	                     } })
                            	      buildPlot()
                            	    },
                            	    height = 700
                            	  )
            	
                            	addPopover(session, "dataPlot", "Data", content = paste0("The heatmap shows normalized (z scores) of chromatin profiles for differential peaks across PBMC samples where a Sample ID is specific for each PBMC sample from young 20, old14 donors.   "), placement="top", trigger = 'hover')
                              addPopover(session,"btton","Info",content = paste0("Use this button to unlock the drop down menu ,[USE ONLY ONE DROP DOWN MENU AT A TIME] "), placement="top", trigger = 'hover')                                                      
                              addPopover(session,"BTTN","Info",content = paste0("Use this button to unlock the drop down menu ,[USE ONLY ONE DROP DOWN MENU AT A TIME] "), placement="top", trigger = 'hover')                                                      
                              
            	
            	
            	
            	
            	
            #============= =============	============= ============= ============= =============         	
                                     #RNA AND ATAC PLOT STARTS HERE
            #============= =============	============= ============= ============= =============         	
                              
                          	  buildPlot12_1<-reactive({
                          	    
                          	    hintjs(session, options = list("hintButtonLabel1"="That was a info"))
                          	   cd_data_frame=cDat()
                          	   p<-ggplot(cDat(),aes(ATAC_score,RNA_score))+geom_point()+stat_smooth(method = "lm",col="red")+theme_classic()+facet_wrap(~Gene_name,scales="free")+theme(strip.background = element_rect(fill = "aquamarine"),axis.text.x = element_text(size=19,face = "bold"),axis.text.y = element_text(size=19,face="bold"),strip.text = element_text(size=18))
                          	   p+theme(plot.title = element_text(size = 18,face = "bold.italic",lineheight =1.0 ),axis.title.x = element_text(size = 18,face = "bold.italic",lineheight =1.0 ),axis.title.y =element_text(size = 18,face = "bold.italic",lineheight =1.0)  )
                          	   
                          	  })
                            	  output$Rna_AtacPlot_single <- renderPlot(
                            	    {
                            	      buildPlot12_1()
                            	    },
                            	    height = 500
                            	  )
                        	  addPopover(session, "Rna_AtacPlot_single", "Data", content = paste0("The scatter plot presents RNA-seq vs ATAC-seq normalized (z scores) for gene expression and chromatin accessibility correlation among subjects "),placement="top" ,trigger = 'hover')
                        	  
                        	                                                                      
            #============= =============	============= ============= ============= ============= #============= =============	============= ============= ============= =============         	
             #============= =============	============= ============= ============= =============         	
                        	  
                          	  buildPlot12<-reactive({
                          	  
                          	    p<-ggplot(cDat(),aes(ATAC_score,RNA_score))+geom_point()+stat_smooth(method = "lm",col="red")+theme_classic()+facet_wrap(ChromeState~peakco,scales="free")+theme(strip.background = element_rect(fill = "aquamarine"),axis.text.x = element_text(size=15,face="bold"),axis.text.y = element_text(size=15,face = "bold"),strip.text = element_text(size=11,face = "bold"))#
                          	    p+theme(plot.title = element_text(size = 18,face = "bold.italic",lineheight =1.0 ),axis.title.x = element_text(size = 18,face = "bold.italic",lineheight =1.0 ),axis.title.y =element_text(size = 18,face = "bold.italic",lineheight =1.0),panel.spacing.x =   unit(1,"lines")  )
                          	    
                          	  })
                          	  output$Rna_AtacPlot <- renderPlot(
                          	    {
                          	      withProgress(message = 'Calculation in progress',
                          	                   detail = 'This may take a while...', value = 0, {
                          	                     for (i in 1:5) {
                          	                       incProgress(1/5)
                          	                       Sys.sleep(0.25)
                          	                     } })
                          	      buildPlot12()
                          	    },
                          	    height = 500
                          	   
                          	  )
            #============= =============	============= ============= ============= =============	  
            	  #Gene_expression and chromatin aceesibility box plot
            #============= =============	============= ============= ============= =============
                          	  	  buildPlot33<-reactive({
            	    
                            	     cd_data_frame=cDat()
                             
                            	     cd_data_frame%<>%gather(key = "ATAC_RNA",value = "VALUE_ATAC_RNA",ATAC_score:RNA_score)
                            	     cd_data_frameSum=ddply(cd_data_frame,.(TYPE),summarise,sum=length(unique(ID)))
                                   p<-ggplot(cd_data_frame,aes(x=  TYPE,y=VALUE_ATAC_RNA,fill=ATAC_RNA ))+geom_boxplot()+scale_fill_manual(values = c("deeppink3","darkturquoise "))+theme_bw()+facet_wrap(~Gene_name,scales = "free_y")+xlab("TYPE")+ylab("scaled open chromatin/gene expression")+theme(strip.background = element_rect(fill = "aquamarine"),axis.text.x = element_text(size=16,face = "bold"),axis.text.y = element_text(size=16,face="bold"),strip.text = element_text(size=18),legend.text = element_text(size = 18),axis.title.x = element_text(size = 18),axis.title.y = element_text(size = 18),legend.title = element_text(size = 18))
                            	     p+theme(plot.title = element_text(size = 18,face = "bold.italic",lineheight =1.0 ),axis.title.x = element_text(size = 18,face = "bold.italic",lineheight =1.0 ),axis.title.y =element_text(size = 18,face = "bold.italic",lineheight =1.0) )
                            	     p+scale_x_discrete(label=paste(cd_data_frameSum$TYPE,"(",cd_data_frameSum$sum,")",sep = ""))
                            	    
                            	      })
                            	      
            	    
            	    
                            	  output$AtacPlot_1 <- renderPlot(
                            	       {
                            	         withProgress(message = 'Calculation in progress',
                            	                      detail = 'This may take a while...', value = 0, {
                            	                        for (i in 1:5) {
                            	                          incProgress(1/5)
                            	                          Sys.sleep(0.25)
                            	                        } })
                            	       buildPlot33()
                            	       },
                            	       height = 500
                            	      
                            	  )
            	                   addPopover(session, "AtacPlot_1", "Data", content = paste0("The boxplot shows chromatin accessibility at promoters and gene expression changes with aging"),placement="top" ,trigger = 'hover')
            	  
            #============= =============	============= ============= ============= =============	  
                                      # ANNOTATION SECTION  
            #============= =============	============= ============= ============= =============	                   
            	 
            	                   
            	                   
            #============= =============	============= ============= ============= =============	                   
                                             # box_plot
            #============= =============	============= ============= ============= =============	                   
            	                   
            	                   
                        	     buildPlot36999999_small_box_cell_type_data2<-reactive({
                        	  
                        	       cd_data_frame=cDat2()
                        	       cd_data_frame%<>%gather(key = "ATAC_RNA",value = "VALUE_ATAC_RNA",ATAC_score:RNA_score)
                        	       cd_data_frameSum=ddply(cd_data_frame,.(TYPE),summarise,sum=length(unique(ID)))
                        	       p<-ggplot(cd_data_frame,aes(x=TYPE,y=VALUE_ATAC_RNA,fill=ATAC_RNA))+geom_boxplot()+scale_fill_manual(values = c("deeppink3","darkturquoise"))+theme_bw()+xlab("TYPE")+ylab("scaled open chromatin/gene expression")+theme(strip.background = element_rect(fill = "aquamarine"),axis.text.x = element_text(size=16,face = "bold"),axis.text.y = element_text(size=16,face="bold"),strip.text = element_text(size=18),legend.title=element_text(size=18),  legend.text=element_text(size=18),axis.title.x = element_text(size=18),axis.title.y = element_text(size = 18))
                        	       p+theme(plot.title = element_text(size = 18,face = "bold.italic",lineheight =1.0 ),axis.title.x = element_text(size = 18,face = "bold.italic",lineheight =1.0 ),axis.title.y =element_text(size = 18,face = "bold.italic",lineheight =1.0) )
                        	       p+scale_x_discrete(label=paste(cd_data_frameSum$TYPE,"(",cd_data_frameSum$sum,")",sep = ""))
                        	      })
            	     
                        	     output$small_box_cell_type_data2 <- renderPlot(
                        	       {
                        	         withProgress(message = 'Calculation in progress',
                        	                      detail = 'This may take a while...', value = 0, {
                        	                        for (i in 1:5) {
                        	                          incProgress(1/5)
                        	                          Sys.sleep(0.25)
                        	                        } })
                        	         buildPlot36999999_small_box_cell_type_data2()
                        	       },
                        	       height = 500
                        	       )
            	     
            #============= =============	============= ============= ============= =============	                   
                                     # box_plot with facets
            #============= =============	============= ============= ============= =============	                   
                        	     
                        	     buildPlot36999999<-reactive({
                        	       
                        	       cd_data_frame=cDat2()
                        	       
                        	       cd_data_frame%<>%gather(key = "ATAC_RNA",value = "VALUE_ATAC_RNA",ATAC_score:RNA_score)
                        	       
                        	       p<-ggplot(cd_data_frame,aes(x=TYPE,y=VALUE_ATAC_RNA,fill=ATAC_RNA))+geom_boxplot()+scale_fill_manual(values = c("deeppink3","darkturquoise "))+theme_bw()+xlab("TYPE")+ylab("scaled open chromatin/gene expression")+facet_wrap(~Gene_name)+theme(strip.background = element_rect(fill = "aquamarine"),axis.text.x = element_text(size=16,face = "bold"),axis.text.y = element_text(size=16,face="bold"),strip.text = element_text(size=18),legend.text = element_text(size = 18),axis.title.x = element_text(size = 18),axis.title.y = element_text(size = 18))
                        	       p+theme(plot.title = element_text(size = 18,face = "bold.italic",lineheight =1.0 ),axis.title.x = element_text(size = 18,face = "bold.italic",lineheight =1.0 ),axis.title.y =element_text(size = 18,face = "bold.italic",lineheight =1.0) )
                        	       p+theme(panel.spacing = unit(1,"lines"))
                        	      p
                        	     })
            	     
                      	     output$box_cell_pathway <- renderPlot(
                      	       {withProgress(message = 'Calculation in progress',
                      	                     detail = 'This may take a while...', value = 0, {
                      	                       for (i in 1:5) {
                      	                         incProgress(1/5)
                      	                         Sys.sleep(0.25)
                      	                       } })
                      	         buildPlot36999999()
                      	       },
                      	       height = 600
                      	       
            	       
            	       
            	       
            	     )
            #============= =============	============= ============= ============= =============	                   
                                  # heatmap for atac data
            #============= =============	============= ============= ============= =============	                   
                    buildPlot9699 <- reactive({
                                      	       abcd=cDat2() 
                                      	       yy=abcd %>% group_by(Gene_name,ID) %>% arrange(Gene_name,-abs(atac_log_fc)) %>% slice(1) %>% ungroup()
                                               annot_df1<-data.frame(yy %>% distinct(ID,TYPE),row.names = "ID")
                                      	       ha_colum<-HeatmapAnnotation(annot_df1,col = list(TYPE=c("HO"="deeppink3","HY"="darkturquoise")),annotation_legend_param = list(Legend_direction = "horizontol",legend_width = unit(5, "cm"),title = "AGE GROUP", title_gp = gpar(fontsize = 10,fontface="bold"), labels_gp = gpar(fontsize = 10)))
                                      	       h1=Heatmap(reshape2::dcast(yy,Gene_name~ID,value.var="ATAC_score") %>% data.frame(.,row.names = "Gene_name") %>% apply(.,1,function(x) (x-mean(x))/sd(x)) %>% t(), name ="Chromatin accessibility",heatmap_legend_param = list(legend_direction = "horizontal",legend_width = unit(5, "cm"),title_gp = gpar(fontsize = 12,fontface="bold"), labels_gp = gpar(fontsize = 8,fontface="bold")),column_title = " ATAC_score ",column_title_gp = gpar(fontsize=20,fontface="bold"),top_annotation = ha_colum)
                                               draw(h1,heatmap_legend_side="bottom",annotation_legend_side="bottom")
                             
            	                              })	
            	      output$heat_pathways <- renderPlot(
                                      	       {
                                	            withProgress(message = 'Calculation in progress',
                                	            detail = 'This may take a while...', value = 0, {
                                	            for (i in 1:5) 
                                	            {
                                	            incProgress(1/5)
                                	            Sys.sleep(0.25)
                                	            }
                                	            })
                                	           buildPlot9699()
                                	           },
                                	           height = 500
            	                               )
            	          addPopover(session, "heat_pathways", "Data", content = paste0("The heatmap displays subject-specific chromatin accessibility peaks. The test for differential ATAC-seq peaks was based on a GLM, with Benjamini-Hochberg FDR <5%. Subject-specific chromatin accessibility of peaks significantly closing with aging and annotated to genes. red (blue) hues represent increased (decreased) chromatin accessibility relative to the cohort mean. Data shown as normalized (z scores) values."),placement="top" ,trigger = 'hover')
            #============= =============	============= ============= ============= =============	                   
            	          # HEATMAP FOR RNA VALUES
            #============= =============	============= ============= ============= =============	                   
            	         buildPlot96999PCA_RNA_heat_pathways_1 <- reactive({
            
                                  	         abcd=cDat2() 
                                  
                                  	        yy=abcd %>% group_by(Gene_name,ID) %>% arrange(Gene_name,-abs(atac_log_fc)) %>% slice(1) %>% ungroup()
                                  	        annot_df1<-data.frame(yy %>% distinct(ID,TYPE),row.names = "ID")
                                  	        ha_colum<-HeatmapAnnotation(annot_df1,col = list(TYPE=c("HO"="deeppink3","HY"="darkturquoise")),annotation_legend_param = list(Legend_direction = "horizontol",legend_width = unit(5, "cm"),title = "AGE GROUP", title_gp = gpar(fontsize = 10,fontface="bold"), labels_gp = gpar(fontsize = 10)))
                                  	        h1=Heatmap(reshape2::dcast(yy,Gene_name~ID,value.var="RNA_score") %>% data.frame(.,row.names = "Gene_name") %>% apply(.,1,function(x) (x-mean(x))/sd(x)) %>% t(), name ="Chromatin accessibility",heatmap_legend_param = list(legend_direction = "horizontal",legend_width = unit(5, "cm"),title_gp = gpar(fontsize = 12,fontface="bold"), labels_gp = gpar(fontsize = 8,fontface="bold")),column_title = " RNA_score ",column_title_gp = gpar(fontsize=20,fontface="bold"),top_annotation = ha_colum)
                                  	        draw(h1,heatmap_legend_side="bottom",annotation_legend_side="bottom")
            	        
            	                            })	
            	      
            	      output$PCA_RNA_heat_pathways_1 <- renderPlot(
            	        {
            	          withProgress(message = 'Calculation in progress',
            	          detail = 'This may take a while...', value = 0, {
            	          for (i in 1:5)
            	           {
            	           incProgress(1/5)
            	           Sys.sleep(0.25)
            	           }
            	           })
            	          buildPlot96999PCA_RNA_heat_pathways_1()
            	           },
            	          height = 500
                 	      )
            	      
            	      addPopover(session, "PCA_RNA_heat_pathways_1", "Data", content = paste0("The heatmap displays subject-specific mRNA expression. The test for differential mRNA expression was based on a GLM, with Benjamini-Hochberg FDR <5%. red (blue) hues represent increased (decreased) mRNA expression relative to the cohort mean. Data shown as normalized (z scores) values."),placement="bottom" ,trigger = 'hover')
            #============= =============	============= ============= ============= =============	                   
                                       # PCA PLOT FOR ATAC SCORE
            #============= =============	============= ============= ============= =============	                   
            	      buildPlot369111111333<- reactive({
                                            	        abcd=cDat2() 
                                            	        yy=abcd %>% group_by(Gene_name,ID) %>% arrange(Gene_name,-abs(atac_log_fc)) %>% slice(1) %>% ungroup()
                                            	        new_lmn<-reshape2::dcast(yy,ID+TYPE~peakco,value.var = "ATAC_score",mean)
                                            	        rownames(new_lmn)<-new_lmn$ID
                                            	        new_lmn$ID<-NULL
                                            	        new_lmn_version1<-new_lmn
                                            	        new_lmn$TYPE<-NULL
                                            	        pca_wor<-prcomp(x=new_lmn,center = TRUE,scale. = TRUE)
                                            	        pca_wor_mat<-pca_wor$x
                                            	        pca_wor_df<-as.data.frame(pca_wor_mat)
                                            	        pca_wor_df$TYPE<-new_lmn_version1$TYPE
                                            	        p<-ggplot(data = pca_wor_df, aes(x = PC1, y = PC2,colour=TYPE)) +
                                            	        geom_hline(yintercept = 0, colour = "black") +
                                            	        geom_vline(xintercept = 0, colour = "black") +
                                            	        geom_text(aes(label=rownames(pca_wor_df)),size = 5)+
                                            	        ggtitle("PCA plot of ATAC score")+
                                            	        theme(plot.title = element_text(size = 18,face = "bold.italic",lineheight =1.0 ),axis.title.x = element_text(size = 18,face = "bold.italic",lineheight =1.0 ),axis.title.y =element_text(size = 18,face = "bold.italic",lineheight =1.0),strip.background = element_rect(fill = "aquamarine"),axis.text.x = element_text(size=16,face = "bold"),axis.text.y = element_text(size=16,face="bold"),strip.text = element_text(size=18),legend.title=element_text(size=18),  legend.text=element_text(size=18))
                                            	        
                                            	        p
            	        
            	                                     })	
            	     
            	        
            	       
            	     	
                                        	      output$ATAC_pca_ui<- renderPlot(
                                        	        {
                                        	        buildPlot369111111333()
                                        	        },
                                        	        height = 500
                                        	        )
            	                                 addPopover(session, "PCA_heat_pathways", "Data", content = paste0("Principal component analysis (PCA) plot shows the correlation between PC1 and PC2. Percentage of variation among differential peaks accounted for by each PC is shown in parentheses."),placement="bottom" ,trigger = 'hover')
            #============= =============	============= ============= ============= =============	                   
            	                      # PCA FOR RNA SCORE
            #============= =============	============= ============= ============= =============	                   
            	      
            	     
            
            	      buildPlot99339393333<- reactive({
            
            	        abcd=cDat2() 
            	        
            	        yy=abcd %>% group_by(Gene_name,ID) %>% arrange(Gene_name,-abs(atac_log_fc)) %>% slice(1) %>% ungroup()
            	        
            	        new_lmn<-reshape2::dcast(yy,ID+TYPE~peakco,value.var = "RNA_score",mean)
            	        rownames(new_lmn)<-new_lmn$ID
            	        new_lmn$ID<-NULL
            	        new_lmn_version1<-new_lmn
            	        new_lmn$TYPE<-NULL
            	        pca_wor<-prcomp(x=new_lmn,center = TRUE,scale. = TRUE)
            	        pca_wor_mat<-pca_wor$x
            	        pca_wor_df<-as.data.frame(pca_wor_mat)
            	        pca_wor_df$TYPE<-new_lmn_version1$TYPE
            	        
            	        
            	        
            	        p<-ggplot(data = pca_wor_df, aes(x = PC1, y = PC2,colour=TYPE)) +
            	          geom_hline(yintercept = 0, colour = "black") +
            	          geom_vline(xintercept = 0, colour = "black") +
            	          geom_text(aes(label=rownames(pca_wor_df)),size = 5)+
            	          ggtitle("PCA plot of RNA score")+
            	          theme(plot.title = element_text(size = 18,face = "bold.italic",lineheight =1.0 ),axis.title.x = element_text(size = 18,face = "bold.italic",lineheight =1.0 ),axis.title.y =element_text(size = 18,face = "bold.italic",lineheight =1.0),strip.background = element_rect(fill = "aquamarine"),axis.text.x = element_text(size=16,face = "bold"),axis.text.y = element_text(size=16,face="bold"),strip.text = element_text(size=18),legend.title=element_text(size=18),  legend.text=element_text(size=18))
            	        
            	        p
            	        
            	        
            	        
            	       
            	        
            	      })	
            	      
            	      output$RNA_pca_ui<- renderPlot(
            	        {
            	         
            	          buildPlot99339393333()
            	          
            	          
            	        },
            	        height = 500
            	        
            	        
            	      )
            	      addPopover(session, "heat_pathways_1", "Data", content = paste0("Principal component analysis (PCA) plot shows the correlation between PC1 and PC2. Percentage of variation among differential peaks accounted for by each PC is shown in parentheses."),placement="bottom" ,trigger = 'hover')
            	      
            	      
            	    
            	      
            	      
            	      
            	      #============= =============	============= ============= ============= =============	                   
            	                                    # CELL TYPE STARTS HERE 
            	      #============= =============	============= ============= ============= =============	                   
            	      
            	      #============= =============	============= ============= ============= =============	                   
            	                                             # BOXPLOT
            	      #============= =============	============= ============= ============= =============	                   
            	      
            	      buildPlottype_data9333333three1 <- reactive({
            	        abcd=cDat3()
            	        yy4=abcd %>%filter(Cell_Type=="CD8_Naive")%>% group_by(Gene_name,ID) %>% arrange(Gene_name,-abs(logFC)) %>% slice(1) %>% ungroup()
            	        annot_df4<-data.frame(yy4 %>% distinct(ID,AGE),row.names = "ID")
            	        nclea  <-length(yy4$Gene_name)
            	        ha_colum4<-HeatmapAnnotation(annot_df4,col = list(AGE=c("HO"="deeppink3","HY"="darkturquoise","ND"="green","VHY"="gold")),annotation_legend_param = list(Legend_direction = "horizontol",legend_width = unit(5, "cm"),title = "AGE GROUP", title_gp = gpar(fontsize = 10,fontface="bold"), labels_gp = gpar(fontsize = 10)))
            	        h4=Heatmap(reshape2::dcast(yy4,Gene_name~ID,value.var="values") %>% data.frame(.,row.names = "Gene_name") %>% apply(.,1,function(x) (x-mean(x))/sd(x)) %>% t(),name ="Chromatin accessibility" ,heatmap_legend_param = list(legend_direction = "horizontal",legend_width = unit(5, "cm"),title_gp = gpar(fontsize = 12,fontface="bold"), labels_gp = gpar(fontsize = 8,fontface="bold")) ,column_title = "CD8_Naive",column_title_gp = gpar(fontsize=20,fontface="bold"),top_annotation = ha_colum4)
            	        draw(h4, newpage = FALSE,heatmap_legend_side="bottom",annotation_legend_side="bottom")
            	      })	     
            	      
            	      
            	      output$box_cell_type_data9333333three1 <- renderPlot(
            	        {
            	          withProgress(message = 'Calculation in progress',
            	                       detail = 'This may take a while...', value = 0, {
            	                         for (i in 1:5) 
            	                         {
            	                           incProgress(1/5)
            	                           Sys.sleep(0.25)
            	                         }
            	                       })
            	          buildPlottype_data9333333three1()},
            	        height = 500
            	      )
            	      
            	      buildPlottype_data9333333three1 <- reactive({
            	        abcd=cDat3()
            	        yy4=abcd %>%filter(Cell_Type=="CD8_Naive")%>% group_by(Gene_name,ID) %>% arrange(Gene_name,-abs(logFC)) %>% slice(1) %>% ungroup()
            	        annot_df4<-data.frame(yy4 %>% distinct(ID,AGE),row.names = "ID")
            	        nclea  <-length(yy4$Gene_name)
            	        ha_colum4<-HeatmapAnnotation(annot_df4,col = list(AGE=c("HO"="deeppink3","HY"="darkturquoise","ND"="green","VHY"="gold")),annotation_legend_param = list(Legend_direction = "horizontol",legend_width = unit(5, "cm"),title = "AGE GROUP", title_gp = gpar(fontsize = 10,fontface="bold"), labels_gp = gpar(fontsize = 10)))
            	        h4=Heatmap(reshape2::dcast(yy4,Gene_name~ID,value.var="values") %>% data.frame(.,row.names = "Gene_name") %>% apply(.,1,function(x) (x-mean(x))/sd(x)) %>% t(),name ="Chromatin accessibility" ,heatmap_legend_param = list(legend_direction = "horizontal",legend_width = unit(5, "cm"),title_gp = gpar(fontsize = 12,fontface="bold"), labels_gp = gpar(fontsize = 8,fontface="bold")) ,column_title = "CD8_Naive",column_title_gp = gpar(fontsize=20,fontface="bold"),top_annotation = ha_colum4)
            	        draw(h4, newpage = FALSE,heatmap_legend_side="bottom",annotation_legend_side="bottom")
            	      })	     
            	      
            	      
            	      output$box_cell_type_data9333333three1 <- renderPlot(
            	        {
            	          withProgress(message = 'Calculation in progress',
            	                       detail = 'This may take a while...', value = 0, {
            	                         for (i in 1:5) 
            	                         {
            	                           incProgress(1/5)
            	                           Sys.sleep(0.25)
            	                         }
            	                       })
            	          buildPlottype_data9333333three1()},
            	        height = 500
            	      )
            	      
            	     buildPlot39996<-reactive({
            	          cd_data_frame=cDat3()
            	       
            	          #xlabs<-paste(levels(cd_data_frame$Cell_Type),"\n(N=",table(cd_data_frame$Cell_Type),")",sep = "")
            	          cd_data_frameSum=ddply(cd_data_frame,.(Cell_Type),summarise,sum=length(Gene_name))
            	          
            	          p<-ggplot(cd_data_frame,aes(x=Cell_Type, y=logFC,fill=Cell_Type))+geom_boxplot()+xlab("Cell_TYPE")+ylab("LOG FOLD CHANGE (Chromatin Accessibility)")+theme_bw()+theme(axis.text.x = element_text(margin = margin(t = 5, r = 5, b = 5, l = 5),size = 13,face = "bold"),axis.text.y = element_text(size = 16,face = "bold"),strip.text = element_text(size = 18),legend.title = element_text(size=18),legend.text=element_text(size=18),axis.title.x = element_text(size=18),axis.title.y = element_text(size = 18))
            	          p+theme(axis.title.y = element_text(color="#993333", size=14, face="bold"),axis.title.x = element_text(color="#993333", size=14, face="bold"))
            	          p+scale_x_discrete(label=paste(cd_data_frameSum$Cell_Type,"(",cd_data_frameSum$sum,")",sep = ""))
            	          
            	          
            	          
            	        })
            	     
            	    
            	     #   
            	     # })
            	     
            	     
            	     output$box_cell_type_data <- renderPlot(
            	       {
            	         withProgress(message = 'Calculation in progress',
            	                      detail = 'This may take a while...', value = 0, {
            	                        for (i in 1:5) {
            	                          incProgress(1/5)
            	                          Sys.sleep(0.25)
            	                        } })
            	         buildPlot39996()
            	       },
            	       height = 500
            	     )
            	     
            	     
            	     
            	     #============= =============	============= ============= ============= =============	                   
            	                        # HEATNAP FOR PBMC STARTS HERE
            	     #============= =============	============= ============= ============= =============	                   
            	     
            	     buildPlottype_data9333333three1 <- reactive({
                                                    	       abcd=cDat3()
                                                    	       yy4=abcd %>%filter(Cell_Type=="CD8_Naive")%>% group_by(Gene_name,ID) %>% arrange(Gene_name,-abs(logFC)) %>% slice(1) %>% ungroup()
                                                    	       annot_df4<-data.frame(yy4 %>% distinct(ID,AGE),row.names = "ID")
                                                    	       nclea  <-length(yy4$Gene_name)
                                                    	       ha_colum4<-HeatmapAnnotation(annot_df4,col = list(AGE=c("HO"="deeppink3","HY"="darkturquoise","ND"="green","VHY"="gold")),annotation_legend_param = list(Legend_direction = "horizontol",legend_width = unit(5, "cm"),title = "AGE GROUP", title_gp = gpar(fontsize = 10,fontface="bold"), labels_gp = gpar(fontsize = 10)))
                                                    	       h4=Heatmap(reshape2::dcast(yy4,Gene_name~ID,value.var="values") %>% data.frame(.,row.names = "Gene_name") %>% apply(.,1,function(x) (x-mean(x))/sd(x)) %>% t(),name ="Chromatin accessibility" ,heatmap_legend_param = list(legend_direction = "horizontal",legend_width = unit(5, "cm"),title_gp = gpar(fontsize = 12,fontface="bold"), labels_gp = gpar(fontsize = 8,fontface="bold")) ,column_title = "CD8_Naive",column_title_gp = gpar(fontsize=20,fontface="bold"),top_annotation = ha_colum4)
                                                    	       draw(h4, newpage = FALSE,heatmap_legend_side="bottom",annotation_legend_side="bottom")
            	                                                })	     
            	     
            	     
            	     output$box_cell_type_data9333333three1 <- renderPlot(
            	       {
            	         withProgress(message = 'Calculation in progress',
            	                      detail = 'This may take a while...', value = 0, {
            	                        for (i in 1:5) 
            	                        {
            	                          incProgress(1/5)
            	                          Sys.sleep(0.25)
            	                        }
            	                      })
            	         buildPlottype_data9333333three1()},
            	       height = 500
            	     )
            	     
            	     
            	     buildPlot3 <- reactive({
            	       
            	       
            	       abcd=cDat3()
            	       
            	       yy=abcd %>%filter(Cell_Type=="PBMC")%>% group_by(Gene_name,ID) %>% arrange(Gene_name,-abs(logFC)) %>% slice(1) %>% ungroup()
            	       annot_df1<-data.frame(yy %>% distinct(ID,AGE),row.names = "ID")
            	       ha_colum<-HeatmapAnnotation(annot_df1,col = list(AGE=c("HO"="deeppink3","HY"="darkturquoise","ND"="green","VHY"="gold")),annotation_legend_param = list(Legend_direction = "horizontol",legend_width = unit(5, "cm"),title = "AGE GROUP", title_gp = gpar(fontsize = 10,fontface="bold"), labels_gp = gpar(fontsize = 10)))
            	       h1=Heatmap(reshape2::dcast(yy,Gene_name~ID,value.var="values") %>% data.frame(.,row.names = "Gene_name") %>% apply(.,1,function(x) (x-mean(x))/sd(x)) %>% t(),name ="Chromatin accessibility" , heatmap_legend_param = list(legend_direction = "horizontal",legend_width = unit(5, "cm"),title_gp = gpar(fontsize = 12,fontface="bold"), labels_gp = gpar(fontsize = 8,fontface="bold")),column_title = " PBMC",column_title_gp = gpar(fontsize=20,fontface="bold"),top_annotation = ha_colum)
            	       draw(h1,heatmap_legend_side="bottom",annotation_legend_side="bottom")
            	       
            	       
            	     })	
            	     output$box_cell_type_data9333333 <- renderPlot(
            	       {
            	         # withProgress(message = 'Calculation in progress',
            	         #              detail = 'This may take a while...', value = 0, {
            	         #                for (i in 1:5) {
            	         #                  incProgress(1/5)
            	         #                  Sys.sleep(0.25)
            	         #                } })
            	         buildPlot3()#error here
            	       },
            	       height = 500
            	       # height = 700
            	       
            	     )
            	     
            	     
            	     
            	     
            	     
            	     
            	     
            	     
            	     
            	     #============= =============	============= ============= ============= =============	                   
            	                                # CD4 NAIVE DATA
            	     #============= =============	============= ============= ============= =============	                   
            	     
            	     buildPlotthree2 <- reactive({
                                        	       abcd=cDat3()
                                        	       yy3=abcd %>%filter(Cell_Type=="CD4_Naive")%>% group_by(Gene_name,ID) %>% arrange(Gene_name,-abs(logFC)) %>% slice(1) %>% ungroup()
                                        	       annot_df1<-data.frame(yy3 %>% distinct(ID,AGE),row.names = "ID")
                                        	       ha_colum<-HeatmapAnnotation(annot_df1,col = list(AGE=c("HO"="deeppink3","HY"="darkturquoise","ND"="green","VHY"="gold")),annotation_legend_param = list(Legend_direction = "horizontol",legend_width = unit(5, "cm"),title = "AGE GROUP", title_gp = gpar(fontsize = 10,fontface="bold"), labels_gp = gpar(fontsize = 10)))
                                        	       h2=Heatmap(reshape2::dcast(yy3,Gene_name~ID,value.var="values") %>% data.frame(.,row.names = "Gene_name") %>% apply(.,1,function(x) (x-mean(x))/sd(x)) %>% t(),name ="Chromatin accessibility" , heatmap_legend_param = list(legend_direction = "horizontal",legend_width = unit(5, "cm"),title_gp = gpar(fontsize = 12,fontface="bold"), labels_gp = gpar(fontsize = 8,fontface="bold")),column_title = "CD4_Naive ",column_title_gp = gpar(fontsize=20,fontface="bold"),top_annotation = ha_colum)
                                        	       draw(h2,heatmap_legend_side="bottom",annotation_legend_side="bottom")
                                                })	
            	     
                   output$box_cell_type_data9333333three2 <- renderPlot(
                                          	       {
                                          	       buildPlotthree2()
                                          	       },
                                          	       height = 500
                                          	       
                                          	       )
            	     
                   #============= =============	============= ============= ============= =============	                   
                   # CD4 MEMORY
                   #============= =============	============= ============= ============= =============	                   
                   
            	     buildPlotcell_type_data9333333three <- reactive({
                                                            	       abcd=cDat3()
                                                            	       yy2=abcd %>%filter(Cell_Type=="CD4_Memory")%>% group_by(Gene_name,ID) %>% arrange(Gene_name,-abs(logFC)) %>% slice(1) %>% ungroup()
                                                            	       annot_df2<-data.frame(yy2 %>% distinct(ID,AGE),row.names = "ID")
                                                            	       ha_colum1<-HeatmapAnnotation(annot_df2,col = list(AGE=c("HO"="deeppink3","HY"="darkturquoise")),annotation_legend_param = list(Legend_direction = "horizontol",legend_width = unit(5, "cm"),title = "AGE GROUP", title_gp = gpar(fontsize = 10,fontface="bold"), labels_gp = gpar(fontsize = 10)))
                                                            	       h3=Heatmap(reshape2::dcast(yy2,Gene_name~ID,value.var="values") %>% data.frame(.,row.names = "Gene_name") %>% apply(.,1,function(x) (x-mean(x))/sd(x)) %>% t(),name ="Chromatin accessibility" ,heatmap_legend_param = list(legend_direction = "horizontal",legend_width = unit(5, "cm"),title_gp = gpar(fontsize = 12,fontface="bold"), labels_gp = gpar(fontsize = 8,fontface="bold")),column_title = "CD4_Memory ", column_title_gp = gpar(fontsize=20,fontface="bold"),top_annotation = ha_colum1)
                                                            	       draw(h3,heatmap_legend_side="bottom",annotation_legend_side="bottom")
                                                                     })	
            	     
            	     output$box_cell_type_data9333333three <- renderPlot(
            	                                                      {
            	         
            	                                                       buildPlotcell_type_data9333333three()#error here
                                                        	           },
                                                        	           height = 500
            	                                                       )
            	     
            	     #============= =============	============= ============= ============= =============	                   
            	                                   # CD8 NAIVE
            	     #============= =============	============= ============= ============= =============	                   
            	     
            	     buildPlottype_data9333333three1 <- reactive({
                                                        	       abcd=cDat3()
                                                        	       yy4=abcd %>%filter(Cell_Type=="CD8_Naive")%>% group_by(Gene_name,ID) %>% arrange(Gene_name,-abs(logFC)) %>% slice(1) %>% ungroup()
                                                        	       annot_df4<-data.frame(yy4 %>% distinct(ID,AGE),row.names = "ID")
                                                        	       nclea  <-length(yy4$Gene_name)
                                                        	       ha_colum4<-HeatmapAnnotation(annot_df4,col = list(AGE=c("HO"="deeppink3","HY"="darkturquoise","ND"="green","VHY"="gold")),annotation_legend_param = list(Legend_direction = "horizontol",legend_width = unit(5, "cm"),title = "AGE GROUP", title_gp = gpar(fontsize = 10,fontface="bold"), labels_gp = gpar(fontsize = 10)))
                                                        	       h4=Heatmap(reshape2::dcast(yy4,Gene_name~ID,value.var="values") %>% data.frame(.,row.names = "Gene_name") %>% apply(.,1,function(x) (x-mean(x))/sd(x)) %>% t(),name ="Chromatin accessibility" ,heatmap_legend_param = list(legend_direction = "horizontal",legend_width = unit(5, "cm"),title_gp = gpar(fontsize = 12,fontface="bold"), labels_gp = gpar(fontsize = 8,fontface="bold")) ,column_title = "CD8_Naive",column_title_gp = gpar(fontsize=20,fontface="bold"),top_annotation = ha_colum4)
                                                        	       draw(h4, newpage = FALSE,heatmap_legend_side="bottom",annotation_legend_side="bottom")
                                                        	     })	     
            	     
            	     
            	     output$box_cell_type_data9333333three1 <- renderPlot(
            	       {
                                        	                      withProgress(message = 'Calculation in progress',
                                        	                      detail = 'This may take a while...', value = 0, {
                                        	                      for (i in 1:5) 
                                        	                       {
                                        	                       incProgress(1/5)
                                        	                       Sys.sleep(0.25)
                                        	                        }
                                        	                        })
                                        	                        buildPlottype_data9333333three1()},
                                        	                        height = 500
                                        	                         )
            	     
            	     
            	     #============= =============	============= ============= ============= =============	                   
            	                                  # CD8 MEMORY
            	     #============= =============	============= ============= ============= =============	                   
            	     
            	     buildPlotthree3 <- reactive({
                                    	        abcd=cDat3()
                                    	        yy1=abcd %>%filter(Cell_Type=="CD8_Memory")%>% group_by(Gene_name,ID) %>% arrange(Gene_name,-abs(logFC)) %>% slice(1) %>% ungroup()
                                    	        annot_df3<-data.frame(yy1 %>% distinct(ID,AGE),row.names = "ID")
                                    	        ha_colum3<-HeatmapAnnotation(annot_df3,col = list(AGE=c("HO"="deeppink3","HY"="darkturquoise")),annotation_legend_param = list(Legend_direction = "horizontol",legend_width = unit(5, "cm"),title = "AGE GROUP", title_gp = gpar(fontsize = 10,fontface="bold"), labels_gp = gpar(fontsize = 10)))
                                    	        h1=Heatmap(reshape2::dcast(yy1,Gene_name~ID,value.var="values") %>% data.frame(.,row.names = "Gene_name") %>% apply(.,1,function(x) (x-mean(x))/sd(x)) %>% t(), name ="Chromatin accessibility",heatmap_legend_param = list(legend_direction = "horizontal",legend_width = unit(5, "cm"),title_gp = gpar(fontsize = 12,fontface="bold"), labels_gp = gpar(fontsize = 8,fontface="bold")),column_title = " CD8_Memory ",column_title_gp = gpar(fontsize=20,fontface="bold"),top_annotation = ha_colum3)
                                    	        draw(h1, newpage = FALSE,heatmap_legend_side="bottom",annotation_legend_side="bottom")
                                              })
            	     
            	     output$box_cell_type_data9333333three3 <- renderPlot(
            	       {
            	         buildPlotthree3()
            	       },
            	       height = 500
            	      )
            	     
            	    
            	     
            	     #============= =============	============= ============= ============= =============	                   
            	                          # DOWNLOAD FUNCTION STARTS HERE
            	     #============= =============	============= ============= ============= =============	                   
            	     
            	     
            	    
            	      output$downloadPlot <- downloadHandler(  # heatmap
            	         filename = function() {
            	           "visualxPlot.pdf"
            	         },
            	         
            	         content = function(file) {
            	           jpeg(file = file,
            	                width = 12,
            	                height = 12)
            	           print(buildPlot())
            	           dev.off()
            	         }
            	       )	
            	    output$downloadPlot8 <- downloadHandler(    # scatter plot
            	      filename = function() {
            	        "visualxPlot.pdf"
            	      },
            	      
            	      content = function(file) {
            	        jpeg(file = file,
            	            width = 12,
            	            height = 12)
            	        print(buildPlot12_1())
            	        dev.off()
            	      }
            	    )	
            	    output$downloadPlot9 <- downloadHandler(   # boxplot
            	      filename = function() {
            	        "visualxPlot.pdf"
            	      },
            	      
            	      content = function(file) {
            	        pdf(file = file,
            	            width = 12,
            	            height = 12)
            	        print(buildPlot33())
            	        dev.off()
            	      }
            	    )	
            	    output$downloadPlot393 <- downloadHandler(   # atac heatmap
            	      filename = function() {
            	        "visualxPlot.pdf"
            	      },
            	      
            	      content = function(file) {
            	        pdf(file = file,
            	            width = 12,
            	            height = 12)
            	        print(buildPlot9699())
            	        dev.off()
            	      }
            	    )	
            	    output$downloadPlot396663 <- downloadHandler( # rna heatmap
            	      filename = function() {
            	        "visualxPlot.pdf"
            	      },
            	      
            	      content = function(file) {
            	        pdf(file = file,
            	            width = 12,
            	            height = 12)
            	        print(buildPlot96999PCA_RNA_heat_pathways_1()) 
            	        dev.off()
            	      }
            	    )	
            	    output$downloadPlot1332<- downloadHandler( # boxplot
            	      filename = function() {
            	        "visualxPlot.pdf"
            	      },
            	      
            	      content = function(file) {
            	        pdf(file = file,
            	            width = 12,
            	            height = 12)
            	        print(buildPlot36999999_small_box_cell_type_data2())
            	        dev.off()
            	      }
            	    )	
            	    
            	    output$downloadPlot331 <- downloadHandler( # pca plot
            	      filename = function() {
            	        "visualxPlot.pdf"
            	      },
            	      
            	      content = function(file) {
            	        pdf(file = file,
            	            width = 12,
            	            height = 12)
            	        print( buildPlot369111111())
            	        dev.off()
            	      }
            	    )	 
            	    
            	    output$downloadPlot9333333 <- downloadHandler(  # heatmap for cell type
            	      filename = function() {
            	        "visualxPlot.pdf"
            	      },
            	      
            	      content = function(file) {
            	        pdf(file = file,
            	            width = 12,
            	            height = 12)
            	        print(buildPlot3())
            	        dev.off()
            	      }
            	    )
            	    output$downloadPlot3993 <- downloadHandler( # cell type boxplot
            	      filename = function() {
            	        "visualxPlot.pdf"
            	      },
            	      
            	      content = function(file) {
            	        pdf(file = file,
            	            width = 12,
            	            height = 12)
            	        print(buildPlot39996())
            	        dev.off()
            	      }
            	    )
            	    
            	    
            	# ========== LOADING THE APP ==========
            	
            	# We need to have a quasi-variable flag to indicate when the app is loaded
            	dataValues <- reactiveValues(
            		appLoaded = FALSE
            	)
            	
            	# Wait for the years input to be rendered as a proxy to determine when the app
            	# is loaded. Once loaded, call the javascript funtion to fix the plot area
            	# (see www/helper-script.js for more information)
            	observe({
            		if (dataValues$appLoaded) {
            			return(NULL)
            		}
            		if(!is.null(input$Age)) {
            			dataValues$appLoaded <- TRUE
            			
              		session$sendCustomMessage(type = "equalizePlotHeight",
              															message = list(target = "dataPlot",
              																						 by = "resultsTab"))
            		}
            	})
            	
            	# Show form content and hide loading message
            	hide("loadingContent")
            	show("allContent")
            })
            
            
            
