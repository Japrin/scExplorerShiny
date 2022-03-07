# Load packages----

# Load functions----
#source("./dataprepare_utils.R")
#source("./plot_utils.R")
#source("./lib/plot.func.R")
# utilities ----
my.getPanelList <- function(dat.list,SelectData_dataset){
  sce <- dat.list[[SelectData_dataset]][["sce.perMini"]]
  out.list <- list("meta.cluster"=sort(levels(as.factor(sce[["meta.cluster"]]))),
                   "cancerType"=levels(as.factor(sce[["cancerType"]])),
                   "study"=levels(as.factor(sce[["dataset"]])),
                   "reducedDimNames"=reducedDimNames(sce),
                   "geneSymbol"=sort(unname(rowData(sce)$display.name)),
                   "metaInfo.col"=intersect(colnames(colData(sce)),
                                            c("dataset","cancerType","meta.cluster",
                                              "batchV",
                                              "globalC","subC"))
  )
  if("loc" %in% colnames(colData(sce))){
    out.list[["tissue"]] <- c("P","N","T","L")
  }else{
    out.list[["tissue"]] <- NULL
  }
  return(out.list)
}

# Transfer the dataset map to a list tree
datasetmap2list <- function(dataset_map){
  dataset_map <- dataset_map[dataset_map$DatasetName != "Initialize",]
  all_datasource <- unique(dataset_map$DatasetSource)
  dt <- NULL
  for(i in 1:length(all_datasource)){
    dt[all_datasource[i]] <- list(NULL)
    all_datasetname <- unique(dataset_map[dataset_map$DatasetSource == all_datasource[i], "DatasetName"])
    for(j in 1:length(all_datasetname)){
      dt[[i]][all_datasetname[j]] <- list(all_datasetname[j])
    }
    dt[[i]] <- structure(dt[[i]], stid=i, stopened=TRUE, stclass='project')
  }
  dt <- structure(dt, stopend = TRUE)
  return(dt)
}

loadDataSet <- function(LoadData_selected) {
  ## Load the required dataset from the LoadData_selected parameter.
  ##
  ## Args:
  #' @LoadData_selected: Record the dataset to load into the memory.
  ##
  ## Returns:
  ## A dataframe with two columns. The first column is the dataset name. The second column
  ## is the loading status, which could be "Failed", "Existed" or "Succeeded".
  LoadData.status <- data.frame(Dataset = "Dataset", Status = "Status", stringsAsFactors = F)
  ret.dat.list <- list()
  #### if use g.dat.list here, it will be just a copy of the g.dat.list from an eviroment which is not global,
  ##### but something like <environment: 0x0000000043f64b00>)
  for(dataset in LoadData_selected){
    if(!(dataset %in% names(.GlobalEnv$g.dat.list))){
      exp.perMini.filepath <- sprintf("./data/%s", g.dataset_map[dataset,"perMiniCluster"])
      #exp.perMeta.filepath <- sprintf("./data/%s", g.dataset_map[dataset,"perMetaCluster"])
      geneDesc.filepath <- sprintf("./data/%s",g.dataset_map[dataset,"geneDesc"])
      meta.perCell.filepath <- sprintf("./data/%s", g.dataset_map[dataset,"meta.perCell"])
      geneTableLong.filepath <- sprintf("./data/%s", g.dataset_map[dataset,"geneTableLong"])
      colSet.filepath <- sprintf("./data/%s", g.dataset_map[dataset,"colSet"])
      if(file.exists(exp.perMini.filepath)){
        ret.dat.list[[dataset]] <- list("sce.perMini"=reformatData(sce=readRDS(exp.perMini.filepath)),
                                        "geneDesc"=NULL,
                                        "geneTableLong"=NULL,
                                        "colSet"=NULL,
                                        "meta.perCell"=NULL,
                                        "plots"=list())
        if(file.exists(geneDesc.filepath)){
            ret.dat.list[[dataset]][["geneDesc"]] <- reformatData(geneDesc.tb=readRDS(geneDesc.filepath))
        }
        if(file.exists(geneTableLong.filepath)){
            ret.dat.list[[dataset]][["geneTableLong"]] <- readRDS(geneTableLong.filepath)
        }
        if(file.exists(colSet.filepath)){
            ret.dat.list[[dataset]][["colSet"]] <- reformatData(colSet=readRDS(colSet.filepath))
        }else{
            mcls.vec <- as.character(unique(sort(ret.dat.list[[dataset]][["sce.perMini"]][["meta.cluster"]])))
            cancerType.vec <- as.character(unique(sort(ret.dat.list[[dataset]][["sce.perMini"]][["cancerType"]])))
            ret.dat.list[[dataset]][["colSet"]] <- list("meta.cluster"=structure(sscVis:::auto.colSet(length(mcls.vec)), names=mcls.vec),
                                                        "cancerType"=structure(sscVis:::auto.colSet(length(cancerType.vec),name="Paired"), names=cancerType.vec))
        }
        if(file.exists(meta.perCell.filepath)){
            ret.dat.list[[dataset]][["meta.perCell"]] <- reformatData(meta.tb=readRDS(meta.perCell.filepath))
        }
        LoadData.status <- rbind(LoadData.status, c(dataset, "Succeeded"))
      }else{
        LoadData.status <- rbind(LoadData.status, c(dataset, "Failed"))
      }
    }else{
      LoadData.status <- rbind(LoadData.status, c(dataset, "Existed"))
    }
  }

  # print("ppp")
  # print(names(g.dat.list))
  # print("qqq")
  # print(names(.GlobalEnv$g.dat.list))
  assign('g.dat.list',c(ret.dat.list,.GlobalEnv$g.dat.list),envir=.GlobalEnv)
  LoadData.status <- LoadData.status[-1,]
  return(LoadData.status)
  #return(list("LoadData.status"=LoadData.status,"dat.list"=ret.dat.list))
}


# Password for the website----
user_base <- read.csv("./data/accounts.csv")

# Initialize the website with a small dataset----
#load("./data/Initialize_expression.rda", envir = .GlobalEnv)
#load("./data/Initialize_metadata.rda", envir = .GlobalEnv)
load("./data/Saved_genes_panel.rda", envir = .GlobalEnv)

#print("which envir?")
#print(environment())
### <environment: 0x0000000043f64b00>

g.dat.list <- list()
g.dat.list[["SampleData"]] <- list("sce.perMini"=readRDS("./data/panC.sample.perMiniCluster.R3.4.rds"),
                                   #"sce.perMeta"=readRDS("./data/panC.sample.perMetaCluster.R3.4.rds"),
                                   "geneDesc"=readRDS("./data/panC.geneDesc.sample.R3.4.rds"),
                                   "geneTableLong"=readRDS("./data/panC.geneTable.sample.cancerType.R3.4.rds"),
                                   "colSet"=readRDS("./data/panC.freq.all.colSet.list.rds"),
                                   "meta.perCell"=readRDS("./data/panC.metaInfo.perCell.SampleData.rds"),
                                   "plots"=list())
g.dat.list[["SampleData"]][["colSet"]][["meta.cluster"]] <- g.dat.list[["SampleData"]][["colSet"]][["meta.cluster"]][unique(sort(as.character(colData(g.dat.list[["SampleData"]][["sce.perMini"]])[["meta.cluster"]])))]
reducedDim(g.dat.list[["SampleData"]][["sce.perMini"]],"u.UMAP") <- reducedDim(g.dat.list[["SampleData"]][["sce.perMini"]],"harmony.umap")
#g.dat.list[["metaInfo"]] <- readRDS("./data/panC.metaInfo.perCell.rds")
## push to ghe Global Env
assign("g.dat.list",g.dat.list,envir = .GlobalEnv)

#g.colSet <- readRDS("./data/panC.freq.all.colSet.list.rds")
###g.colSet$cancerType["panC"] <- "#f2f3f4"
###saveRDS(g.colSet,"./data/panC.freq.all.colSet.list.rds",version = 2)



# Read the dataset map file----
g.dataset_map <- read.csv("./data/dataset_map.csv", row.names = 1, head = T, stringsAsFactors = F)
g.dataset_tree <- datasetmap2list(g.dataset_map)

# Shiny Server----
server <- function(input, output, session) {
  # Close the server----
  observeEvent(input$close, {
    js$closeWindow()
    stopApp()
  })

  # # Loggin Page----
  credentials <- callModule(
    shinyauthr::login,
    "login",
    data = user_base,
    user_col = user,
    pwd_col = password,
    log_out = reactive(logout_init())
  )

  logout_init <-
    callModule(shinyauthr::logout,
               "logout",
               reactive(credentials()$user_auth))

  output$Login_logged <- reactive({
    req(credentials()$user_auth)
  })
  outputOptions(output, 'Login_logged', suspendWhenHidden = FALSE)

  # Load data tree-----
  output$LoadDataTree <- renderTree({
    g.dataset_tree
  })

  # Dynamically  fill in the content of some UI----
  updateSelectizeInput(session, 'GeneInput_saved', choices = Saved_genes_panel, selected = NULL)

  # Debug info----
  # output$EnvirementAA <- renderPrint({
  #   #Sys.getenv()
  # })
  # output$EnvirementBB <- renderPrint({
  #   #sessionInfo()
  # })
### output$EnvirementCC <- renderPrint({
###   #R.utils::System$getHostname()
###   #system("ifconfig", intern=TRUE)
###   system("pwd", intern=TRUE)
###   print(InputValue$debug.list)
###   #print(.GlobalEnv$exp.perMini.filepath)
###   #print(sce.perMeta)
###   #rjson::fromJSON(readLines("http://api.hostip.info/get_json.php", warn=F))$ip
### })

  # Interactive load button----
  observeEvent(input$LoadButton, {
    req(input$LoadDataTree)
    LoadData_selected <- get_selected(input$LoadDataTree, format = "classid") # Record the dataset to load
    LoadData_selected <- LoadData_selected[LoadData_selected %in% g.dataset_map$DatasetName]
    if (is.null(LoadData_selected)) {
      LoadData_selected <- character(0)
    }
    LoadData.status <- loadDataSet(LoadData_selected)
    if(any(LoadData.status[["Status"]]=="Failed")){
      showshinyalert(session, "LoadDataset_alert", "Some file(s) not exists!",styleclass = "warning")
    }else{
      if(any(LoadData.status[["Status"]]=="Succeeded")){
        showshinyalert(session, "LoadDataset_alert", "Done.",styleclass = "success")
      }else{
        showshinyalert(session, "LoadDataset_alert", "Existed dataset not loaded",styleclass = "info")
      }
    }
    #dataset.available <- unique(c(LoadData.status[LoadData.status$Status!="Failed","Dataset"],
    #                              "SampleData"))
    dataset.available <- names(.GlobalEnv$g.dat.list)
    updateSelectInput(
      session,
      inputId = "SelectData_dataset",
      choices = dataset.available,
      selected = dataset.available[1]
    )
  })

  # Update the panel----
  # Update the UI require selected data ( SubsetData panel, gene available, reducedDims available)
  observe({
    SubsetData_panel_list <- my.getPanelList(.GlobalEnv$g.dat.list,input$SelectData_dataset)

    updateSelectizeInput(session, 'GeneInput_text',
                         choices = SubsetData_panel_list$geneSymbol,
                         selected = "CXCL13")
    updateSelectizeInput(session, 'Embedding_used1',
                         choices = SubsetData_panel_list$reducedDimNames,
                         selected = intersect(c("u.UMAP",SubsetData_panel_list$reducedDimNames[1]),
                                              SubsetData_panel_list$reducedDimNames)[1] )
    updateSelectizeInput(session, 'Embedding_colorby',
                         choices = SubsetData_panel_list$metaInfo.col,
                         selected = "meta.cluster")
    updateSelectizeInput(session, 'Embedding_gene_splitBy',
                         choices = c("None",SubsetData_panel_list$metaInfo.col),
                         selected = "None")
    updateSelectizeInput(session, 'Embedding_metaInfo_splitBy',
                         choices = c("None",SubsetData_panel_list$metaInfo.col),
                         selected = "None")
    updateSelectizeInput(session, 'Distribution_groupby',
                         choices = SubsetData_panel_list$metaInfo.col,
                         selected = "meta.cluster")
    updateSelectizeInput(session, 'Distribution_splitBy',
                         choices = c("None",SubsetData_panel_list$metaInfo.col),
                         selected = "None")
    updateCheckboxGroupInput(session, 'Heatmap_colorBy',
                         choices = intersect(c("meta.cluster","cancerType","dataset"),
                                             SubsetData_panel_list$metaInfo.col),
                         selected = c("meta.cluster","cancerType"))
    # updatePickerInput(
    #   session = session,
    #   inputId = "Distribution_groupIn",
    #   choices = SubsetData_panel_list[[input$Distribution_groupby]],
    #   selected = SubsetData_panel_list[[input$Distribution_groupby]]
    # )
    #### signature gene
    updateSelectizeInput(
      session = session,
      inputId = "SignatureGene_meta.cluster",
      choices = SubsetData_panel_list$meta.cluster,
      selected = if("CD8.c12.Tex.CXCL13" %in% SubsetData_panel_list$meta.cluster) "CD8.c12.Tex.CXCL13" else SubsetData_panel_list$meta.cluster[1]
    )
    #### right panel
    updatePickerInput(
      session = session,
      inputId = "SubsetData_meta.cluster",
      choices = SubsetData_panel_list$meta.cluster,
      selected = SubsetData_panel_list$meta.cluster
    )
    updatePickerInput(
      session = session,
      inputId = "SubsetData_cancerType",
      choices = SubsetData_panel_list$cancerType,
      selected = SubsetData_panel_list$cancerType
    )
    updatePickerInput(
      session = session,
      inputId = "SubsetData_study",
      choices = SubsetData_panel_list$study,
      selected = SubsetData_panel_list$study
    )
    updatePickerInput(
      session = session,
      inputId = "SubsetData_tissue",
      choices = SubsetData_panel_list$tissue,
      selected = SubsetData_panel_list$tissue
    )
  })
  # Update the GeneInput panel
  observe({
    updateTextAreaInput(
      session = session,
      inputId = "GeneInput_saved_text",
      value = input$GeneInput_saved
    )
    if (length(input$GeneInput_file$datapath) != 0) {
      df <-
        read.csv(
          input$GeneInput_file$datapath,
          header = T,
          stringsAsFactors = F
        )
      genes_upload <- paste(as.character(df$Symbol), collapse = ",")
      updateTextAreaInput(session = session,
                          inputId = "GeneInput_file_text",
                          value = genes_upload)
    }
  })

  # Initialize InputValue----
  InputValue <- reactiveValues(
    dataset = "SampleData",
    geneinput = "CXCL13",
    ngene.show = 1,
    #geneinfo = checkGeneList(isolate("CD14"), "Initialize"),
    # plot_data = getPlotData(
    #   Genes = "CD14",
    #   SelectData_dataset = "Initialize",
    #   GlobalCluster.selected = "Myeloid cell",
    #   SubCluster.selected = getPanelList("Initialize")$SubCluster_list[["Myeloid cell"]],
    #   Sample.selected = getPanelList("Initialize")$Sample,
    #   Tissue.selected = getPanelList("Initialize")$Tissue,
    #   Treatment.selected = getPanelList("Initialize")$Treatment,
    #   Day.selected = getPanelList("Initialize")$Day
    # ),
    sce.plot=.GlobalEnv$g.dat.list[["SampleData"]][["sce.perMini"]]["CXCL13",],
    #sce.perMeta = .GlobalEnv$g.dat.list[["SampleData"]][["sce.perMeta"]],
    geneDesc = .GlobalEnv$g.dat.list[["SampleData"]][["geneDesc"]],
    geneTableLong = .GlobalEnv$g.dat.list[["SampleData"]][["geneTableLong"]],
    colSet = .GlobalEnv$g.dat.list[["SampleData"]][["colSet"]],
    meta.perCell = .GlobalEnv$g.dat.list[["SampleData"]][["meta.perCell"]],
    #plots = .GlobalEnv$g.dat.list[["SampleData"]][["plots"]],
    ####
    ##### for debug #####
    debug.list=list()
    #####

  )

  # Initialize in-silico FACS DEGenes table----
  FACS_DEGenes <- reactiveValues(
    limma = data.frame(logFC = 0, adj.P.Val = 0, Warning = "Please click the 'Calculate' button!")
  )

  # Update InputValue----
  observeEvent(input$GeneInput_submit, {
    ###### dataset
    InputValue$dataset <- input$SelectData_dataset
    ###### geneinput
    if (input$GeneInput_panel == "Genes") {
      geneinput.str <- toupper(input$GeneInput_text)
    } else if (input$GeneInput_panel == "Saved") {
      geneinput.str <- toupper(input$GeneInput_saved_text)
    } else if (input$GeneInput_panel == "Upload") {
      geneinput.str <- toupper(input$GeneInput_file_text)
    }
    geneinput.str <- unlist(strsplit(geneinput.str, ",|\\|| |\n|\t"))
    geneinput.str <- unique(geneinput.str[geneinput.str!=""])
    if(length(geneinput.str)>=1){
      f.gene.rpesent <- geneinput.str %in% rowData(.GlobalEnv$g.dat.list[[InputValue$dataset
                                                               ]][["sce.perMini"]])$display.name
      if(!all(f.gene.rpesent)){
        message <- sprintf("Genes not found: %s",paste(geneinput.str[!f.gene.rpesent],collapse = ","))
        sendSweetAlert(session = session, title = message, type = "warning")
      }
      InputValue$geneinput <- geneinput.str[f.gene.rpesent]
      InputValue$ngene.show <- sum(f.gene.rpesent)
    }else{
      message <- "No gene found!"
      sendSweetAlert(session = session, title = message, type = "error")
    }

    sce.ref <- .GlobalEnv$g.dat.list[[InputValue$dataset]][["sce.perMini"]]
    
    f.sce.cell <- (sce.ref[["meta.cluster"]] %in% input$SubsetData_meta.cluster) &
      (sce.ref[["cancerType"]] %in% input$SubsetData_cancerType) &
      (sce.ref[["dataset"]] %in% input$SubsetData_study)
    if(!is.null(input$SubsetData_tissue) && "loc" %in% colnames(colData(sce.ref)) ){
      f.sce.cell <- f.sce.cell & (sce.ref[["loc"]] %in% input$SubsetData_tissue)
    }
    
    f.sce.gene <- match(InputValue$geneinput,rowData(sce.ref)$display.name)
    InputValue$sce.plot <- sce.ref[f.sce.gene,f.sce.cell]
    InputValue$meta.perCell <- .GlobalEnv$g.dat.list[[InputValue$dataset]][["meta.perCell"]]
    ##### signature gene
    #InputValue$sce.perMeta <- .GlobalEnv$g.dat.list[[InputValue$dataset]][["sce.perMeta"]]
    InputValue$geneDesc <- .GlobalEnv$g.dat.list[[InputValue$dataset]][["geneDesc"]]
    InputValue$geneTableLong <- .GlobalEnv$g.dat.list[[InputValue$dataset]][["geneTableLong"]]
    InputValue$colSet <- .GlobalEnv$g.dat.list[[InputValue$dataset]][["colSet"]]
    InputValue$colSet[["meta.cluster"]] <- InputValue$colSet[["meta.cluster"]][unique(sort(as.character(colData(InputValue$sce.plot)[["meta.cluster"]])))]
    #InputValue$plots <- .GlobalEnv$g.dat.list[[InputValue$dataset]][["plots"]]
    
  })

  # Update in-silico FACS DEG calculation----
  observeEvent(input$FACS_DEG_submit, {
    FACS_DEGenes$limma <- getLIMMAData(
      Plot.data = InputValue$plot_data,
      SelectData_dataset = InputValue$dataset,
      genes = InputValue$geneinfo$right_gene,
      x.cutoff = input$FACS_xcutoff,
      y.cutoff = input$FACS_xcutoff,
      group1 = input$FACS_DEG_group1,
      group2 = input$FACS_DEG_group2
    )
  })

  # Embedding Plot----
  output$Embedding_geneplot <- renderPlot({
    if(InputValue$ngene.show>0){
      if(input$Embedding_GenePar=="Mean"){
          sce.mean <- sscVis::ssc.average.gene(InputValue$sce.plot)
          p <- sscVis::ssc.plot.tsne(sce.mean,
                                      gene=rowData(sce.mean)$display.name,
                                      ##gene=(InputValue$geneinput),
                                      reduced.name = input$Embedding_used1,
                                      #vector.friendly=T,
                                      vector.friendly=input$Embedding_gene_download_vf,
                                      par.geom_point = list(scale=input$Embedding_gene_raster_scale),
                                      #clamp=c(-0.5,1.5),
                                      clamp=c(input$Embedding_exp_bLo,input$Embedding_exp_bHi),
                                      do.scale=ifelse(input$Embedding_do_scale=="Yes",T,F),
                                      p.ncol=input$PlotPar_ncol,
                                      theme.use=theme_void,
                                      size=input$Embedding_dotsize1,
                                      palette.name=input$Embedding_colorpanel,
                                      splitBy = if(input$Embedding_gene_splitBy=="None") NULL else input$Embedding_gene_splitBy,
                                      par.geneOnTSNE=list(scales="fixed",pt.order="random",pt.alpha = 0.5))
          .GlobalEnv$g.dat.list[[InputValue$dataset]][["plots"]][["Embedding_geneplot"]] <- p
          print(p)
      }else{
          if(InputValue$ngene.show<=30){
            p <- sscVis::ssc.plot.tsne(InputValue$sce.plot,
                                gene=rowData(InputValue$sce.plot)$display.name,
                                ##gene=(InputValue$geneinput),
                                reduced.name = input$Embedding_used1,
                                #vector.friendly=T,
                                vector.friendly=input$Embedding_gene_download_vf,
                                par.geom_point = list(scale=input$Embedding_gene_raster_scale),
                                #clamp=c(-0.5,1.5),
                                clamp=c(input$Embedding_exp_bLo,input$Embedding_exp_bHi),
                                do.scale=ifelse(input$Embedding_do_scale=="Yes",T,F),
                                p.ncol=input$PlotPar_ncol,
                                theme.use=theme_void,
                                size=input$Embedding_dotsize1,
                                palette.name=input$Embedding_colorpanel,
                                splitBy = if(input$Embedding_gene_splitBy=="None") NULL else input$Embedding_gene_splitBy,
                                par.geneOnTSNE=list(scales="fixed",pt.order="random",pt.alpha = 0.5))
            .GlobalEnv$g.dat.list[[InputValue$dataset]][["plots"]][["Embedding_geneplot"]] <- p
            print(p)
            
          }else{
              sendSweetAlert(session = session,
                             title = sprintf("Too many genes (%d>30)! Please select \"Mean\" in the \"Multi gene\" or input less genes",
                                             InputValue$ngene.show),
                             type = "warning")
          }
      }
    }
  })

  output$Embedding_geneplot.ui <- renderUI({
    plotOutput(
      "Embedding_geneplot",
      #width = InputValue$plot_width,
      #height = InputValue$plot_height
      width = input$PlotSize_width,
      height = input$PlotSize_height
    )
  })

  output$Embedding_metaplot <- renderPlot({
    p <- sscVis::ssc.plot.tsne(InputValue$sce.plot,
                                gene=NULL,
                                #columns=InputValue$Embedding_colorby,
                                columns=unlist(strsplit(input$Embedding_colorby,",")),
                                #reduced.name=InputValue$Embedding_used,
                                reduced.name = input$Embedding_used1,
                                #vector.friendly=F,
                                vector.friendly=input$Embedding_metaInfo_download_vf,
                                par.geom_point = list(scale=input$Embedding_metaInfo_raster_scale),
                                p.ncol=input$PlotPar_ncol2,
                                theme.use=theme_void,
                                size=input$Embedding_dotsize2,colSet = InputValue$colSet,
                                label=if(input$Embedding_metaInfo_labelSize==0) NULL else input$Embedding_metaInfo_labelSize,
                                #plotDensity = F,
                                splitBy = if(input$Embedding_metaInfo_splitBy=="None") NULL else input$Embedding_metaInfo_splitBy,
                                par.geneOnTSNE=list(scales="fixed",pt.order="random",pt.alpha = 0.5))
    .GlobalEnv$g.dat.list[[InputValue$dataset]][["plots"]][["Embedding_metaplot"]] <- p
    #assign('g.dat.list',c(ret.dat.list,.GlobalEnv$g.dat.list),envir=.GlobalEnv)
    #InputValue$plots[["Embedding_metaplot"]] <- p
    print(p)

  })

  output$Embedding_metaplot.ui <- renderUI({
    plotOutput(
      "Embedding_metaplot",
      #width = InputValue$plot_width2,
      #height = InputValue$plot_height2
      width = input$PlotSize_width2,
      height = input$PlotSize_height2
    )
  })

  output$Embedding_gene_download <- downloadHandler(
    filename = function() {
      sprintf("%s.Gene.%s.%s.%s",
              "EmbeddingPlot",
              input$SelectData_dataset,
              stringi::stri_rand_strings(1, 10),
              input$Embedding_gene_download_type)
    },
    content = function(file) {
      if(input$Embedding_gene_download_type=="pdf"){
        pdf(file, width = input$PlotSize_width / 75, height = input$PlotSize_height / 75)
      }else{
        png(file, width = input$PlotSize_width*4, height = input$PlotSize_height*4)
      }
      if(InputValue$ngene.show>0){
        print(.GlobalEnv$g.dat.list[[InputValue$dataset]][["plots"]][["Embedding_geneplot"]])
      }
      dev.off()
    }
  )

  output$Embedding_metaInfo_download <- downloadHandler(
    filename = function() {
      sprintf("%s.metaInfo.%s.%s.%s",
              "EmbeddingPlot",
              input$SelectData_dataset,
              stringi::stri_rand_strings(1, 10),
              input$Embedding_metaInfo_download_type)
    },
    content = function(file) {
          if(input$Embedding_metaInfo_download_type=="pdf"){
              pdf(file, width = input$PlotSize_width2 / 75, height = input$PlotSize_height2 / 75)
          }else{
              png(file, width = input$PlotSize_width2*4, height = input$PlotSize_height2*4)
          }
          print(.GlobalEnv$g.dat.list[[InputValue$dataset]][["plots"]][["Embedding_metaplot"]])
          #print(p)
          dev.off()
    }
  )

  # Distribution Plot----
  output$Distribution_plot <- renderPlot({
    if(InputValue$ngene.show>0){
      if(input$Distribution_GenePar=="Mean"){
        sce.mean <- ssc.average.gene(InputValue$sce.plot)
        p <- sscVis::ssc.plot.violin(sce.mean,
                                      #gene=(InputValue$geneinput),
                                      gene=rowData(sce.mean)$display.name,
                                      #par.legend = list(breaks=c(-1.5,-1,0,1,2,3)),
                                      par.legend = list(breaks=seq(input$Distribution_exp_bLo,
                                                                   input$Distribution_exp_bHi, input$Distribution_exp_bStep)),
                                      group.var = input$Distribution_groupby,
                                      splitBy = if(input$Distribution_splitBy=="None") NULL else input$Distribution_splitBy,
                                      #clamp=c(-1.5,3),
                                      clamp=c(input$Distribution_exp_bLo,input$Distribution_exp_bHi),
                                      do.scale=ifelse(input$Distribution_do_scale=="Yes",T,F),
                                      group.in = NULL,
                                      add=c("boxplot"),
                                      palette.name = input$Distribution_colorpanel,
                                      p.ncol = input$Distribution_PlotNCol)
        .GlobalEnv$g.dat.list[[InputValue$dataset]][["plots"]][["Distribution_plot"]] <- p
        print(p)
      }else{
        if(InputValue$ngene.show<=30){
          p <- sscVis::ssc.plot.violin(InputValue$sce.plot,
                                        #gene=(InputValue$geneinput),
                                        gene=rowData(InputValue$sce.plot)$display.name,
                                        #par.legend = list(breaks=c(-1.5,-1,0,1,2,3)),
                                        par.legend = list(breaks=seq(input$Distribution_exp_bLo,
                                                                     input$Distribution_exp_bHi, input$Distribution_exp_bStep)),
                                        group.var = input$Distribution_groupby,
                                        splitBy = if(input$Distribution_splitBy=="None") NULL else input$Distribution_splitBy,
                                        #clamp=c(-1.5,3),
                                        clamp=c(input$Distribution_exp_bLo,input$Distribution_exp_bHi),
                                        do.scale=ifelse(input$Distribution_do_scale=="Yes",T,F),
                                        group.in = NULL,
                                        add=c("boxplot"),
                                        palette.name = input$Distribution_colorpanel,
                                        p.ncol = input$Distribution_PlotNCol)
          .GlobalEnv$g.dat.list[[InputValue$dataset]][["plots"]][["Distribution_plot"]] <- p
          print(p)
        }else{
          sendSweetAlert(session = session,
                         title = sprintf("Too many genes (%d>30)! Please select \"Mean\" in the \"Multi gene\" or input less genes",
                                         InputValue$ngene.show),
                         type = "warning")
        }
      }
    }
  })

  output$Distribution_plot.ui <- renderUI({
    plotOutput(
      "Distribution_plot",
      #width =  InputValue$plot_width_distribution,
      #height = InputValue$plot_height_distribution
      width = input$Distribution_PlotWidth,
      height = input$Distribution_PlotHeight
    )
  })

  output$Distribution_download <- downloadHandler(
    filename = function() {
      sprintf("%s.%s.%s.pdf",
              "DistributionPlot",
              input$SelectData_dataset,
              stringi::stri_rand_strings(1, 10))
    },
    content = function(file) {
      pdf(
        file,
        width = input$Distribution_PlotWidth / 75,
        height = input$Distribution_PlotHeight / 75
      )
      if(InputValue$ngene.show>0){
        print(.GlobalEnv$g.dat.list[[InputValue$dataset]][["plots"]][["Distribution_plot"]])
      }
      dev.off()
    }
  )

  # Significance plot----
  output$Significance_group <- DT::renderDataTable({
    #x.tb <- metadata(InputValue$sce.perMeta)$ssc$gene.desc.top
    x.tb <- InputValue$geneDesc
    col.available <- colnames(x.tb)
    tb.col.show <- c("geneSymbol",
                     ##"meta.cluster",
                     "cluster.name","comb.ES","comb.padj","sig",
                     "OR.comb","OR.comb.adj.pvalue",
                     "comb.ES.rank", "comb.positive.freq", 
                     "cancerType.sig.N", "cancerType.sig.freq", "cancerType.total.N", 
                     "cancerType.ES.max", "cancerType.ES.max.p.adj",
                     "dataset.sig.N", "dataset.sig.freq", "dataset.total.N", 
                     "dataset.ES.max", "dataset.ES.max.p.adj",
                     grep("^geneSet",col.available,perl = T,value = T))
    tb.col.show <- intersect(tb.col.show,col.available)
    col.double <- tb.col.show[sapply(tb.col.show,function(x){ is.numeric(x.tb[[x]]) && !is.integer(x.tb[[x]]) && !grepl("freq",x,perl=T) })]
    col.perc <- tb.col.show[sapply(tb.col.show,function(x){ is.numeric(x.tb[[x]]) && !is.integer(x.tb[[x]]) && grepl("freq",x,perl=T) })]
    DT::datatable(
      x.tb[, tb.col.show, with =F],
      rownames = F,
      class = "stripe table-condensed",
      filter = "top",
      options = list(
        scrollX = TRUE,
        processing = FALSE,
        pageLength = 10
      ),
      autoHideNavigation = F
    ) %>%
      DT::formatRound(col.double,2) %>%
      DT::formatPercentage(col.perc,2)
#      DT::formatRound(c("comb.ES","comb.padj",
#                        "cancerType.ES.max","cancerType.ES.max.p.adj",
#                        "dataset.ES.max","dataset.ES.max.p.adj"),4) %>%
#      DT::formatPercentage(c("comb.positive.freq","cancerType.sig.freq","dataset.sig.freq"),2)
    # {
    #   DT::datatable(data.frame(Warnings = "There needs at least two groups!"))
    # }
  })

  output$Significance_plot <- renderPlot({
    if(InputValue$ngene.show>0){
      if(InputValue$ngene.show<=30){
        gene.long.collapsed.tb <- InputValue$geneTableLong[meta.cluster==input$SignatureGene_meta.cluster &
                                                             geneID %in% InputValue$geneinput,]
        p <- makeFig.ExampleGeneBarplot(gene.to.plot=InputValue$geneinput,
                                   mcls.plot=input$SignatureGene_meta.cluster,
                                   gene.long.tb=gene.long.collapsed.tb,
                                   gene.long.collapsed.tb=gene.long.collapsed.tb,
                                   gene.desc.top=InputValue$geneDesc,
                                   ##mod.sort=if(input$SignatureGene_groupBy=="study") 2 else 3,
                                   mod.sort=3,
                                   th.dprime=0.15,
                                   colSet.cancerType=InputValue$colSet$cancerType,
                                   ncol=input$SignatureGene_PlotNCol)
        .GlobalEnv$g.dat.list[[InputValue$dataset]][["plots"]][["Significance_plot"]] <- p
        print(p)
      }else{
        sendSweetAlert(session = session,
                       title = sprintf("Too many genes (%d>30)! Please select \"Mean\" in the \"Multi gene\" or input less genes",
                                       InputValue$ngene.show),
                       type = "warning")
      }
    }
  })

  output$Significance_plot.ui <- renderUI({
    plotOutput(
      "Significance_plot",
      width = input$SignatureGene_PlotWidth,
      height = input$SignatureGene_PlotHeight
    )
  })

  output$Significance_download <- downloadHandler(
    filename = function() {
      sprintf("%s.%s.%s.%s.pdf",
              "SignatureGenePlot",
              input$SelectData_dataset,
              input$SignatureGene_meta.cluster,
              stringi::stri_rand_strings(1, 10))
    },
    content = function(file) {
      pdf(
        file,
        width = input$SignatureGene_PlotWidth/75,
        height = input$SignatureGene_PlotHeight/75
      )
      if(InputValue$ngene.show>0){
        if(InputValue$ngene.show<=30){
          print(.GlobalEnv$g.dat.list[[InputValue$dataset]][["plots"]][["Significance_plot"]])
        }
      }
      dev.off()
    }
  )

  ####################### Heatmap #########################
  output$Heatmap_plot <- renderPlot({
    if(InputValue$ngene.show>1){
      choice.ave.by <- c("meta.cluster","cancerType","dataset")
      used.ave.by <- choice.ave.by[seq_len(match(input$Heatmap_aveBy,choice.ave.by))]

      if(input$Heatmap_do_scale=="Yes"){
          sce.plot.z <- ssc.scale(InputValue$sce.plot,
                                  gene.symbol=rowData(InputValue$sce.plot)$display.name,
                                  assay.name="exprs",
                                  adjB=if("batchV" %in% colnames(colData(InputValue$sce.plot))) "batchV" else NULL,
                                  do.scale=T)
          assay(sce.plot.z,"exprs") <- assay(sce.plot.z,"exprs.scale")
      }else{
          sce.plot.z <- InputValue$sce.plot
      }

      ht <- ssc.plot.heatmap(sce.plot.z,
                       ##ave.by = if(input$Heatmap_aveby==F) NULL else  c("meta.cluster","cancerType","dataset"),
                       ave.by = used.ave.by,
                       columns = used.ave.by,
                       columns.order = "meta.cluster",
                       column.split = NULL,do.scale = F,
                       do.clustering.row = input$Heatmap_clustering_row,
                       do.clustering.col = input$Heatmap_clustering_col,
                       clustering.method = "ward.D2",
                       colSet = InputValue$colSet[c("meta.cluster","cancerType")],
                       palette.name = input$Heatmap_colorpanel,
                       z.lo = -input$Heatmap_zMax,z.hi = input$Heatmap_zMax,z.step = 0.5,
                       mytitle = "Heatmap",returnHT = T,ann.bar.height = 0.5)
      .GlobalEnv$g.dat.list[[InputValue$dataset]][["plots"]][["Heatmap_plot"]] <- ht
      ht
    }else{
      ggplot(data.frame()) +
        ggtitle("There should be at least two genes") +
        theme_bw() +
        theme(plot.title = element_text(
          size = 26,
          face = "bold",
          hjust = 0.5
        ))
    }
  })

  output$Heatmap_plot.ui <- renderUI({
    plotOutput(
      "Heatmap_plot",
      width = input$Heatmap_PlotWidth,
      height = input$Heatmap_PlotHeight
    )
  })

  output$Heatmap_download <- downloadHandler(
    filename = function() {
      sprintf("%s.%s.%s.pdf",
              "HeatmapPlot",
              input$SelectData_dataset,
              stringi::stri_rand_strings(1, 10))
    },
    content = function(file) {
      pdf(
        file,
        width = input$Heatmap_PlotWidth/75,
        height = input$Heatmap_PlotHeight/75
      )
      opar <- par(mar=c(1,1,1,1))
      plot.new()
      ###title(main = mytitle,cex.main=2)
      vps <- gridBase::baseViewports()
      grid::pushViewport(vps$inner, vps$figure, vps$plot)
      ht <- .GlobalEnv$g.dat.list[[InputValue$dataset]][["plots"]][["Heatmap_plot"]]
      ComplexHeatmap::draw(ht, newpage= FALSE,merge_legends = TRUE,split=NULL)
      dev.off()
      ##### cannot set par here, raise error
      ####par(opar)
    }
  )
  #########################################################

  ####################### DotPlot #########################
  output$DotPlot <- renderPlot({
    if(InputValue$ngene.show>0){
        es.step <- (input$DotPlot_esMax-input$DotPlot_esMin)/3
        es.breaks <- seq(from=input$DotPlot_esMin,to=input$DotPlot_esMax,by=es.step)
        pp <- plotDotPlotFromGeneTable(gene.tb=InputValue$geneDesc[geneID %in% InputValue$geneinput,],
                                 group.tb=NULL,
                                 col.palette=input$DotPlot_colorpanel,
                                 clamp=c(input$DotPlot_esMin,input$DotPlot_esMax),
                                 col.breaks=es.breaks,
                                 par.size=list(breaks=es.breaks,
                                               range=c(0.2,6),
                                               labels=es.breaks,
                                               limits=c(input$DotPlot_esMin,input$DotPlot_esMax)*1))
        .GlobalEnv$g.dat.list[[InputValue$dataset]][["plots"]][["DotPlot"]] <- pp
        print(pp)
    }else{
      ggplot(data.frame()) +
        ggtitle("There should be at least one gene") +
        theme_bw() +
        theme(plot.title = element_text(
          size = 26,
          face = "bold",
          hjust = 0.5
        ))
    }
  })

  output$DotPlot.ui <- renderUI({
    plotOutput(
      "DotPlot",
      width = input$DotPlot_Width,
      height = input$DotPlot_Height
    )
  })

  output$DotPlot_download <- downloadHandler(
    filename = function() {
      sprintf("%s.%s.%s.pdf",
              "DotPlot",
              input$SelectData_dataset,
              stringi::stri_rand_strings(1, 10))
    },
    content = function(file) {
      pdf(
        file,
        width = input$DotPlot_Width/75,
        height = input$DotPlot_Height/75
      )
      print(.GlobalEnv$g.dat.list[[InputValue$dataset]][["plots"]][["DotPlot"]])
      dev.off()
    }
  )

  #########################################################

  # In-silico FACS Plot----
  # output$FACS_plot <- renderPlot({
  #   # DoISFACSPlot(
  #   #   plot.data = InputValue$plot_data,
  #   #   genes = InputValue$geneinfo$right_gene,
  #   #   color.by = input$FACS_colorby,
  #   #   color.panel = input$FACS_colorpanel,
  #   #   dot.size = input$FACS_dotsize,
  #   #   x.cutoff = input$FACS_xcutoff,
  #   #   y.cutoff = input$FACS_ycutoff,
  #   #   font.size = input$PlotPar_fontsize
  #   # )
  # })
  #
  # output$FACS_plot.ui <- renderUI({
  #   plotOutput("FACS_plot",
  #              width = InputValue$plot_width,
  #              height = InputValue$plot_height)
  # })
  #
  # output$FACS_download <- downloadHandler(
  #   filename = function() {
  #     paste0(
  #       input$SelectData_dataset, "_FACSPlot_", stringi::stri_rand_strings(1, 10), ".pdf"
  #     )
  #   },
  #   content = function(file) {
  #     # pdf(file,
  #     #     width = InputValue$plot_width / 80,
  #     #     height = InputValue$plot_height / 80)
  #     # DoISFACSPlot(
  #     #   plot.data = InputValue$plot_data,
  #     #   genes = InputValue$geneinfo$right_gene,
  #     #   color.by = input$FACS_colorby,
  #     #   color.panel = input$FACS_colorpanel,
  #     #   dot.size = input$FACS_dotsize,
  #     #   x.cutoff = input$FACS_xcutoff,
  #     #   y.cutoff = input$FACS_ycutoff,
  #     #   font.size = input$PlotPar_fontsize
  #     # )
  #     # dev.off()
  #   }
  # )
  #
  # output$FACS_volcano_plot <- renderPlot({
  #   # DoISFACSVolcanoPlot(
  #   #   limma.data = FACS_DEGenes$limma,
  #   #   dot.size = input$FACS_Volcano_dotsize,
  #   #   adj.P.Val.cutoff = input$FACS_Volcano_Pvalue_cutoff,
  #   #   logFC.cutoff = input$FACS_Volcano_logFC_cutoff,
  #   #   ngenes.labeled = input$FACS_Volcano_ngenes_labeled,
  #   #   font.size = input$FACS_Volcano_fontsize
  #   # )
  # })
  #
  # output$FACS_volcano_plot.ui <- renderUI({
  #   plotOutput("FACS_volcano_plot",
  #              width = InputValue$plot_width,
  #              height = InputValue$plot_height)
  # })
  #
  # output$FACS_volcano_download <- downloadHandler(
  #   filename = function() {
  #     paste0(
  #       input$SelectData_dataset, "_Volcano_Plot_", stringi::stri_rand_strings(1, 10), ".pdf"
  #     )
  #   },
  #   content = function(file) {
  #     # pdf(file,
  #     #     width = InputValue$plot_width / 80,
  #     #     height = InputValue$plot_height / 80)
  #     # plot(
  #     #   DoISFACSVolcanoPlot(
  #     #     limma.data = FACS_DEGenes$limma,
  #     #     dot.size = input$FACS_Volcano_dotsize,
  #     #     adj.P.Val.cutoff = input$FACS_Volcano_Pvalue_cutoff,
  #     #     logFC.cutoff = input$FACS_Volcano_logFC_cutoff,
  #     #     ngenes.labeled = input$FACS_Volcano_ngenes_labeled,
  #     #     font.size = input$FACS_Volcano_fontsize
  #     #   )
  #     # )
  #     # dev.off()
  #   }
  # )
  #
  # output$FACS_DEGDataTable_output <- DT::renderDataTable({
  #   # DT::datatable(
  #   #   FACS_DEGenes$limma %>% arrange(adj.P.Val) %>% head(2000) %>% arrange(desc(logFC)) %>% mutate_if(is.character, as.factor),
  #   #   class = "stripe table-condensed",
  #   #   options = list(pageLength = 10, scrollX = TRUE)
  #   # )
  # })
  #
  # output$FACS_DEGDataTable_download <- downloadHandler(
  #   filename = function() {
  #     paste0(
  #       input$SelectData_dataset, "_", InputValue$geneinfo$right_gene,
  #       "_X", input$FACS_xcutoff,
  #       "_Y", input$FACS_ycutoff,
  #       "_", input$FACS_DEG_group1,
  #       "_", input$FACS_DEG_group2,
  #       "_", stringi::stri_rand_strings(1, 10),
  #       ".csv"
  #     )
  #   },
  #   content = function(file) {
  #     # write.csv(FACS_DEGenes$limma, file, row.names = TRUE)
  #   }
  # )

  # Metadata plot----
  output$Metadata_plot <- renderPlot({
    p <- do.call(sscVis::plotDistFromCellInfoTable,
                 c(list(obj=InputValue$meta.perCell,
                        plot.type = input$Metadata_plottype,
                        out.prefix = NULL,
                        test.method = "",
                        cmp.var = input$Metadata_cmp,
                        sort.freq = T,
                        alpha=0.5,
                        min.NTotal=30,
                        facet.ncol = input$Metadata_PlotNCol,
                        #fill=if(input$Metadata_plottype!="barplot") "cmp.var" else "",
                        group.var = input$Metadata_groupby1,
                        donor.var = "patient.uid"),
                   if(input$Metadata_plottype!="barplot") list(fill="cmp.var") else list()
                   ))
    if(input$Metadata_cmp %in% c("cancerType","meta.cluster")){
      p <- p + geom_hline(yintercept=0.10,linetype=2) +
        scale_color_manual(values=InputValue$colSet[[input$Metadata_cmp]]) +
        scale_fill_manual(values=InputValue$colSet[[input$Metadata_cmp]])
    }
    .GlobalEnv$g.dat.list[[InputValue$dataset]][["plots"]][["Metadata_plot"]] <- p
    print(p)
  })

  output$Metadata_plot.ui <- renderUI({
    plotOutput("Metadata_plot",
               width = input$Metadata_PlotWidth,
               height = input$Metadata_PlotHeight)
  })

  output$Metadata_download <- downloadHandler(
    filename = function() {
      paste0(
        input$SelectData_dataset, "_MetadataPlot_", stringi::stri_rand_strings(1, 10), ".pdf"
      )
    },
    content = function(file) {
      #pdf(file, width = InputValue$plot_width / 75, height = InputValue$plot_height / 75)
      pdf(file)
      print(.GlobalEnv$g.dat.list[[InputValue$dataset]][["plots"]][["Metadata_plot"]])
      dev.off()
    }
  )

  # DataTable output----
  # output$DataTable_output <- DT::renderDataTable({
  #   x.tb <- InputValue$meta.perCell
  #   DT::datatable(
  #     InputValue$meta.perCell,
  #     rownames = F,
  #     class = "stripe table-condensed",
  #     filter = "top",
  #     options = list(
  #       scrollX = TRUE,
  #       processing = FALSE,
  #       pageLength = 10
  #     ),
  #     autoHideNavigation = F
  #   )
  # })

  # output$DataTable_download <- downloadHandler(
  #   filename = function() {
  #     paste0(
  #       input$SelectData_dataset, "_DataTablePlot_", stringi::stri_rand_strings(1, 10), ".csv"
  #     )
  #   },
  #   content = function(file) {
  #     write.csv(InputValue$meta.perCell, file, row.names = TRUE)
  #   }
  # )

}

server
# Run the application----
#shinyApp(ui = ui, server = server)
