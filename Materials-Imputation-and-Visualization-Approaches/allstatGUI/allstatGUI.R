#' GUI for Mediation/Moderation Analysis
#' 
#' Graphical user interface for a variety of statistical functions
#' 
#' @author Dean Lim and Geoffrey Hubona
#' @keywords VIMGUI, Mediation, Moderation, Rcmdr, Rattle
#' @export allstatGUI
allstatGUI <- function() {
  
  # Obtain names of all packages on CRAN
  # names.available.packages <- rownames(available.packages())
  
  # Install the necessary packages
  required.packages <- c("Rcmdr","cairoDevice",
                         "foreign","RGtk2","RGtk2Extras","colorspace","grid",
                         "tcltk","tkrplot","VIM","survey","gWidgetsRGtk2",
                         "foreach","doParallel","VIM","VIMGUI","rattle")
  new.packages <- required.packages[!(required.packages %in% installed.packages()[,"Package"])]
  if(length(new.packages)) install.packages(new.packages)
  
  ## Extract packages names that contain Rcmdr
  #Rcmdr.related.packages <- names.available.packages[grep("Rcmdr", names.available.packages)]
  
  ## Install these packages
  #install.packages(pkgs = Rcmdr.related.packages)
  
  # Load the necessary packages
  require("cairoDevice")
  require("foreign")
  require("RGtk2")
  require("RGtk2Extras")
  require("foreach")
  require("doParallel")
  require("VIM")
  require("VIMGUI")
  data(chorizonDL)
  data(sleep)
  data(tao)
  
  ##########################################
  ##### Setting up upper level PLS window
  #load parallel backend for foreach
  num_cores <<- detectCores(logical=TRUE)
  cl <<- makeCluster(num_cores)
  registerDoParallel(cl)
  pls_window <<- gtkWindow(show=FALSE)
  pls_window$setTitle("  MEDIATION and MODERATION ANALYSES  /  DATA IMPUTATION and VISUALIZATION / STATISTICAL ANALYSES with R COMMANDER / DATA MINING with RATTLE  ")
  pls_window$setDefaultSize(1280,720)
  pls_window$setBorderWidth(5)
  vbox <<- gtkVBox()
  pls_window$add(vbox)
  notebook <<- gtkNotebook()
  gtkNotebookSetScrollable(notebook,scrollable=TRUE)
  gtkNotebookPopupEnable(notebook)
  
  # Construct the toolbar itself
  toolbar <<- gtkToolbar()
  
  # create toolbar object for "Import Data Files" dataset action
  files_button <<- gtkToolButton(stock.id = "gtk-file")
  gtkToolButtonSetLabel(files_button,label="  Import Data Files  ")
  
  # create toolbar object for "medmod" action
  medmod_button <<- gtkToolButton(stock.id = "gtk-execute")
  gtkToolButtonSetLabel(medmod_button,label="  Mediator/Moderator Analysis  ")
  
  # create toolbar object for "Missing data Imputation" dataset action
  mi_button <- gtkToolButton(stock.id = "gtk-execute")
  gtkToolButtonSetLabel(mi_button,label=" Visualize and Impute Missing Data ")
  
  # create toolbar object for "RCmdr" action
  everything_button <<- gtkToolButton(stock.id = "gtk-execute")
  gtkToolButtonSetLabel(everything_button,label="  Statistical Analyses  ")
  
  # create toolbar object for "Data Mining with Rattle" action
  dm_button <<- gtkToolButton(stock.id = "gtk-execute")
  gtkToolButtonSetLabel(dm_button,label="  Data Mining with Rattle  ")
  
  # add buttons to toolbar
  toolbar$add(files_button)
  toolbar$add(medmod_button)
  toolbar$add(mi_button)
  toolbar$add(everything_button)
  toolbar$add(dm_button)
  
  # implement the clicked signal for Files toolbar item
  gSignalConnect(files_button, "clicked", function(files_button) {
    ## Files button dialog
    dialog <- gtkMessageDialog(parent=pls_window, 
                               "destroy-with-parent",
                               "question", 
                               "ok-cancel", 
                               " Input a Data File ")
    # choices for radio button
    choices <- c("CSV Data File (.csv)", 
                 "SPSS Data File (.sav)",
                 "Stata Data File (.dta)",
                 "SAS Export File (.xpt)")
    radio_buttons <- NULL
    vbox <- gtkVBox(FALSE, 0)
    for (choice in choices) {
      button <- gtkRadioButton(radio_buttons, choice)
      vbox$add(button)
      radio_buttons <- c(radio_buttons, button)
    }
    
    # set Data file button active
    vbox[[1]]$setActive(TRUE)
    sapply(radio_buttons, '[', "active")
    
    # set up frame
    frame1 <- gtkFrame(" Choose an data file: ")
    frame1$add(vbox)
    dialog[["vbox"]]$add(frame1)
    set.data <- FALSE
    set.spss <- FALSE
    set.stata <- FALSE
    set.sas <- FALSE
    
    if (dialog$run() == GtkResponseType["ok"]) {
      set.data <- vbox[[1]]$getActive()
      set.spss <- vbox[[2]]$getActive()
      set.stata <- vbox[[3]]$getActive()
      set.sas <- vbox[[4]]$getActive()
    }
    dialog$destroy()
    # action to follow based on user choice
    if (set.data==TRUE){
      #### Get .csv data file dialog
      dialog <- gtkFileChooserDialog(title = "Choose a .csv data file: ", 
                                     parent = pls_window, action = "open",
                                     "gtk-ok", GtkResponseType["ok"],
                                     "gtk-cancel", GtkResponseType["cancel"],
                                     show = TRUE)
      # read in file for event signal
      gSignalConnect(dialog, "response", function(dialog, response) {
        if(response == GtkResponseType["ok"]) {
          object <- read.csv(dialog$getFilename(),header=TRUE)
          current_data_file <<- object
          if (exists("files_dir")) setwd(files_dir)
          write.csv(x=object, file="current_data_file.csv")
          if (exists("main_dir")) setwd(main_dir)
        }
        dialog$destroy()
      })
      
      # filter for .csv
      fileFilter <- gtkFileFilter()
      fileFilter$setName(".csv data files")
      fileFilter$addPattern(c("*.csv","*.CSV"))
      dialog$addFilter(fileFilter)
      # filter for all files
      #fileFilter <- gtkFileFilter()
      #fileFilter$setName("all files")
      #fileFilter$addPattern("*")
      #dialog$addFilter(fileFilter)
      #############################################
    } else if (set.spss==TRUE) {
      #### Get .sav data file dialog
      dialog <- gtkFileChooserDialog(title = "Choose an SPSS .sav data file: ",
                                     parent = pls_window, action = "open",
                                     "gtk-ok", GtkResponseType["ok"],
                                     "gtk-cancel", GtkResponseType["cancel"],
                                     show = TRUE)
      # read in file for event signal
      gSignalConnect(dialog, "response", function(dialog, response) {
        if(response == GtkResponseType["ok"]) {
          file.spss <- read.spss(dialog$getFilename())
          df.spss <- data.frame(file.spss)
          current_data_file <<- df.spss
        }
        dialog$destroy()
      })
      
      # filter for .sav data file
      fileFilter <- gtkFileFilter()
      fileFilter$setName("SPSS .sav data files")
      fileFilter$addPattern(c("*.sav","*.SAV"))
      dialog$addFilter(fileFilter)
      # filter for all files
      #fileFilter <- gtkFileFilter()
      #fileFilter$setName("all files")
      #fileFilter$addPattern("*")
      #dialog$addFilter(fileFilter)
      #############################################
    } else if (set.stata==TRUE) {
      #### Get .dta data file dialog
      dialog <- gtkFileChooserDialog(title = "Choose a Stata .dta data file: ",
                                     parent = pls_window, action = "open",
                                     "gtk-ok", GtkResponseType["ok"],
                                     "gtk-cancel", GtkResponseType["cancel"],
                                     show = TRUE)
      # read in file for event signal
      gSignalConnect(dialog, "response", function(dialog, response) {
        if(response == GtkResponseType["ok"]) {
          file.dta <- read.dta(dialog$getFilename())
          df.dta <- file.dta
          current_data_file <<- df.dta
        }
        dialog$destroy()
      })
      
      # filter for .dta data file
      fileFilter <- gtkFileFilter()
      fileFilter$setName("Stata .dta data files")
      fileFilter$addPattern(c("*.dta","*.DTA"))
      dialog$addFilter(fileFilter)
      # filter for all files
      #fileFilter <- gtkFileFilter()
      #fileFilter$setName("all files")
      #fileFilter$addPattern("*")
      #dialog$addFilter(fileFilter)
      #############################################
    } else if (set.sas==TRUE) {
      #### Get .xport SAS library file dialog
      dialog <- gtkFileChooserDialog(title = "Choose a SAS .xpt file: ",
                                     parent = pls_window, action = "open",
                                     "gtk-ok", GtkResponseType["ok"],
                                     "gtk-cancel", GtkResponseType["cancel"],
                                     show = TRUE)
      # read in file for event signal
      gSignalConnect(dialog, "response", function(dialog, response) {
        if(response == GtkResponseType["ok"]) {
          file.xport <- read.xport(dialog$getFilename())
          df.xport <- file.xport
          current_data_file <<- df.xport
        }
        dialog$destroy()
      })
      
      # filter for .xport data file
      fileFilter <- gtkFileFilter()
      fileFilter$setName("SAS .xpt library file")
      fileFilter$addPattern(c("*.xpt","*.XPT"))
      dialog$addFilter(fileFilter)
      # filter for all files
      #fileFilter <- gtkFileFilter()
      #fileFilter$setName("all files")
      #fileFilter$addPattern("*")
      #dialog$addFilter(fileFilter)
      #############################################
    } 
    
  })
  
  gSignalConnect(medmod_button, "clicked", function(medmod_button) {
    # implement the signal for medmod analysis button
    ## Check for existence of data.
    if (exists("current_data_file")==FALSE) {
      # data warning dialog
      #if (exists("current_data_file")==FALSE) {
      ###############################################################
      ## Files button dialog
      dialog <- gtkMessageDialog(parent=pls_window, 
                                 "destroy-with-parent",
                                 "question", 
                                 "ok-cancel", 
                                 " Input a Data File (and push button again)")
      # choices for radio button
      choices <- c("CSV Data File (.csv)", 
                   "SPSS Data File (.sav)",
                   "Stata Data File (.dta)",
                   "SAS Export File (.xpt)")
      radio_buttons <- NULL
      vbox <- gtkVBox(FALSE, 0)
      for (choice in choices) {
        button <- gtkRadioButton(radio_buttons, choice)
        vbox$add(button)
        radio_buttons <- c(radio_buttons, button)
      }
      
      # set Data file button active
      vbox[[1]]$setActive(TRUE)
      sapply(radio_buttons, '[', "active")
      
      # set up frame
      frame1 <- gtkFrame(" Choose a data file: ")
      frame1$add(vbox)
      dialog[["vbox"]]$add(frame1)
      set.data <- FALSE
      set.spss <- FALSE
      set.stata <- FALSE
      set.sas <- FALSE
      
      if (dialog$run() == GtkResponseType["ok"]) {
        set.data <- vbox[[1]]$getActive()
        set.spss <- vbox[[2]]$getActive()
        set.stata <- vbox[[3]]$getActive()
        set.sas <- vbox[[4]]$getActive()
      }
      dialog$destroy()
      # action to follow based on user choice
      if (set.data==TRUE){
        #### Get .csv data file dialog
        dialog <- gtkFileChooserDialog(title = "Choose a .csv data file: ", 
                                       parent = pls_window, action = "open",
                                       "gtk-ok", GtkResponseType["ok"],
                                       "gtk-cancel", GtkResponseType["cancel"],
                                       show = TRUE)
        # read in file for event signal
        gSignalConnect(dialog, "response", function(dialog, response) {
          if(response == GtkResponseType["ok"]) {
            object <- read.csv(dialog$getFilename(),header=TRUE)
            current_data_file <<- current_data_file <- object
            if (exists("files_dir")) setwd(files_dir)
            write.csv(x=object, file="current_data_file.csv")
            if (exists("main_dir")) setwd(main_dir)
          }
          dialog$destroy()
        })
        
        # filter for .csv
        fileFilter <- gtkFileFilter()
        fileFilter$setName(".csv data files")
        fileFilter$addPattern(c("*.csv","*.CSV"))
        dialog$addFilter(fileFilter)
        # filter for all files
        #fileFilter <- gtkFileFilter()
        #fileFilter$setName("all files")
        #fileFilter$addPattern("*")
        #dialog$addFilter(fileFilter)
        #############################################
      } else if (set.spss==TRUE) {
        #### Get .sav data file dialog
        dialog <- gtkFileChooserDialog(title = "Choose an SPSS .sav data file: ",
                                       parent = pls_window, action = "open",
                                       "gtk-ok", GtkResponseType["ok"],
                                       "gtk-cancel", GtkResponseType["cancel"],
                                       show = TRUE)
        # read in file for event signal
        gSignalConnect(dialog, "response", function(dialog, response) {
          if(response == GtkResponseType["ok"]) {
            file.spss <- read.spss(dialog$getFilename())
            df.spss <- data.frame(file.spss)
            current_data_file <<- current_data_file <- df.spss
          }
          dialog$destroy()
        })
        
        # filter for .sav data file
        fileFilter <- gtkFileFilter()
        fileFilter$setName("SPSS .sav data files")
        fileFilter$addPattern(c("*.sav","*.SAV"))
        dialog$addFilter(fileFilter)
        # filter for all files
        #fileFilter <- gtkFileFilter()
        #fileFilter$setName("all files")
        #fileFilter$addPattern("*")
        #dialog$addFilter(fileFilter)
        #############################################
      } else if (set.stata==TRUE) {
        #### Get .dta data file dialog
        dialog <- gtkFileChooserDialog(title = "Choose a Stata .dta data file: ",
                                       parent = pls_window, action = "open",
                                       "gtk-ok", GtkResponseType["ok"],
                                       "gtk-cancel", GtkResponseType["cancel"],
                                       show = TRUE)
        # read in file for event signal
        gSignalConnect(dialog, "response", function(dialog, response) {
          if(response == GtkResponseType["ok"]) {
            file.dta <- read.dta(dialog$getFilename())
            current_data_file <<- current_data_file <- file.dta
          }
          dialog$destroy()
        })
        
        # filter for .dta data file
        fileFilter <- gtkFileFilter()
        fileFilter$setName("Stata .dta data files")
        fileFilter$addPattern(c("*.dta","*.DTA"))
        dialog$addFilter(fileFilter)
        # filter for all files
        #fileFilter <- gtkFileFilter()
        #fileFilter$setName("all files")
        #fileFilter$addPattern("*")
        #dialog$addFilter(fileFilter)
        #############################################
      } else if (set.sas==TRUE) {
        #### Get .xport SAS library file dialog
        dialog <- gtkFileChooserDialog(title = "Choose a SAS .xpt file: ",
                                       parent = pls_window, action = "open",
                                       "gtk-ok", GtkResponseType["ok"],
                                       "gtk-cancel", GtkResponseType["cancel"],
                                       show = TRUE)
        # read in file for event signal
        gSignalConnect(dialog, "response", function(dialog, response) {
          if(response == GtkResponseType["ok"]) {
            file.xport <- read.xport(dialog$getFilename())
            df.xport <- file.xport
            current_data_file <<- current_data_file <- df.xport
          }
          dialog$destroy()
        })
        
        # filter for .xport data file
        fileFilter <- gtkFileFilter()
        fileFilter$setName("SAS .xpt library file")
        fileFilter$addPattern(c("*.xpt","*.XPT"))
        dialog$addFilter(fileFilter)
        # filter for all files
        #fileFilter <- gtkFileFilter()
        #fileFilter$setName("all files")
        #fileFilter$addPattern("*")
        #dialog$addFilter(fileFilter)
        #############################################
      }
      ##########################################################
    }
    
    if (exists("current_data_file")==TRUE) {
      dialogw <- gtkWindow()
      dialogw$SetTitle("Mediator/Moderator Analysis")
      dialog <- gtkTable(rows=3, columns=3)
      dialogw$add(dialog)
      # Add title
      # dialog["title"] = "Mediator/Moderator Analysis (PROCESS) "
      var_choices0 = colnames(current_data_file)
      var_choices = c("Select a variable...", var_choices0)
      #######################################################
      framed <- gtkFrame("Choose dependent variable: ")
      d_df = rGtkDataFrame(var_choices0)
      y_vars = gtkTreeViewNewWithModel(d_df)
      y_vars$getSelection()$setMode("browse")
      column <- gtkTreeViewColumn("Dependent Variable", gtkCellRendererText(), text = 0)
      y_vars$appendColumn(column)
      scrolled_window <- gtkScrolledWindow()
      #only one is needed unless homogenous is explicitly set to FALSE
      scrolled_window$setSizeRequest(-1,150)
      scrolled_window$add(y_vars)
      framed$add(scrolled_window)
      dialog$Attach(framed, left.attach=0, right.attach=1, top.attach=0, bottom.attach=1)
      ######################################################
      framei <- gtkFrame("Choose independent variable(s): ")
      ind_df = rGtkDataFrame(var_choices0)
      x_vars = gtkTreeViewNewWithModel(ind_df)
      x_vars$getSelection()$setMode("multiple")
      column <- gtkTreeViewColumn("Independent Variable(s)", gtkCellRendererText(), text = 0)
      x_vars$appendColumn(column)
      scrolled_window <- gtkScrolledWindow()
      scrolled_window$add(x_vars)
      framei$add(scrolled_window)
      dialog$Attach(framei, left.attach=1, right.attach=2, top.attach=0, bottom.attach=1)
      #######################################################
      framem <- gtkFrame("Choose mediator(s) or moderator(s): ")
      ind_df = rGtkDataFrame(var_choices0)
      m_vars = gtkTreeViewNewWithModel(ind_df)
      m_vars$getSelection()$setMode("multiple")
      column <- gtkTreeViewColumn("Mediator(s) or Moderator(s)", gtkCellRendererText(), text = 0)
      m_vars$appendColumn(column)
      scrolled_window <- gtkScrolledWindow()
      scrolled_window$add(m_vars)    
      framem$add(scrolled_window)  
      dialog$Attach(framem, left.attach=2, right.attach=3, top.attach=0, bottom.attach=1)
      #######################################################
      framecv <- gtkFrame("Choose covariate(s): ")
      ind_df = rGtkDataFrame(var_choices0)
      cv_vars = gtkTreeViewNewWithModel(ind_df)
      cv_vars$getSelection()$setMode("multiple")
      column <- gtkTreeViewColumn("Covariate(s)", gtkCellRendererText(), text = 0)
      cv_vars$appendColumn(column)
      scrolled_window <- gtkScrolledWindow()
      scrolled_window$add(cv_vars)      
      framecv$add(scrolled_window)
      dialog$Attach(framecv, left.attach=3, right.attach=4, top.attach=0, bottom.attach=1)  
      ######################################################
      combow <- gtkComboBoxNewText()
      combow$show()
      for (choice in var_choices) combow$appendText(choice)
      combow$setActive(0)
      framew <- gtkFrame("Choose a moderator W variable: ")
      framew$add(combow)
      dialog$Attach(framew, left.attach=0, right.attach=1, top.attach=1, bottom.attach=2)    
      #######################################################
      comboz <- gtkComboBoxNewText()
      comboz$show()
      for (choice in var_choices) comboz$appendText(choice)
      comboz$setActive(0)
      framez <- gtkFrame("Choose a moderator Z variable: ")
      framez$add(comboz)
      dialog$Attach(framez, left.attach=1, right.attach=2, top.attach=1, bottom.attach=2)    
      #######################################################
      combov <- gtkComboBoxNewText()
      combov$show()
      for (choice in var_choices) combov$appendText(choice)
      combov$setActive(0)
      framev <- gtkFrame("Choose a moderator V variable: ")
      framev$add(combov)
      dialog$Attach(framev, left.attach=2, right.attach=3, top.attach=1, bottom.attach=2)    
      #######################################################
      comboq <- gtkComboBoxNewText()
      comboq$show()
      for (choice in var_choices) comboq$appendText(choice)
      comboq$setActive(0)
      frameq <- gtkFrame("Choose a moderator Q variable ")
      frameq$add(comboq)
      dialog$Attach(frameq, left.attach=3, right.attach=4, top.attach=1, bottom.attach=2)
      #######################################################
      combocl <- gtkComboBoxNewText()
      combocl$show()
      for (choice in var_choices) combocl$appendText(choice)
      combocl$setActive(0)
      framecl <- gtkFrame("Choose a cluster variable")
      framecl$add(combocl)
      dialog$Attach(framecl, left.attach=0, right.attach=1, top.attach=2, bottom.attach=3)    
      #######################################
      # create slider for model number
      sliderm <-gtkHScale(min=1, max=77, step=1)
      sliderm['draw-value'] <- TRUE
      framem <- gtkFrame(" Specify model number: ")
      framem$add(sliderm)
      dialog$Attach(framem, left.attach=1, right.attach=2, top.attach=2, bottom.attach=3)
      
      ###################################################
      jumboframe <- gtkFrame()
      jumbovbox <- gtkVBoxNew()
      jumboframe$add(jumbovbox)
      
      combobc <- gtkComboBoxNewText()
      combobc$show()
      choices = c("Use bias-corrected confidence intervals", "Use percentile confidence intervals", "Use monte carlo confidence intervals")
      for (choice in choices) combobc$appendText(choice)
      combobc$setActive(0)
      framebc <- gtkFrame("Select a bootstrap method:")
      framebc$add(combobc)
      jumbovbox$add(framebc)
      
      combocvar <- gtkComboBoxNewText()
      combocvar$show()
      choices = c("Both M and Y", "M only", "Y only")
      for (choice in choices) combocvar$appendText(choice)
      combocvar$setActive(0)
      framecvar <- gtkFrame("Select M and Y vars as covariates:")
      framecvar$add(combocvar)
      jumbovbox$add(framecvar)
      
      dialog$Attach(jumboframe, left.attach=1, right.attach=2, top.attach=3, bottom.attach=4)
      ###################################################
      framespec <- gtkFrame("Test specific values for conditional effects")
      vspecbox = gtkVBoxNew()
      framespec$add(vspecbox)
      wbox <- gtkHBoxNew()
      vspecbox$add(wbox)
      wspec <- gtkEntryNew()
      labelw = gtkLabelNewWithMnemonic("_W")
      wbox$add(labelw)
      labelw$setMnemonicWidget(wspec)
      wbox$add(wspec)
      
      vbox <- gtkHBoxNew()
      vspecbox$add(vbox)
      vspec <- gtkEntryNew()
      labelv = gtkLabelNewWithMnemonic("_V")
      vbox$add(labelv)
      labelv$setMnemonicWidget(vspec)
      vbox$add(vspec)
      
      qbox <- gtkHBoxNew()
      vspecbox$add(qbox)
      qspec <- gtkEntryNew()
      labelq = gtkLabelNewWithMnemonic("_Q")
      qbox$add(labelq)
      labelq$setMnemonicWidget(qspec)
      qbox$add(qspec)
      
      xbox <- gtkHBoxNew()
      vspecbox$add(xbox)
      xspec <- gtkEntryNew()
      labelx = gtkLabelNewWithMnemonic("_X")
      xbox$add(labelx)
      labelx$setMnemonicWidget(xspec)
      xbox$add(xspec)
      
      mbox <- gtkHBoxNew()
      vspecbox$add(mbox)
      mspec <- gtkEntryNew()
      labelm = gtkLabelNewWithMnemonic("_M")
      mbox$add(labelm)
      labelm$setMnemonicWidget(mspec)
      mbox$add(mspec)
      
      zbox <- gtkHBoxNew()
      vspecbox$add(zbox)
      zspec <- gtkEntryNew()
      labelz = gtkLabelNewWithMnemonic("_Z")
      zbox$add(labelz)
      labelz$setMnemonicWidget(zspec)
      zbox$add(zspec)
      
      dialog$Attach(framespec, left.attach=0, right.attach=1, top.attach=3, bottom.attach=4)   
      #######################################
      frameo <- gtkFrame("Select options:")
      boxo <- gtkVBox(TRUE, 5)
      frameo$add(boxo)
      o1 = gtkCheckButton("Mean center all interaction variables")
      boxo$add(o1)
      o2 = gtkCheckButton("Use hetereoscedasticity consistent SEs")
      boxo$add(o2)
      o3 = gtkCheckButton("Supress printing OLS/ML confidence intervals")
      boxo$add(o3)
      o4 = gtkCheckButton("Generate data for plotting (models 1,2,3 only)")
      boxo$add(o4)
      o5 = gtkCheckButton("Estimate indirect effect sizes (models 4 and 6 only)")
      boxo$add(o5)
      o6 = gtkCheckButton("Run sobel test (model 4 only)")
      boxo$add(o6)
      o7 = gtkCheckButton("Show total effects (model 4 and 6 only)")
      boxo$add(o7)
      o8 = gtkCheckButton("Test differences in indirect effects (models 4 and 6 only)")
      boxo$add(o8)
      o9 = gtkCheckButton("Show model coefficient covariance matrix")
      boxo$add(o9)
      o10 = gtkCheckButton("Use Johnson-Neyman Technique (Models 1 and 3 only)")
      boxo$add(o10)
      o11 = gtkCheckButton("Probe interactions with percentiles instead of mean +/- SD")
      boxo$add(o11)
      o12 = gtkCheckButton("Suppress detailed model information")
      boxo$add(o12)
      savesob <- gtkCheckButton("Use 1st order instead of 2nd order SE estimators (Sobel)")
      boxo$add(savesob)
      dialog$Attach(frameo, left.attach=2, right.attach=4, top.attach=2, bottom.attach=6)
      #######################################
      # create slider 
      slidercc <-gtkHScale(min=80, max=99, step=1)
      slidercc['draw-value'] <- TRUE
      gtkRangeSetValue(object=slidercc, value=95)
      framesa <- gtkFrame(" Specify desired confidence interval: ")
      framesa$add(slidercc)
      dialog$Attach(framesa, left.attach=0, right.attach=1, top.attach=4, bottom.attach=5)   
      
      #######################################
      # create slider 
      sliderss <-gtkHScale(min=0, max=50000, step=1000)
      sliderss['draw-value'] <- TRUE
      gtkRangeSetValue(object=sliderss, value=5000)
      framesb <- gtkFrame(" Specify sample size for bootstrap (0 for none): ")
      framesb$add(sliderss)
      dialog$Attach(framesb, left.attach=1, right.attach=2, top.attach=4, bottom.attach=5)
      
      ##############################################
      saverep <- gtkCheckButton("Yes")
      saverep['active'] <- FALSE
      framerep <- gtkFrame(" Save final report to reports directory?")
      framerep$add(saverep)
      dialog$Attach(framerep, left.attach=0, right.attach=2, top.attach=5, bottom.attach=6)
      
      ########################################
      saveout <- gtkCheckButton("Yes")
      saveout['active'] <- FALSE
      frameout <- gtkFrame(" Save bootstrap results to files directory? (will override existing file)")
      frameout$add(saveout)
      dialog$Attach(frameout, left.attach=0, right.attach=2, top.attach=6, bottom.attach=7)
      
      #######################################
      #frame for progress bar for iterations
      buttonCancel = gtkButtonNewFromStock("gtk-close")
      gSignalConnect(buttonCancel, "clicked", dialogw$destroy)
      dialog$Attach(buttonCancel, left.attach=3, right.attach=4, top.attach=6, bottom.attach=7)
      buttonOK = gtkButtonNewFromStock("gtk-ok")
      dialog$Attach(buttonOK, left.attach=2, right.attach=3, top.attach=6, bottom.attach=7)
      gSignalConnect(buttonOK, "clicked", function(buttonOK) {
        ###process variables###
        model <- sliderm$getValue()
        yname=''
        if (length(y_vars$getSelection()$getSelectedRows()$retval) > 0) {
          yname = var_choices0[y_vars$getSelection()$getSelectedRows()$retval[[1]]$getIndices()[[1]] + 1]
        }
        num_meds = length(m_vars$getSelection()$getSelectedRows()$retval)
        mnames = vector()
        if (num_meds != 0) {
          for (i in 1:num_meds) {
            mnames = c(mnames, var_choices0[m_vars$getSelection()$getSelectedRows()$retval[[i]]$getIndices()[[1]] + 1])
          }
        }
        num_x = length(x_vars$getSelection()$getSelectedRows()$retval)
        xnames = vector()
        if (num_x != 0) {
          for (i in 1:num_x) {
            xnames = c(xnames, var_choices0[x_vars$getSelection()$getSelectedRows()$retval[[i]]$getIndices()[[1]] + 1])
          }
        }
        num_covar = length(cv_vars$getSelection()$getSelectedRows()$retval)
        covars = vector()
        if (num_covar != 0) {
          for (i in 1:num_covar) {
            covars = c(covars, var_choices0[cv_vars$getSelection()$getSelectedRows()$retval[[i]]$getIndices()[[1]] + 1])
          }
        }
        var_choices[1] = 'XXX'
        wname <- var_choices[combow['active']+1]
        zname <- var_choices[comboz['active']+1]
        vname <- var_choices[combov['active']+1]
        qname <- var_choices[comboq['active']+1]
        cluster <- var_choices[combocl['active']+1]
        
        covmy = combocvar['active']
        #create vars list without x for now
        vars = union(yname,mnames)
        vars = union(vars, covars)
        vars = union(vars, wname)
        vars = union(vars, zname)
        vars = union(vars, vname)
        vars = union(vars, qname)
        vars = union(vars, cluster)
        boot.ss <- sliderss$getValue()
        conf <- slidercc$getValue()
        #starts at 0
        mc <- 0
        percent <- 0
        if (combobc['active'] == 1) {
          percent <- 1
        } else if (combobc['active'] == 2) {
          mc <- boot.ss
        }
        saveboot = ifelse(saveout['active'] == TRUE, 1, 'xxx')
        
        center <- o1['active']
        hc3 <- o2['active']
        coeffci <- ifelse(o3['active'], 0, 1)
        plot1 <- o4['active']
        effsize <- o5['active']
        normal <- o6['active']
        total <- o7['active']
        contrast <- o8['active']
        covcoeff <- o9['active']
        jn <- o10['active']
        quantile1 <- o11['active']
        detail <- ifelse(o12['active'], 0, 1)
        
        wmodval = ifelse(wspec$GetText() == '', 999, as.numeric(wspec$GetText()))
        zmodval = ifelse(zspec$GetText() == '', 999, as.numeric(zspec$GetText()))
        vmodval = ifelse(vspec$GetText() == '', 999, as.numeric(vspec$GetText()))
        qmodval = ifelse(qspec$GetText() == '', 999, as.numeric(qspec$GetText()))
        xmodval = ifelse(xspec$GetText() == '', 999, as.numeric(xspec$GetText()))
        mmodval = ifelse(mspec$GetText() == '', 999, as.numeric(mspec$GetText()))
        varorder= ifelse(savesob['active'] == TRUE, 1, 2)
        
        for (i in ifelse(length(xnames) == 0,0,1):length(xnames)) {
          ################loop through each X########################
          if (i == 0) {
            xname = ''
          } else {
            xname = xnames[i]
          }
          vars2 = union(vars, xname)
          vars2 = vars2[vars2 != ""]
          vars2 = vars2[vars2 != "XXX"]
          if(saverep['active'] == TRUE ) {
            output = paste('medmod_results',model,yname,xname,paste(mnames,sep="_",collapse="_"),'.csv',sep='_')
          } else {
            output = NULL
          }
          process(current_data_file, vars=vars2,model=model,yname=yname,mnames=mnames,xname=xname,
                  wname=wname,zname=zname,vname=vname,qname=qname,conf=conf,
                  hc3=hc3,boot=boot.ss,center=center,quantile=quantile1,effsize=effsize,
                  normal=normal,total=total,detail=detail,percent=percent,jn=jn,coeffci=coeffci,
                  contrast=contrast,saveboot=saveboot,mc=mc,covcoeff=covcoeff,cluster=cluster,
                  plot=plot1,wmodval=wmodval,zmodval=zmodval,vmodval=vmodval,qmodval=qmodval,
                  xmodval=xmodval,mmodval=mmodval,covmy=covmy,varorder=varorder, medmod_file=output
          )
        }
      })
    }
  })
  
  
  gSignalConnect(mi_button, "clicked", function(mi_button) {
    ####################################################
    # implement the clicked signal for Missing Data Imputation toolbar item  
    ## Missing Data Imputation button dialog
    VIMGUI()
  })
  
  
  gSignalConnect(everything_button, "clicked", function(everything_button) {
    # implement the signal for Everything Else toolbar item
    # Check for Data, then for a Model, then run Rcmdr dialog
    # Run Rcmdr script
    myDir <- .packages(all.available=TRUE)
    if ("Rcmdr" %in% myDir) unloadNamespace("Rcmdr")
    library("Rcmdr")
  })
  
  
  gSignalConnect(dm_button, "clicked", function(dm_button) {
    ####################################################
    # implement the clicked signal for Missing Data Imputation toolbar item
    ## Missing Data Imputation button dialog
    require(rattle)
    rattle()
  })
  
  # add menu bar to top of window
  vbox$packStart(toolbar, FALSE, FALSE, 0)
  vbox$packStart(notebook, TRUE, TRUE, 0)
  
  # show top-level pls window
  pls_window$show()
  
}

gtkNotebookInsertPageWithCloseButton <- 
  function(object, child, label.text="", position=-1) {
    # function to create a notebook page with close button
    icon <- gtkImage(pixbuf = 
                       object$renderIcon("gtk-close", "button", size = "menu"))
    closeButton <- gtkButton()
    closeButton$setImage(icon)
    closeButton$setRelief("none")
    ##
    label <- gtkHBox()
    label$packStart(gtkLabel(label.text))
    label$packEnd(closeButton)
    ##
    gSignalConnect(closeButton, "clicked", function(button) {
      index <- object$pageNum(child)
      object$removePage(index)
    })
    object$insertPage(child, label, position)
  }

write_output <- function(object, col.names=FALSE, row.names=FALSE, output=NULL){
  if (identical(col.names, FALSE)) {
    hv = FALSE
  } else {
    hv = TRUE
  }
  if (is.matrix(object)) {
    df.object = as.data.frame(object)
    rownames(df.object) <- NULL
    if (!identical(row.names, FALSE)) {
      df.object=cbind(row.names,df.object)
      if (!identical(col.names, FALSE)) {
        col.names = c('',col.names)
      } else {
        col.names = c('','')            
      }
    }
    if (!identical(col.names, FALSE)) {
      colnames(df.object) <- col.names
    } else {
      hdrs = rep("", length.out=ncol(df.object))
      colnames(df.object) <- hdrs
    }
    #df.object = rbind(df.object, rep('',length.out=ncol(df.object)))
    model <- rGtkDataFrame(df.object)
    view <- gtkTreeView(model)
    gtkTreeViewSetHeadersVisible(view, headers.visible=hv)
    mapply(view$insertColumnWithAttributes,  
           position = -1, 
           title = colnames(model), 
           cell = list(gtkCellRendererText()), 
           text = seq_len(ncol(model)) - 1
    )
    # add dataframe
    medmod_box$add(view)
    if (!is.null(output)) write.table(df.object, file=output, sep=",", append=TRUE, quote=FALSE, row.names=FALSE, col.names=TRUE)
  } else {
    textview <- gtkTextView()
    medmod_box$add(textview)
    buffer <- textview$getBuffer()
    buffer$SetText(paste("\n", object))
    if (!is.null(output)) write.table(matrix(object, dimnames=list(' ',' ')), file=output, sep=",", append=TRUE, quote=FALSE, row.names=FALSE, col.names=TRUE)
  }
}

process <- function(dat,vars,model=77,yname='',mnames,xname,wname='XXX',zname='XXX',vname='XXX',qname='XXX',conf=95,
                    hc3=0,cluster='XXX',wmodval=999,zmodval=999,vmodval=999,qmodval=999,mmodval=999,
                    xmodval=999,boot=1000,center=0,quantile=0,effsize=0,normal=0,varorder=2,total=0,
                    plot=0,detail=1,iterate=10000,converge=0.00000001,percent=0,jn=0,coeffci=1,
                    covmy=0,contrast=0,seed=0,saveboot='xxx',mc=0,decimals=4,covcoeff=0,medmod_file=NULL) {
  #vars - string vector of variable names from data set to be used in the model
  #mnames is a vector of strings
  #yname,xname,w,z,v,q - strings with one variable
  #original default value of decimals=10.4
  
  vnames=matrix(vars,nrow=1)
  dat <- as.matrix(dat[,vnames])
  #convert mnames into matrix
  if (!missing(mnames)) {
    mnames = matrix(mnames, nrow=1)
  }
  
  # qname=toupper(qname)
  clname = cluster
  # clname=toupper(clname)
  
  # setup window
  medmod_window <- gtkScrolledWindow()
  medmod_box <<- gtkVBox()
  gtkScrolledWindowAddWithViewport(medmod_window, medmod_box)
  win.title = paste("Med/Mod Analysis - X: ",xname, ", Model: ", model)
  notebook$insertPageWithCloseButton(medmod_window, win.title)     
  
  ninit=nrow(dat)
  #remove incomplete records
  dat <- dat[complete.cases(dat), , drop=F]
  n=nrow(dat)
  p0=-.322232431088
  p1 = -1
  p2 = -.342242088547
  p3 = -.0204231210245
  p4 = -.0000453642210148
  q0 = .0993484626060
  q1 = .588581570495
  q2 = .531103462366
  q3 = .103537752850
  q4 = .0038560700634
  badend=0
  priorlo=-9999999
  priorhi=9999999
  criterr=0
  cluster=0
  clsdmy=0
  jndich=0
  booterr=0
  wvdich=0
  mod74dic=0
  effsize=(effsize==1);
  covcoeff=(covcoeff==1);
  note=matrix(0,nrow=10,ncol=1)
  notes=1;
  iterr=0;
  clsmtch=0;
  quantile=(quantile==1);
  jn=(jn==1);
  contrast=(contrast==1);
  center=(center==1);
  detail=(detail==1);
  coeffci=(coeffci==1);
  conf=(conf);
  bconoff=(percent != 1);
  covmy=floor(covmy);
  
  if (covmy < 0 || covmy > 2) covmy=0
  
  if ((floor(conf) >= 100) || (floor(conf) <= 50)) {
    conf=95;
    note[notes,1]=1;
    notes=notes+1;
  }
  if (n < ninit) {
    nmiss=ninit-n;
    note[notes,1]=11;
    notes=notes+1;
  }
  errs=0;
  quantd=matrix(0,1,6)
  quantc=matrix(0,1,6)
  mcheck=0;
  ttt=0;
  plot=(plot != 0);
  runerrs=matrix(0,50,1)
  model=floor(model);
  if ((jn == 1) && (model != 1) && (model != 3)) {
    note[notes,1]=7;
    notes=notes+1;
  }
  if ((model > 76) || (model < 1)) {
    model=77;
    criterr=1;
    errs=errs+1;
    runerrs[errs,1]=19;
  }
  toteff=0;
  toteff=((total==1)*((model==4) || (model==6)));
  normal=(normal);
  varorder=(varorder);
  hc3=(hc3 != 0);
  centvar=c('xxx')
  modelm <- matrix(c(0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,1,
                     0,0,0,0,0,0,1,0,0,0,0,0,0,0,0,0,0,0,2,
                     0,0,0,0,0,0,1,0,0,0,0,0,0,0,0,0,0,0,3,
                     0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,4,
                     0,0,0,0,0,0,1,0,0,0,0,0,0,0,0,0,0,0,5,
                     0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,6,
                     1,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,7,
                     1,0,0,0,0,0,1,0,0,0,0,0,0,0,0,0,0,0,8,
                     1,1,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,9,
                     1,1,0,0,0,0,1,1,0,0,0,0,0,0,0,0,0,0,10,
                     1,1,1,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,11,
                     1,1,1,0,0,0,1,1,1,0,0,0,0,0,0,0,0,0,12,
                     1,1,1,0,0,0,1,0,0,0,0,0,0,0,0,0,0,0,13,
                     0,0,0,1,0,0,0,0,0,0,0,0,0,0,0,0,0,0,14,
                     0,0,0,1,0,0,0,0,0,1,0,0,0,0,0,0,0,0,15,
                     0,0,0,1,1,0,0,0,0,0,0,0,0,0,0,0,0,0,16,
                     0,0,0,1,1,0,0,0,0,1,1,0,0,0,0,0,0,0,17,
                     0,0,0,1,1,1,0,0,0,0,0,0,0,0,0,0,0,0,18,
                     0,0,0,1,1,1,0,0,0,1,1,1,0,0,0,0,0,0,19,
                     0,0,0,1,1,1,0,0,0,1,0,0,0,0,0,0,0,0,20,
                     1,0,0,1,0,0,0,0,0,0,0,0,0,0,0,0,0,0,21,
                     1,0,0,1,0,0,1,0,0,0,0,0,0,0,0,0,0,0,22,
                     1,1,0,1,0,0,0,0,0,0,0,0,0,0,0,0,0,0,23,
                     1,1,0,1,0,0,1,1,0,0,0,0,0,0,0,0,0,0,24,
                     1,1,1,1,0,0,0,0,0,0,0,0,0,0,0,0,0,0,25,
                     1,1,1,1,0,0,1,1,1,0,0,0,0,0,0,0,0,0,26,
                     1,1,1,1,0,0,1,0,0,0,0,0,0,0,0,0,0,0,27,
                     1,0,0,1,0,0,0,0,0,1,0,0,0,0,0,0,0,0,28,
                     1,0,0,1,0,0,1,0,0,1,0,0,0,0,0,0,0,0,29,
                     1,1,0,1,0,0,0,0,0,1,0,0,0,0,0,0,0,0,30,
                     1,1,0,1,0,0,1,1,0,1,0,0,0,0,0,0,0,0,31,
                     1,1,1,1,0,0,0,0,0,1,0,0,0,0,0,0,0,0,32,
                     1,1,1,1,0,0,1,1,1,1,0,0,0,0,0,0,0,0,33,
                     1,1,1,1,0,0,1,0,0,1,0,0,0,0,0,0,0,0,34,
                     1,0,0,1,1,0,0,0,0,0,0,0,0,0,0,0,0,0,35,
                     1,0,0,1,1,0,0,0,0,1,1,0,0,0,0,0,0,0,36,
                     1,0,0,1,1,1,0,0,0,0,0,0,0,0,0,0,0,0,37,
                     1,0,0,1,1,1,0,0,0,1,1,1,0,0,0,0,0,0,38,
                     1,0,0,1,1,1,0,0,0,1,0,0,0,0,0,0,0,0,39,
                     1,0,0,1,1,0,1,0,0,0,0,0,0,0,0,0,0,0,40,
                     1,0,0,1,1,0,1,0,0,1,1,0,0,0,0,0,0,0,41,
                     1,0,0,1,1,1,1,0,0,0,0,0,0,0,0,0,0,0,42,
                     1,0,0,1,1,1,1,0,0,1,1,1,0,0,0,0,0,0,43,
                     1,0,0,1,1,1,1,0,0,1,0,0,0,0,0,0,0,0,44,
                     1,1,0,1,1,0,0,0,0,0,0,0,0,0,0,0,0,0,45,
                     1,1,1,1,1,0,0,0,0,0,0,0,0,0,0,0,0,0,46,
                     1,1,0,1,1,1,0,0,0,0,0,0,0,0,0,0,0,0,47,
                     1,1,1,1,1,1,0,0,0,0,0,0,0,0,0,0,0,0,48,
                     1,1,0,1,1,0,1,1,0,0,0,0,0,0,0,0,0,0,49,
                     1,1,0,1,1,0,0,0,0,1,1,0,0,0,0,0,0,0,50,
                     1,1,1,1,1,0,1,1,1,0,0,0,0,0,0,0,0,0,51,
                     1,1,1,1,1,0,0,0,0,1,1,0,0,0,0,0,0,0,52,
                     1,1,0,1,1,1,1,1,0,0,0,0,0,0,0,0,0,0,53,
                     1,1,0,1,1,1,0,0,0,1,1,1,0,0,0,0,0,0,54,
                     1,1,1,1,1,1,1,1,1,0,0,0,0,0,0,0,0,0,55,
                     1,1,1,1,1,1,0,0,0,1,1,1,0,0,0,0,0,0,56,
                     1,1,1,1,1,1,1,1,1,1,1,1,0,0,0,0,0,0,57,
                     1,0,0,0,0,0,0,0,0,0,0,0,1,0,0,0,0,0,58,
                     1,0,0,0,0,0,1,0,0,0,0,0,1,0,0,0,0,0,59,
                     1,1,0,0,0,0,0,0,0,0,0,0,1,0,0,0,0,0,60,
                     1,1,0,0,0,0,1,0,0,0,0,0,1,0,0,0,0,0,61,
                     1,1,0,0,0,0,0,1,0,0,0,0,1,0,0,0,0,0,62,
                     1,1,0,0,0,0,1,1,0,0,0,0,1,0,0,0,0,0,63,
                     1,0,0,1,0,0,0,0,0,0,0,0,1,0,0,0,0,0,64,
                     1,0,0,1,0,0,1,0,0,0,0,0,1,0,0,0,0,0,65,
                     1,0,0,1,0,0,0,0,0,1,0,0,1,0,0,0,0,0,66,
                     1,0,0,1,0,0,1,0,0,1,0,0,1,0,0,0,0,0,67,
                     1,1,1,0,0,0,0,0,0,0,0,0,1,0,0,0,0,0,68,
                     1,1,1,0,0,0,1,1,1,0,0,0,1,0,0,0,0,0,69,
                     1,0,0,1,0,0,0,0,0,0,0,0,1,1,0,0,0,0,70,
                     1,0,0,1,0,0,1,0,0,1,0,0,1,1,1,0,0,0,71,
                     1,1,1,0,0,0,0,0,0,0,0,0,1,0,0,1,1,0,72,
                     1,1,1,0,0,0,1,1,1,0,0,0,1,0,0,1,1,0,73,
                     0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,1,74,
                     1,1,0,0,0,0,0,0,0,0,0,0,1,0,0,1,0,0,75,
                     1,1,0,0,0,0,1,1,0,0,0,0,1,0,0,1,0,0,76,
                     0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,77),ncol=19, nrow=77, byrow=TRUE)
  
  wm=modelm[model,1, drop=F];
  zm=modelm[model,2, drop=F];
  wzm=modelm[model,3, drop=F];
  vy=modelm[model,4, drop=F];
  qy=modelm[model,5, drop=F];
  vqy=modelm[model,6, drop=F];
  wy=modelm[model,7, drop=F];
  zy=modelm[model,8, drop=F];
  wzy=modelm[model,9, drop=F];
  vxy=modelm[model,10, drop=F];
  qxy=modelm[model,11, drop=F];
  vqxy=modelm[model,12, drop=F];
  wmy=modelm[model,13, drop=F];
  wvmy=modelm[model,14, drop=F];
  wvxy=modelm[model,15, drop=F];
  zmy=modelm[model,16, drop=F];
  wzmy=modelm[model,17, drop=F];
  xmy=modelm[model,18, drop=F];
  
  #start of error checking
  if (missing(xname)) {
    errs=errs+1
    runerrs[errs,1]=20
    criterr=1
  }
  if (missing(yname)) {
    errs=errs+1;
    runerrs[errs,1]=21;
    criterr=1;
  }
  xlist = ifelse((wm==1)||(zm==1)||(wzm==1)||(wy==1)||(zy==1)||(wzy==1)||(vxy==1)||(qxy==1)||(vqxy==1)||(wvxy==1)||(xmy==1),1,0)
  mlist = ifelse(((vy==1)||(qy==1)||(vqy==1)||(zmy==1)||(wmy==1)||(wzmy==1)||(xmy==1)||(model < 4)),1,0)
  bad=0;
  if (xname %in% mnames) {
    errs=errs+1;
    runerrs[errs,1]=29;
    criterr=1;
  }
  if (yname %in% mnames) {
    errs=errs+1;
    runerrs[errs,1]=30;
    criterr=1;
  }    
  if (criterr==0) {
    werr=0
    verr=0
    qerr=0
    zerr=0
    yerr=1
    xerr=1
    wlist=if((wm==1)||(wzm==1)||(wy==1)||(wzy==1)||(wm==1)||(wvmy==1)||(wvmy==1)||(wvxy==1)||(wzmy==1)) 1 else 0
    if ((wlist==1) && (wname == "XXX")) {
      werr=1
      wlist=0
      errs=errs+1
      runerrs[errs,1]=4
    }
    if ((wlist==1) && ((wname==qname)||(wname==vname)||(wname==zname)||(wname==xname)||(wname==yname))) {
      werr=4
      errs=errs+1
      runerrs[errs,1]=12
    }
    zlist=if((zm==1)||(wzm==1)||(zy==1)||(wzy==1)||(zmy==1)||(wzmy==1)) 1 else 0
    if ((zlist==1) && (zname == "XXX")) {
      zerr=1
      zlist=0
      errs=errs+1
      runerrs[errs,1]=5
    }
    if ((zlist==1) && ((zname==qname)||(zname==vname)||(zname==wname)||(zname==xname)||(zname==yname))) {
      zerr=4
      errs=errs+1
      runerrs[errs,1]=13
    }
    qlist=if((qy==1)||(vqy==1)||(qxy==1)||(vqxy==1)) 1 else 0
    if ((qlist==1) && (qname == "XXX")) {
      qerr=1
      qlist=0
      errs=errs+1
      runerrs[errs,1]=6
    }
    if ((qlist==1) && ((qname==zname)||(qname==vname)||(qname==wname)||(qname==xname)||(qname==yname))) {
      qerr=4
      errs=errs+1
      runerrs[errs,1]=14
    }
    vlist=if((vy==1)||(vqy==1)||(vxy==1)||(vqxy==1)||(wvmy==1)||(wvxy==1)) 1 else 0
    if ((vlist==1) && (vname == "XXX")) {
      verr=1
      vlist=0
      errs=errs+1
      runerrs[errs,1]=7
    }
    if ((vlist==1) && ((vname==zname)||(vname==qname)||(vname==wname)||(vname==xname)||(vname==yname))) {
      qerr=4
      errs=errs+1
      runerrs[err,1]=15
    }
    if ((wlist==0) && (wname != "XXX")) {
      werr=2
      errs=errs+1
      runerrs[errs,1]=8
    }
    if ((zlist==0) && (zname != "XXX")) {
      zerr=2
      errs=errs+1
      runerrs[errs,1]=9
    }
    if ((qlist==0) && (qname != "XXX")) {
      qerr=2
      errs=errs+1
      runerrs[errs,1]=10
    }
    if ((vlist==0) && (vname != "XXX")) {
      verr=2
      errs=errs+1
      runerrs[errs,1]=11
    }
    if (hc3==1) {
      note[notes,1]=3
      notes=notes+1
    }
    
    alpha2=(1-(conf/100))/2;
    y5=sqrt(-2*log(alpha2));
    xp2=-(y5+((((y5*p4+p3)*y5+p2)*y5+p1)*y5+p0)/((((y5*q4+q3)*y5+q2)*y5+q1)*y5+q0));
    cons=matrix(1,n,1)
    #check for constant value variables
    for(i in 1:ncol(dat)) {
      if (length(unique(dat[,i, drop=F])) == 1) {
        criterr=1
        errs=errs+1
        runerrs[errs,1]=27
        break()
      }
    }
    nmeds=ncol(mnames);
    mcmats=as.matrix(diag(nmeds*2));
    mccoeff=matrix(0,(nmeds*2),1);
    sobel=matrix(-999,nmeds,4);
    if ((model == 6) && (nmeds > 4)) {
      errs=errs+1
      runerrs[errs,1]=2;
    }
    if ((model < 4) && (nmeds > 1)) {
      errs=errs+1;
      runerrs[errs,1]=3;
    }
    #end of error checking
    nmods=(model==74);
    bad=0
    intcnt=1
    modvals=0
    modvalsd=0
    #declare matrixes
    yintemp=matrix(
      c("INT_1","INT_2","INT_3","INT_4","INT_5","INT_6","INT_7","INT_8","INT_9","INT_10","INT_11","INT_12",
        "INT_13","INT_14","INT_15","INT_16","INT_17","INT_18","INT_19","INT_20","INT_21","INT_22",
        "INT_23","INT_24","INT_25","INT_26","INT_27","INT_28","INT_29","INT_30","INT_31","INT_32",
        "INT_33","INT_34","INT_35","INT_36","INT_37","INT_38","INT_39","INT_40","INT_41","INT_42",
        "INT_43","INT_44","INT_45","INT_46","INT_47","INT_48","INT_49","INT_50","INT_51","INT_52",
        "INT_53","INT_54","INT_55","INT_56","INT_57","INT_58","INT_59","INT_60","INT_61","INT_62"), nrow=1)
    cntname=matrix(c("(C1)","(C2)","(C3)","(C4)","(C5)","(C6)","(C7)","(C8)","(C9)","(C10)",
                     "(C11)","(C12)","(C13)","(C14)","(C15)","(C16)","(C17)","(C18)","(C19)","(C20)",
                     "(C21)","(C22)","(C23)","(C24)","(C25)","(C26)","(C27)","(C28)","(C29)","(C30)",
                     "(C31)","(C32)","(C33)","(C34)","(C35)","(C36)","(C37)","(C38)","(C39)","(C40)",
                     "(C41)","(C42)","(C43)","(C44)","(C45)","(C46)","(C47)","(C48)","(C49)","(C50)",
                     "(C51)","(C52)","(C53)","(C54)","(C55)","(C56)","(C57)","(C58)","(C59)","(C60)",
                     "(C61)","(C62)","(C63)","(C64)","(C65)","(C66)","(C67)","(C68)","(C69)","(C70)",
                     "(C71)","(C72)","(C73)","(C74)","(C75)","(C76)","(C77)","(C78)","(C79)","(C80)",
                     "(C81)","(C82)","(C83)","(C84)","(C85)","(C86)","(C87)","(C88)","(C89)","(C90)",
                     "(C91)","(C92)","(C93)","(C94)","(C95)","(C96)","(C97)","(C98)","(C99)","(C100)",
                     "(C101)","(C102)","(C103)","(C104)","(C105)"), ncol=1)
    
    modvnm = matrix(c("xxxxxxxxxxxxxxxx","xxx","xxx","xxx","xxx"), nrow=1)
    modvnm2 = modvnm;
    mlab=matrix(c("M1    =","M2    =","M3    =","M4    =","M5    =","M6    =","M7    =","M8    =","M9    =","M10   ="),ncol=1)
    m=matrix(1,n,nmeds)
    mmat=matrix(0,16,nmeds)
    ymat=matrix(0,8,nmeds)
    deco=matrix(0,10,1)
    modmat=matrix(999,5,5)
    modmatv=matrix(1,1,5)
    modmatp=matrix(0,1,5)
    modprod=modmatv;
    iterate=abs(floor(iterate))
    converge=abs(converge)
    boot=abs(floor(boot))
    adjust=0
    mc=abs(floor(mc))
    if ((mc > 0) && (model > 5))  {
      if (boot == 0) boot=mc
      mc=0;
      note[notes,1]=12;
      notes=notes+1;
    }
    if (boot != 0)  {
      cilow=floor(boot*(1-(conf/100))/2)
      cihigh=floor((boot*(conf/100)+(boot*(1-(conf/100))/2)))+1;
      repeat {
        cilow=floor(boot*(1-(conf/100))/2)
        cihigh=floor((boot*(conf/100)+(boot*(1-(conf/100))/2)))+1
        if ((cilow < 1) || (cihigh > boot))  {
          boot=floor((boot+1000)/1000)*1000
          adjust=1
        }
        if ((cilow > 0) && (cihigh <= boot)) break()
      }
      if (adjust == 1)  {
        note[notes,1]=6
        notes=notes+1
      }
    }
    if ((mc > 0) && ((model > 3) && (model < 6)))  {
      boot=0
      bconoff=0
    }
    if ((boot > 0) && (mc > 0))  mc=0
    savboot=0
    if ((saveboot != "xxx") && (boot > 0) && (model > 3)) savboot=1
    if ((boot != 0) || (mc != 0))  {
      bootsz=boot
      if (mc > 0) bootsz=mc
      cilow=round(bootsz*(1-(conf/100))/2)
      cihigh=trunc((bootsz*(conf/100)+(bootsz*(1-(conf/100))/2)))+1
      repeat {
        cilow=floor(bootsz*(1-(conf/100))/2)
        cihigh=floor((bootsz*(conf/100)+(bootsz*(1-(conf/100))/2)))+1;
        if ((cilow < 1) || (cihigh > bootsz))  {
          bootsz=floor((bootsz+1000)/1000)*1000
          adjust=1
        }
        if ((cilow > 0) && (cihigh <= bootsz)) break()
      }
      boot=bootsz;
      if (mc > 0) mc=bootsz
      if ((boot > 0) && (mc > 0) && ((model > 3) && (model < 6)))  boot=0
      if ((adjust == 1) && (boot > 0))  {
        note[notes,1]=6
        notes=notes+1
      }
      if ((adjust == 1) && (mc > 0))  {
        note[notes,1]=13
        notes=notes+1
      }
    }
    if ((model == 6) && (nmeds > 1))  {
      mmpaths=matrix(0,(nmeds+2),(nmeds+2));
      if (nmeds == 2)  indboot=matrix(999,(boot+1),3)
      if (nmeds == 3)  indboot=matrix(999,(boot+1),7)
      if (nmeds == 4)  indboot=matrix(999,(boot+1),15)
      indlbl = as.matrix(c(
        "Total", "Ind1:", "Ind2:", "Ind3:", "Ind4:", "Ind5:", "Ind6:", "Ind7:", "Ind8:", "Ind9:", "Ind10:", "Ind11:", "Ind12:", "Ind13:", "Ind14:", "Ind15:"
      ))
      indlbl2 = as.matrix(c(
        "Ind1", "Ind2", "Ind3", "Ind4", "Ind5", "Ind6", "Ind7", "Ind8", "Ind9", "Ind10", "Ind11", "Ind12", "Ind13", "Ind14", "Ind15"
      ))
      indces=matrix(999,(boot+1),4);
    }
    if (model < 4)  {
      boot=0
      cmat=matrix(0,10,1)
      zmat=matrix(0,10,1)
    }
    nvarch=matrix(0,1,ncol(dat))
    wmatch=0
    zmatch=0
    vmatch=0
    qmatch=0
    mmatch=0
    minprobe=0
    maxprobe=0
    for (i in 1:ncol(vnames)) {
      if (vnames[,i]==yname)  {
        y=dat[,i, drop=F]
        nvarch[1,i]=1
        yerr=0
        if ((yname==xname)||(yname==wname)||(yname==zname)||(yname==vname)||(yname==qname))  {
          errs=errs+1
          runerrs[errs,1]=17
        }
      }
      if (vnames[,i]==xname)  {
        x=dat[,i, drop=F]
        nvarch[1,i]=1
        xdich=1
        xerr=0
        for (jj in 1:n) {
          if ((x[jj,1, drop=F] != max(x)) && (x[jj,1, drop=F] != min(x)))  {
            xdich=0
            break
          }
        }
        #calculate residual
        xmean = mean(x)
        if ((center == 1) && ((model < 4) || (xlist > 0)))  {
          meanvec=matrix(xmean,n,1)
          x=x-meanvec
          centvar=cbind(centvar,xname)
        }
        xmean = mean(x)
        
        tmp=x-(cons * xmean)
        xsd=sqrt((1/(n-1))*(t(tmp) %*% tmp))
        if (xdich==0)  {
          quantc[1,6]=1
          matx=rbind((xmean-xsd),xmean,(xmean+xsd))
          if ((xmodval == 999) && (quantile == 0))  {
            if ((matx[1,1, drop=F] < min(x)) && (model==74))  {
              matx[1,1]=min(x)
              minprobe=1
            }
            if ((matx[3,1, drop=F] > max(x)) && (model==74))  {
              matx[3,1]=max(x)
              maxprobe=1
            }
          }
          if (quantile == 1)  {
            quantd[1,6]=1
            quantc[1,6]=0
            #sort tmp, smallest to greatest
            tmp=x[order(x),, drop=F]
            matx=rbind(tmp[floor(n*0.10),1, drop=F],tmp[floor(n*0.25),1, drop=F],tmp[floor(n*0.5),1, drop=F],tmp[floor(n*0.75),1, drop=F],tmp[floor(n*0.9),1, drop=F])
          }
        }
        if (xdich==1)  {
          matx=rbind(min(x),max(x))
          if (model==74)  {
            matx=min(x)
            mod74dic=1
          }
        }
        if (xmodval != 999)  {
          matx=xmodval
          quantd[1,6]=0
          quantc[1,6]=0
        }
      }
      if ((werr==0) && (wlist==1))  {
        if (vnames[,i]==wname)  {
          werr=0;
          wmatch=1;
          w=dat[,i, drop=F]
          if (center == 1)  {
            wmean = mean(w)
            meanvec=matrix(wmean,n,1);
            w=w-meanvec;
            centvar=cbind(centvar,wname)
          }
          nvarch[1,i]=1
          nmods=nmods+1
          wmean = mean(w)
          tmp=w-(cons*wmean)
          wsd=sqrt((1/(n-1))*(t(tmp) %*% tmp));
          wdich=1;
          for (jj in 1:n) {
            if ((w[jj,1, drop=F] != max(w)) && (w[jj,1, drop=F] != min(w)))  {
              wdich=0
              break()
            }
          }
          if (model == 3)  {
            jndich=wdich
            jnmin=min(w)
            jnmax=max(w)
          }
          if (wdich == 0)  {
            matw=rbind((wmean-wsd),wmean,(wmean+wsd))
            quantc[1,1]=1;
            if ((wmodval==999) && (quantile==0))  {
              if (matw[1,1, drop=F] < min(w))  {
                matw[1,1]=min(w)
                minprobe=1
              }
              if (matw[3,1, drop=F] > max(w))  {
                matw[3,1]=max(w)
                maxprobe=1
              }
            }
            if (quantile == 1)  {
              quantd[1,1]=1
              quantc[1,1]=0
              tmp = w[order(w),, drop=F]
              matw=rbind(tmp[floor(n*0.10),1, drop=F],tmp[floor(n*0.25),1, drop=F],tmp[floor(n*0.5),1, drop=F],tmp[floor(n*0.75),1, drop=F],tmp[floor(n*0.9),1, drop=F])
            }
          }
          if (wdich==1)  {
            matw=rbind(min(w),max(w))
            wvdich=1
            cmaxw=max(w)
            cminw=min(w)
          }
          if (wmodval != 999)  {
            matw=wmodval
            quantd[1,1]=0
            quantc[1,1]=0
          }
          modmatv[1,1]=nrow(matw);
          modmat[(1:nrow(matw)),1]=matw;
          modvnm[1,1]=wname;
          modmatp[1,1]=1;
        }
      }
      if ((zerr==0) && (zlist==1))  {
        if (vnames[,i]==zname)  {
          zerr=0
          zmatch=1
          z=dat[,i, drop=F]
          if (center == 1)  {
            zmean = mean(z)
            meanvec=matrix(zmean,n,1)
            z=z-meanvec
            centvar=cbind(centvar,zname)
          }
          nvarch[1,i]=1
          nmods=nmods+1
          zmean = mean(z)
          tmp=z-(cons*zmean)
          zsd=sqrt((1/(n-1))*(t(tmp) %*% tmp));
          zdich=1;
          for (jj in 1:n) {
            if ((z[jj,1, drop=F] != max(z)) && (z[jj,1, drop=F] != min(z)))  {
              zdich=0
              break()
            }
          }
          if (zdich == 0)  {
            matz=rbind((zmean-zsd),zmean,(zmean+zsd))
            quantc[1,2]=1;
            if ((zmodval==999) && (quantile==0))  {
              if (matz[1,1, drop=F] < min(z))  {
                matz[1,1]=min(z)
                minprobe=1
              }
              if (matz[3,1, drop=F] > max(z))  {
                matz[3,1]=max(z)
                maxprobe=1
              }
            }
            if (quantile == 1)  {
              quantd[1,2]=1
              quantc[1,2]=0
              tmp=z[order(z),, drop=F]
              matz=rbind(tmp[floor(n*0.10),1, drop=F],tmp[floor(n*0.25),1, drop=F],tmp[floor(n*0.5),1, drop=F],tmp[floor(n*0.75),1, drop=F],tmp[floor(n*0.9),1, drop=F])
            }
          }
          if (zdich==1)  {
            matz=rbind(min(z),max(z))
          }
          if (zmodval != 999)  {
            matz=zmodval
            quantd[1,2]=0
            quantc[1,2]=0
          }
          modmatv[1,2]=nrow(matz);
          modmat[(1:nrow(matz)),2]=matz;
          modvnm[1,2]=zname;
          modmatp[1,2]=1;
        }
      }
      if ((verr==0) && (vlist==1))  {
        if (vnames[,i]==vname)  {
          verr=0
          vmatch=1
          v=dat[,i, drop=F]
          if (center == 1)  {
            vmean = mean(v)
            meanvec=matrix(vmean,n,1)
            v=v-meanvec
            centvar=cbind(centvar,vname)
          }
          nvarch[1,i]=1
          nmods=nmods+1
          vmean = mean(v)
          tmp=v-(cons*vmean)
          vsd=sqrt((1/(n-1))*(t(tmp) %*% tmp));
          vdich=1;
          for (jj in 1:n) {
            if ((v[jj,1, drop=F] != max(v)) && (v[jj,1, drop=F] != min(v)))  {
              vdich=0
              break()
            }
          }
          if (vdich == 0)  {
            matv=rbind((vmean-vsd),vmean,(vmean+vsd))
            quantc[1,3]=1;
            if (vmodval==999)  {
              if (matv[1,1, drop=F] < min(v))  {
                matv[1,1]=min(v)
                minprobe=1
              }
              if (matv[3,1, drop=F] > max(v))  {
                matv[3,1]=max(v)
                maxprobe=1
              }
            }
            if (quantile == 1)  {
              quantd[1,3]=1
              quantc[1,3]=0
              tmp=v[order(v),, drop=F]
              matv=rbind(tmp[floor(n*0.10),1, drop=F],tmp[floor(n*0.25),1, drop=F],tmp[floor(n*0.5),1, drop=F],tmp[floor(n*0.75),1, drop=F],tmp[floor(n*0.9),1, drop=F])
            }
          }
          if (vdich==1)  {
            matv=rbind(min(v),max(v))
            wvdich=1
            cmaxv=max(v)
            cminv=min(v)
          }
          if (vmodval != 999)  { 
            matv=vmodval
            quantd[1,3]=0
            quantc[1,3]=0
          }
          modmatv[1,3]=nrow(matv);
          modmat[(1:nrow(matv)),3]=matv;
          modvnm[1,3]=vname;
          modmatp[1,3]=1;
        }
      }
      if ((qerr == 0) && (qlist==1))  {
        if (vnames[,i] == qname)  {
          qerr=0
          qmatch=1
          q=dat[,i, drop=F]
          if (center == 1)  {
            qmean = mean(q)
            meanvec=matrix(qmean,n,1);
            q=q-meanvec;
            centvar=cbind(centvar,qname)
          }
          nvarch[1,i]=1
          nmods=nmods+1
          qmean=mean(q)
          tmp=q-(cons*qmean)
          qsd=sqrt((1/(n-1))*(t(tmp) %*% tmp));
          qdich=1;
          for (jj in 1:n) {
            if ((q[jj,1, drop=F] != max(q)) && (q[jj,1, drop=F] != min(q)))  {
              qdich=0
              break()
            }
          }
          if (qdich == 0)  {
            matq=rbind((qmean-qsd),qmean,(qmean+qsd))
            quantc[1,4]=1
            if ((qmodval==999) && (quantile == 0))  {
              if (matq[1,1, drop=F] < min(q))  {
                matq[1,1]=min(q)
                minprobe=1
              }
              if (matq[3,1, drop=F] > max(q))  {
                matq[3,1]=max(q)
                maxprobe=1
              }
            }
            if (quantile == 1)  {
              quantd[1,4]=1
              quantc[1,4]=0
              tmp=q[order(q),, drop=F]
              matq=rbind(tmp[floor(n*0.10),1, drop=F],tmp[floor(n*0.25),1, drop=F],tmp[floor(n*0.5),1, drop=F],tmp[floor(n*0.75),1, drop=F],tmp[floor(n*0.9),1, drop=F])
            }
          }
          if (qdich==1) matq=rbind(min(q), max(q))
          if (qmodval != 999)  {
            matq=qmodval
            quantd[1,4]=0
            quantc[1,4]=0;
          }
          modmatv[1,4]=nrow(matq);
          modmat[(1:nrow(matq)),4]=matq;
          modvnm[1,4]=qname;
          modmatp[1,4]=1;
        }
      }
      if (vnames[,i]==clname)  {
        cld=dat[,i, drop=F]
        cvname=vnames[,i, drop=F]
        nvarch[1,i]=1
        clsmtch=1
      }
      for (j in 1:ncol(mnames)) {
        if (vnames[,i]==mnames[1,j, drop=F])  {
          mmatch=mmatch+1
          m[,j]=dat[,i, drop=F]
          if ((center==1) && (nvarch[1,i]==0) && (mlist > 0))  {
            tmp=m[,j, drop=F]
            meanvec=matrix(mean(tmp),n,1)
            m[,j]=m[,j, drop=F]-meanvec
            mmmm=m[,j, drop=F]
            centvar=cbind(centvar,mnames[1,j, drop=F])
          }
          nvarch[1,i]=1;
          dichm=1;
          for(jj in 1:n) {
            if ((m[jj,j, drop=F] != max(m[,j, drop=F])) && (m[jj,j, drop=F] != min(m[,j, drop=F])))  {
              dichm=0
              break()
            }
          }
          if ((dichm==1) && (model > 3) && (mcheck == 0))  {
            errs=errs+1
            runerrs[errs,1]=1
            mcheck=1
          }
          if ((model <=3) && (ncol(mnames) == 1))  {
            tmp=m[,j, drop=F]
            nmods=nmods+1
            mmean=mean(tmp)
            tmp=m[,j, drop=F]-(cons*mmean)
            msd=sqrt((1/(n-1))*(t(tmp) %*% tmp))
            mdich=1;
            for(jj in 1:n) {
              if ((m[jj,j, drop=F] != max(m[,j, drop=F])) && (m[jj,j, drop=F] != min(m[,j, drop=F])))  {
                mdich=0
                break()
              }
            }
            if (model == 1)  {
              jndich=mdich
              jnmin=min(m[,j, drop=F])
              jnmax=max(m[,j, drop=F])
            }
            if (mdich==0)  {
              matm=rbind((mmean-msd),mmean,(mmean+msd))
              quantc[1,5]=1
              if ((mmodval==999) && (quantile==0))  {
                if (matm[1,1, drop=F] < min(m[,j, drop=F]))  {
                  matm[1,1]=min(m[,j, drop=F])
                  minprobe=1;
                }
                if (matm[3,1, drop=F] > max(m[,j, drop=F]))  {
                  matm[3,1]=max(m[,j, drop=F])
                  maxprobe=1;
                }
              }
              if (quantile == 1)  {
                quantd[1,5]=1
                quantc[1,5]=0
                tmp=m[order(m[,j, drop=F]),, drop=F]
                matm=rbind(tmp[floor(n*0.10),1, drop=F],tmp[floor(n*0.25),1, drop=F],tmp[floor(n*0.5),1, drop=F],tmp[floor(n*0.75),1, drop=F],tmp[floor(n*0.9),1, drop=F])
              }
            }
            if (mdich == 1)  {
              matm=rbind(min(m),max(m))
            }
            if (mmodval != 999)  {
              matm=mmodval
              quantd[1,5]=0
              quantc[1,5]=0;
            }
            modmatv[1,5]=nrow(matm);
            modmat[(1:nrow(matm)),5]=matm;
            modvnm[1,5]=mnames[1,j, drop=F];
            modmatp[1,5]=1;
          }
        }
      }
    }
    if (minprobe==1) {
      note[notes,1]=14
      notes=notes+1
    }
    if (maxprobe==1) {
      note[notes,1]=15
      notes=notes+1
    }
    if ((clname != "XXX") && (clsmtch == 0))  {
      errs=errs+1
      runerrs[errs,1]=23
    }
    if (clname != "XXX")  {
      if ((clname==zname) || (clname==vname) || (clname==wname) || (clname==xname) || (clname==yname) || (clname==qname))  {
        errs=errs+1
        runerrs[errs,1]=24
      }
    }
    if ((wlist==1) && (werr==0) && (wmatch==0))  {
      werr=3
      errs=errs+1
      runerrs[errs,1]=4
    }
    if ((zlist==1) && (zerr==0) && (zmatch==0))  {
      zerr=3
      errs=errs+1
      runerrs[errs,1]=5
    }
    if ((qlist==1) && (qerr==0) && (qmatch==0))  {
      qerr=3
      errs=errs+1
      runerrs[errs,1]=6
    }
    if ((vlist==1) && (verr==0) && (vmatch==0))  {
      verr=3
      errs=errs+1
      runerrs[errs,1]=7
    }
    if (yerr == 1)  {
      errs=errs+1
      runerrs[errs,1]=16
    }
    if (xerr == 1)  {
      errs=errs+1
      runerrs[errs,1]=28
    }
    if ((model==6) && (nmeds < 2))  {
      errs=errs+1
      runerrs[errs,1]=18
    }
    if (mmatch < ncol(mnames))  {
      errs=errs+1
      runerrs[errs,1]=25
    }
  } 
  
  if (clname != "XXX")  {
    cld = model.matrix( ~ factor(cld) - 1)
    attributes(cld)$dimnames <- NULL
    cluster=ncol(cld)
    cld=cld[,2:ncol(cld), drop=F]
    clsdmy=ncol(cld)
    if (clsdmy > 19)  {
      errs=errs+1
      runerrs[err,1]=26 
    }
  }
  
  
  if (errs == 0)  {
    tmp1=as.matrix(rowSums(quantd))
    tmp2=as.matrix(rowSums(quantc))
    if (tmp1 > 0)  {
      note[notes,1]=4
      notes=notes+1
    }
    if (tmp2 > 0)  {
      note[notes,1]=5
      notes=notes+1
    }
    #test for dichotomous dependent Y
    if (length(unique(y)) == 2) dichy = 1 else dichy = 0
    if (dichy == 1)  {
      jncrit=xp2 ^2
    }
    ncovs = ncol(dat)-as.matrix(rowSums(nvarch))
    if (ncovs > 0)  {
      c=matrix(0,n,ncovs)
      cnames=matrix(c("x"))
      j=1
      for (i in 1:ncol(vnames)) {
        if (nvarch[1,i]==0)  {
          c[,j]=dat[,i, drop=F]
          nvarch[1,i]=1
          j=j+1
          cnames=cbind(cnames,vnames[,i, drop=F])
        }
      }
      cnames=cnames[1,2:ncol(cnames), drop=F];
    }
    names=cbind(yname,xname,mnames,wname,zname,vname,qname)
    if (ncovs > 0)  {
      names=cbind(names,cnames)
    }
    if ((dichy==1) && (effsize==1))  {
      note[notes,1]=2
      notes=notes+1
    }
    if ((model > 3) && (model < 6))  {
      indeff=matrix(0,nmeds,1)
      indboot=matrix(999,(boot+1),nmeds)
      if (mc > 0)  indboot=matrix(999,(mc+1),nmeds)
      if ((effsize==1) && (dichy==0) && (ncovs==0))  {
        rmeff=matrix(999,(boot+1),(nmeds+1));
        abpseff=matrix(999,(boot+1),(nmeds+1));
        abcseff=matrix(999,(boot+1),(nmeds+1));
        pmeff=matrix(999,(boot+1),(nmeds+1));
        r245=matrix(999,(boot+1),1);
        kappa2=matrix(999,(boot+1),1);
      }
    }
    if ((model == 6) && (effsize==1) && (dichy==0) && (ncovs == 0))  {
      rmeff=matrix(999,(boot+1),ncol(indboot));
      abpseff=matrix(999,(boot+1),ncol(indboot));
      abcseff=matrix(999,(boot+1),ncol(indboot));
      pmeff=matrix(999,(boot+1),ncol(indboot));
    }
    if (nmods > 0)  {
      tmp=1;
      for (i in 1:5) {
        if (modmatp[1,i]==1)  {
          modmat[,tmp]=modmat[,i, drop=F]
          modvnm[1,tmp]=modvnm[1,i, drop=F]
          modmatv[1,tmp]=modmatv[1,i, drop=F]
          tmp=tmp+1
        }
      }
      modmat=modmat[,(1:nmods), drop=F]
      modvnm=modvnm[,(1:nmods), drop=F]
      modmatv=modmatv[,(1:nmods), drop=F]
      if ((ncol(modmatv)-1) > 0) {
        for (i in 1:(ncol(modmatv)-1)) {
          tmp=1
          for (j in (i+1):ncol(modmatv)) {
            tmp=tmp*modmatv[1,j, drop=F]
          }
          modprod[1,i]=tmp
        }
      }
      modvals=matrix(0,(modmatv[1,1, drop=F] * modprod[1,1, drop=F]),nmods);
      for (i in 1:nmods) {
        strt=1;fnsh=0;
        while (fnsh < nrow(modvals)) {
          for (j in 1:modmatv[1,i]) {
            tmp=matrix(modmat[j,i], modprod[1,i],1);
            fnsh=fnsh+nrow(tmp);
            modvals[(strt:fnsh),i]=tmp;
            strt=fnsh+1;
          }
        }
      }
      if (model == 74)  {
        modvals=matx
        modvnm=xname
      }
      vmat=matrix(0,8,nrow(modvals));
      vmat[1,1:nrow(modvals)]=matrix(1,1,nrow(modvals));
      vmat[5,1:nrow(modvals)]=matrix(1,1,nrow(modvals));
      indeff=matrix(0,nrow(modvals),1);
      if (model != 5)  {
        indboot=matrix(-99999999,((boot+1)*nmeds),nrow(modvals));
        indbootp=matrix(-99999999,(boot+1),nmeds);
      }
    }
    if (nmods > 0)  {
      for (i in 1:ncol(modvals)) {
        if (modvnm[1,i]==wname)  wcol=i
        if (modvnm[1,i]==zname)  zcol=i
        if (modvnm[1,i]==vname)  vcol=i
        if (modvnm[1,i]==qname)  qcol=i
      }
    }
    if (dichy==1)  {
      omx=max(y)
      omn=min(y)
      y=(y==omx)
      rcd=cbind(omn,0)
      rcd1=cbind(omx,1)
      rcd=rbind(rcd,rcd1)
    }
    data=cbind(cons,y,m,x)
    datamed=data
    datayed=data
    datanm=rbind("CONSTANT",yname,t(mnames),xname)
    datanmm=datanm
    datanmy=datanm
    yintkey=cbind(" "," "," "," "," "," ")
    if ((model < 4) && (errs == 0))  {
      yintkeyt=cbind(yintemp[1,intcnt, drop=F],xname,"  X",mnames," "," ")
      yintkey=rbind(yintkey,yintkeyt)
      #elementwise
      datayed=cbind(datayed,(x*m))
      datanmy=rbind(datanmy,yintemp[1,intcnt, drop=F])
      intcnt=intcnt+1;
      for (i in 1:nrow(modvals)) {
        vmat[1,i]=1;
        vmat[2,i]=modvals[i,1, drop=F];
      }
    }
    if ((model == 2) || (model == 3))  {
      #elementwise
      int1=x*w;
      datayed=cbind(datayed,w,int1)
      yintkeyt=cbind(yintemp[1,intcnt, drop=F],xname,"  X",wname," "," ")
      yintkey=rbind(yintkey,yintkeyt)
      datanmy=rbind(datanmy,wname,yintemp[1,intcnt, drop=F])
      intcnt=intcnt+1
      for (i in 1:nrow(modvals)) {
        vmat[2,i]=modvals[i,2, drop=F];
        vmat[3,i]=modvals[i,1, drop=F];
        vmat[4,i]=modvals[i,1, drop=F] * modvals[i,2, drop=F];
      }
    }
    if (model == 3)  {
      yintkeyt=cbind(yintemp[1,intcnt, drop=F],mnames,"  X",wname," "," ")
      yintkey=rbind(yintkey,yintkeyt)
      datanmy=rbind(datanmy,yintemp[1,intcnt, drop=F])
      intcnt=intcnt+1;
      #elementwise
      int1=w*m
      int2=x*w*m
      datayed=cbind(datayed,int1,int2)
      yintkeyt=cbind(yintemp[1,intcnt, drop=F],xname,"  X",mnames,"  X",wname)
      yintkey=rbind(yintkey,yintkeyt)
      datanmy=rbind(datanmy,yintemp[1,intcnt, drop=F])
      intcnt=intcnt+1;
    }
    if ((model == 4) || (model == 5))  {
      vmat=matrix(1,8,1);
    }
    yintkey2=yintkey;
    if (wm == 1)  {
      #elementwise
      int1=x*w;
      datamed=cbind(datamed,w,int1)
      yintkeyt=cbind(yintemp[1,intcnt, drop=F],xname,"  X",wname," "," ")
      yintkey=rbind(yintkey,yintkeyt)
      datanmm=rbind(datanmm,wname,yintemp[1,intcnt, drop=F])
      intcnt=intcnt+1;
      for (i in 1:nrow(modvals)) {
        vmat[2,i]=modvals[i,wcol, drop=F];
      }
      if (zm == 1)  {
        #elementwise
        int1=x*z;
        datamed=cbind(datamed,z,int1)
        yintkeyt=cbind(yintemp[1,intcnt, drop=F],xname,"  X",zname," "," ")
        yintkey=rbind(yintkey,yintkeyt)
        datanmm=rbind(datanmm,zname,yintemp[1,intcnt, drop=F])
        intcnt=intcnt+1;
        for (i in 1:nrow(modvals)) {
          vmat[3,i]=modvals[i,zcol, drop=F];
        }
      }
      if (wzm == 1)  {
        yintkeyt=cbind(yintemp[1,intcnt, drop=F],wname,"  X",zname," "," ")
        yintkey=rbind(yintkey,yintkeyt)
        datanmm=rbind(datanmm,yintemp[1,intcnt, drop=F])
        intcnt=intcnt+1;
        yintkeyt=cbind(yintemp[1,intcnt, drop=F],xname,"  X",wname,"  X",zname)
        yintkey=rbind(yintkey,yintkeyt)
        datanmm=rbind(datanmm,yintemp[1,intcnt, drop=F])
        intcnt=intcnt+1;
        #elementwise
        int1=w*z;
        int2=x*w*z;
        datamed=cbind(datamed,int1,int2)
        for (i in 1:nrow(modvals)) {
          vmat[4,i]=(modvals[i,wcol, drop=F])*(modvals[i,zcol, drop=F]);
        }
      }
    }
    mdatcol=ncol(datamed);
    mintkey=yintkey;
    yintkey=cbind(" "," "," "," "," "," ")
    medints=intcnt-1;
    if ((vy==1) || (xmy==1))  {
      mp=1;
      for (i in 1:nrow(modvals)) {
        vmat[6,i]=modvals[i,1, drop=F];
      }
      if (vy==1)  {
        datayed=cbind(datayed,v)
        datanmy=rbind(datanmy,vname)
        mmods=1
        for (i in 1:nrow(modvals)) {
          vmat[6,i]=modvals[i,vcol, drop=F];
        }
        if (qy==1)  {
          mp=2
          datayed=cbind(datayed,q)
          datanmy=rbind(datanmy,qname)
          mmods=2;
          for (i in 1:nrow(modvals)) {
            vmat[7,i]=modvals[i,qcol, drop=F];
          }
        }
        if (vqy==1)  {
          mp=3
          #elementwise
          datayed=cbind(datayed,(v*q))
          mmodls=3;
          for (i in 1:nrow(modvals)) {
            vmat[8,i]=modvals[i,vcol, drop=F] * modvals[i,qcol, drop=F];
          }
        }
      }
      mints=matrix(0,n,(nmeds*mp));
      for (i in 0:(nmeds-1)) {
        if ((i == 0) && (vqy==1))  {
          yintkeyt=cbind(yintemp[1,intcnt, drop=F],vname,"  X",qname," "," ")
          yintkey=rbind(yintkey,yintkeyt)
          datanmy=rbind(datanmy,yintemp[1,intcnt, drop=F])
          intcnt=intcnt+1;
        }
        if (vy==1)  {
          #elementwise
          mints[,((i*mp)+1)]=m[,(i+1), drop=F]*v;
          yintkeyt=cbind(yintemp[1,intcnt, drop=F],mnames[1,(i+1), drop=F],"  X",vname," "," ")
          yintkey=rbind(yintkey,yintkeyt)
          datanmy=rbind(datanmy,yintemp[1,intcnt, drop=F])
          intcnt=intcnt+1;
        }
        if (xmy==1)  {
          #elementwise
          mints[,((i*mp)+1)]=m[,(i+1), drop=F]*x;
          yintkeyt=cbind(yintemp[1,intcnt, drop=F],mnames[1,(i+1), drop=F],"  X",xname," "," ")
          yintkey=rbind(yintkey,yintkeyt)
          datanmy=rbind(datanmy,yintemp[1,intcnt, drop=F])
          intcnt=intcnt+1;
        }
        if (qy==1)  {
          #elementwise
          mints[,((i*mp)+2)]=m[,(i+1), drop=F]*q;
          yintkeyt=cbind(yintemp[1,intcnt, drop=F],mnames[1,(i+1), drop=F],"  X",qname," "," ")
          yintkey=rbind(yintkey,yintkeyt)
          datanmy=rbind(datanmy,yintemp[1,intcnt, drop=F])
          intcnt=intcnt+1;
          if (vqy==1)  {
            #elementwise
            mints[,((i*mp)+3)]=m[,(i+1), drop=F]*v*q;
            yintkeyt=cbind(yintemp[1,intcnt, drop=F],mnames[1,(i+1), drop=F],"  X",vname,"  X",qname)
            yintkey=rbind(yintkey,yintkeyt)
            datanmy=rbind(datanmy,yintemp[1,intcnt, drop=F])
            intcnt=intcnt+1;
          }
        }
      }
      datayed=cbind(datayed,mints)
    }
    mp=1;
    if (wvmy==1)  {
      mp=2;
      for ( i in 1 : nrow(modvals)) {
        vmat[8,i]=modvals[i,wcol, drop=F] * modvals[i,vcol, drop=F]
      }
    }
    mints2=matrix(0,n,(nmeds*mp));
    if (wmy==1)  {
      for ( i in 1 : nrow(modvals)) {
        vmat[7,i]=modvals[i,wcol, drop=F];
      }
      if ((wy==0) && (model > 3))  {
        datayed=cbind(datayed,w)
        datanmy=rbind(datanmy,wname)
      }
      for (i in 0:(nmeds-1)) {
        if ((i == 0) && (wvmy == 1))  {
          #elementwise
          datayed=cbind(datayed,(w*v))
          yintkeyt=cbind(yintemp[1,intcnt, drop=F],wname,"  X",vname," "," ")
          yintkey=rbind(yintkey,yintkeyt)
          datanmy=rbind(datanmy,yintemp[1,intcnt, drop=F])
          intcnt=intcnt+1;
        }
        #elementwise
        mints2[,((i*mp)+1)]=m[,(i+1), drop=F]*w;
        yintkeyt=cbind(yintemp[1,intcnt, drop=F],mnames[1,(i+1), drop=F],"  X",wname," "," ")
        yintkey=rbind(yintkey,yintkeyt)
        datanmy=rbind(datanmy,yintemp[1,intcnt, drop=F])
        intcnt=intcnt+1;
        if (wvmy==1)  {
          #elementwise
          mints2[,((i*mp)+2)]=m[,(i+1), drop=F]*w*v;
          yintkeyt=cbind(yintemp[1,intcnt, drop=F],mnames[1,(i+1), drop=F],"  X",wname,"  X",vname)
          yintkey=rbind(yintkey,yintkeyt)
          datanmy=rbind(datanmy,yintemp[1,intcnt, drop=F])
          intcnt=intcnt+1
        }
      }
      datayed=cbind(datayed,mints2)
    }
    mp=1;
    if (zmy==1)  {
      for ( i in 1 : nrow(modvals)) {
        vmat[6,i]=modvals[i,zcol, drop=F];
      }
      if (wzmy==1)  {
        mp=2;
        for ( i in 1 : nrow(modvals)) {
          vmat[8,i]=modvals[i,zcol, drop=F] * modvals[i,wcol, drop=F];
        }
      }
    }
    if (zmy==1)  {
      mints3=matrix(0,n,(nmeds*mp));
      if (zy==0)  {
        datayed=cbind(datayed,z)
        datanmy=rbind(datanmy,zname)
      }
      for ( i in 0 : (nmeds-1)) {
        if ((i==0) && (wzmy == 1) && (wzy==0))  {
          #elementwise
          datayed=cbind(datayed,(w*z))
          yintkeyt=cbind(yintemp[1,intcnt, drop=F],wname,"  X",zname," "," ")
          yintkey=rbind(yintkey,yintkeyt)
          datanmy=rbind(datanmy,yintemp[1,intcnt, drop=F])
          intcnt=intcnt+1;
        }
        #elementwise
        mints3[,((i*mp)+1)]=m[,(i+1), drop=F]*z;
        yintkeyt=cbind(yintemp[1,intcnt, drop=F],mnames[1,(i+1), drop=F],"  X",zname," "," ")
        yintkey=rbind(yintkey,yintkeyt)
        datanmy=rbind(datanmy,yintemp[1,intcnt, drop=F])
        intcnt=intcnt+1;
        if (wzmy == 1)  {
          #elementwise
          mints3[,((i*mp)+2)]=m[,(i+1), drop=F]*w*z;
          yintkeyt=cbind(yintemp[1,intcnt, drop=F],mnames[1,(i+1), drop=F],"  X",wname,"  X",zname)
          yintkey=rbind(yintkey,yintkeyt)
          datanmy=rbind(datanmy,yintemp[1,intcnt, drop=F])
          intcnt=intcnt+1;
        }
      }
      datayed=cbind(datayed,mints3)
    }
    decoc=1;
    modmat=matrix(999,5,5)
    modmatv=matrix(1,1,5)
    modmatp=matrix(0,1,5)
    modprod=modmatv
    if ((wy == 1) && (model > 3))  {
      #elementwise
      datayed=cbind(datayed,w,x*w)
      decoc=decoc+1;
      deco[decoc,1]=ncol(datayed)-1;
      modmatv[1,1]=nrow(matw);
      modmat[(1:nrow(matw)),1]=matw;
      modvnm2[1,1]=wname;
      modmatp[1,1]=1;
      yintkeyt=cbind(yintemp[1,intcnt, drop=F],xname,"  X",wname," "," ")
      yintkey=rbind(yintkey,yintkeyt)
      datanmy=rbind(datanmy,wname,yintemp[1,intcnt, drop=F])
      intcnt=intcnt+1;
    }
    if (zy == 1)  {
      #elementwise
      datayed=cbind(datayed,z,x*z)
      decoc=decoc+1;
      deco[decoc,1]=ncol(datayed)-1;
      modmatv[1,2]=nrow(matz);
      modmat[(1:nrow(matz)),2]=matz;
      modvnm2[1,2]=zname;
      modmatp[1,2]=1;
      yintkeyt=cbind(yintemp[1,intcnt, drop=F],xname,"  X",zname," "," ")
      yintkey=rbind(yintkey,yintkeyt)
      datanmy=rbind(datanmy,wname,yintemp[1,intcnt, drop=F])
      intcnt=intcnt+1;
    }
    if (wzy == 1)  {
      #elementwise
      datayed=cbind(datayed,w*z,x*w*z)
      decoc=decoc+1;
      deco[decoc,1]=ncol(datayed)-1;
      yintkeyt=cbind(yintemp[1,intcnt, drop=F],wname,"  X",zname," "," ")
      yintkey=rbind(yintkey,yintkeyt)
      datanmy=rbind(datanmy,wname,yintemp[1,intcnt, drop=F])
      intcnt=intcnt+1;
      yintkeyt=cbind(yintemp[1,intcnt, drop=F],xname,"  X",wname,"  X",zname)
      yintkey=rbind(yintkey,yintkeyt)
      datanmy=rbind(datanmy,wname,yintemp[1,intcnt, drop=F])
      intcnt=intcnt+1;
    }
    if (vxy == 1)  {
      #elementwise
      datayed=cbind(datayed,x*v)
      decoc=decoc+1;
      deco[decoc,1]=ncol(datayed)-1;
      modmatv[1,3]=nrow(matv);
      modmat[(1:nrow(matv)),3]=matv;
      modvnm2[1,3]=vname;
      modmatp[1,3]=1;
      yintkeyt=cbind(yintemp[1,intcnt, drop=F],xname,"  X",vname," "," ")
      yintkey=rbind(yintkey,yintkeyt)
      datanmy=rbind(datanmy,wname,yintemp[1,intcnt, drop=F])
      intcnt=intcnt+1;
      if (qxy == 1)  {
        #elementwise
        datayed=cbind(datayed,x*q)
        decoc=decoc+1;
        deco[decoc,1]=ncol(datayed)-1;
        modmatv[1,4]=nrow(matq);
        modmat[(1:nrow(matq)),4]=matq;
        modvnm2[1,4]=qname;
        modmatp[1,4]=1;
        yintkeyt=cbind(yintemp[1,intcnt, drop=F],xname,"  X",qname," "," ")
        yintkey=rbind(yintkey,yintkeyt)
        datanmy=rbind(datanmy,wname,yintemp[1,intcnt, drop=F])
        intcnt=intcnt+1;
        if (vqxy == 1)  {
          #elementwise
          datayed=cbind(datayed,x*v*q)
          decoc=decoc+1;
          deco[decoc,1]=ncol(datayed)-1;
          yintkeyt=cbind(yintemp[1,intcnt, drop=F],xname,"  X",vname,"  X",qname)
          yintkey=rbind(yintkey,yintkeyt)
          datanmy=rbind(datanmy,wname,yintemp[1,intcnt, drop=F])
          intcnt=intcnt+1;
        }
      }
    }
    if (wvxy == 1)  {
      #elementwise
      datayed=cbind(datayed,x*w*v)
      decoc=decoc+1;
      deco[decoc,1]=ncol(datayed)-1;
      yintkeyt=cbind(yintemp[1,intcnt, drop=F],xname,"  X",wname,"  X",vname)
      yintkey=rbind(yintkey,yintkeyt)
      datanmy=rbind(datanmy,wname,yintemp[1,intcnt, drop=F])
      intcnt=intcnt+1;
    }
    modvalsd=0
    ttt=as.matrix(rowSums(modmatp))
    ssss=ttt
    if (ssss > 0)  {
      tmp=1;
      for ( i in 1 : 5) {
        if (modmatp[1,i]==1)  {
          modmat[,tmp]=modmat[,i, drop=F];
          modvnm2[1,tmp]=modvnm2[1,i, drop=F];
          modmatv[1,tmp]=modmatv[1,i, drop=F];
          tmp=tmp+1;
        }
      }
      modmat=modmat[,(1:ttt), drop=F];
      modvnm2=modvnm2[,(1:ttt), drop=F];
      modmatv=modmatv[,(1:ttt), drop=F];
      if ((ncol(modmatv)-1) > 0) {
        for ( i in 1:(ncol(modmatv)-1)) {
          tmp=1;
          for ( j in (i+1) : ncol(modmatv)) {
            tmp=tmp*modmatv[1,j, drop=F];
          }
          modprod[1,i]=tmp;
        }
      }
      modvalsd=matrix(0,(modmatv[1,1, drop=F]*modprod[1,1, drop=F]),ttt)
      for (i in 1:ttt) {
        strt=1
        fnsh=0
        while (fnsh < nrow(modvalsd)) {
          for ( j in 1 : modmatv[1,i]) {
            tmp=matrix(modmat[j,i], modprod[1,i],1);
            fnsh=fnsh+nrow(tmp);
            modvalsd[(strt:fnsh),i]=tmp;
            strt=fnsh+1;
          }
        }
      }
    }
    if (ttt > 0)  {
      for ( i in 1 : ncol(modvalsd)) {
        if (modvnm2[1,i] == wname)  wcol=i
        if (modvnm2[1,i] == zname)  zcol=i
        if (modvnm2[1,i] == vname)  vcol=i
        if (modvnm2[1,i] == qname)  qcol=i
      }
      directv=matrix(1,nrow(modvalsd),1);
      #all elementwise
      if (wy==1)  directv=cbind(directv,modvalsd[,wcol, drop=F])
      if (zy==1)  directv=cbind(directv,modvalsd[,zcol, drop=F])
      if (wzy==1)  directv=cbind(directv,(modvalsd[,wcol, drop=F])*(modvalsd[,zcol, drop=F]))
      if (vxy==1)  directv=cbind(directv,modvalsd[,vcol, drop=F])
      if (qxy==1)  directv=cbind(directv,modvalsd[,qcol, drop=F])
      if (vqxy==1) directv=cbind(directv,(modvalsd[,vcol, drop=F])*(modvalsd[,qcol, drop=F]))
      if (wvxy==1) directv=cbind(directv,(modvalsd[,vcol, drop=F])*(modvalsd[,wcol, drop=F]))
    }
    ydatacol=ncol(datayed);
    if (ncovs > 0)  {
      if (covmy != 2)  {
        datamed=cbind(datamed,c)
      }
      if (covmy != 1)  {
        datayed=cbind(datayed,c)
      }
      covmeans=matrix(colSums(c),nrow=1)/n
    }
    if (cluster > 0)  {
      datamed=cbind(datamed,cld)
      datayed=cbind(datayed,cld)
      cldmeans=matrix(colSums(cld),nrow=1)/n
    }
    mst=3;
    mnd=mst+nmeds-1;
    ydatacol=ncol(datayed);
    mdatacol=ncol(datamed);
    if (ncovs > 0)  {
      datanmy=rbind(datanmy,t(cnames))
      if (model > 3)  {
        datanmm=rbind(datanmm,t(cnames))
      }
    }
    datanmy=rbind("constant",datanmy[3:nrow(datanmy),1, drop=F])
    if (model > 3)  {
      datanmm=rbind("constant",datanmm[3:nrow(datanmm),1, drop=F])
    }
    amm=matrix(0,2,1)
    abmm=matrix(0,2,1)
    mnv=datayed[,2, drop=F]/n
    mnv=matrix(colSums(mnv),nrow=1)
    mnv=matrix(mnv,n,1)
    ssty=(datayed[,2, drop=F]-mnv) ^ 2
    ssty=matrix(colSums(ssty),nrow=1)
    sigma=(n*t(datayed) %*% datayed)-as.vector(t(matrix(colSums(datayed),nrow=1)) %*% matrix(colSums(datayed),nrow=1));
    sigma=sigma/(n*(n-1));
    stddevy=sqrt(sigma[2,2, drop=F]);
    stddevx=sqrt(sigma[(3+nmeds),(3+nmeds), drop=F]);
    r2xy=(sigma[2,(3+nmeds), drop=F]/(stddevy*stddevx)) ^ 2;
    r2my=(sigma[2,3, drop=F]/(stddevy*sqrt(sigma[3,3, drop=F]))) ^ 2;
    ctot=sigma[2,(3+nmeds), drop=F]/sigma[(3+nmeds),(3+nmeds), drop=F];
    if ((model == 4) && (nmeds == 1) && (cluster == 0) && (ncovs == 0))  {
      kappaa=sigma[2,3, drop=F]*sigma[2,4, drop=F];
      kappab=sqrt((sigma[3,3, drop=F]*sigma[2,2, drop=F])-(sigma[2,3, drop=F]*sigma[2,3, drop=F]));
      kappac=sqrt((sigma[4,4, drop=F]*sigma[2,2, drop=F])-(sigma[2,4, drop=F]*sigma[2,4, drop=F]));
      kappad=sigma[4,4, drop=F]*sigma[2,2, drop=F];
      kappae=sqrt((sigma[4,4, drop=F]*sigma[3,3, drop=F])-(sigma[3,4, drop=F]*sigma[3,4, drop=F]));
      amm[1,1]=(kappaa+(kappab*kappac))/kappad;
      amm[2,1]=(kappaa-(kappab*kappac))/kappad;
      if (sigma[3,4, drop=F] < 0)  {
        amma=min(amm);
      }
      if (sigma[3,4, drop=F] > 0)  {
        amma=max(amm);
      }
      abmm[1,1]=-amma*(kappac/kappae);
      abmm[2,1]=amma*(kappac/kappae);
    }
    datatm=datamed;
    dataty=datayed;
    mdlnms2=rbind(model,yname,xname)
    mdlnms=rbind("Model =","Y     = ","X     = ")
    for (i in 1:ncol(mnames)) {
      mdlnms2=rbind(mdlnms2,mnames[1,i, drop=F])
      if ((i==1) && (ncol(mnames)==1)) {
        mdlnms=rbind(mdlnms,"M     = ")
      } else {
        mdlnms=rbind(mdlnms,mlab[i,1, drop=F])
      }
    }
    if (wname != "XXX")  {
      mdlnms2=rbind(mdlnms2,wname)
      mdlnms=rbind(mdlnms,"W     = ")
    }
    if (zname != "XXX")  {
      mdlnms2=rbind(mdlnms2,zname)
      mdlnms=rbind(mdlnms,"Z     = ")
    }
    if (vname != "XXX")  {
      mdlnms2=rbind(mdlnms2,vname)
      mdlnms=rbind(mdlnms,"V     = ")
    }
    if (qname != "XXX")  {
      mdlnms2=rbind(mdlnms2,qname)
      mdlnms=rbind(mdlnms,"Q     = ")
    }
    if ((jn == 1) && (model == 1) && (jndich == 1))  {
      note[notes,1]=8;
      notes=notes+1;
    }
    if ((jn == 1) && (model == 3) && (jndich == 1))  {
      note[notes,1]=8;
      notes=notes+1;
    }
    if ((effsize == 1) && (ncovs != 0))  {
      note[notes,1]=10;
      notes=notes+1;
    }
    
    write_output("Model Parameters:\n", output=medmod_file)
    write_output(mdlnms2,row.names = mdlnms, col.names=FALSE, output=medmod_file);
    if (ncovs > 0)  {
      write_output("\nStatistical controls:", output=medmod_file)
      write_output(cnames, row.names=FALSE, col.names=FALSE, output=medmod_file);
    }
    write_output("\nSample size:", output=medmod_file)
    write_output(n, output=medmod_file)
    if (cluster > 0)  {
      write_output("\nClustering variable and number of clusters:", output=medmod_file)
      write_output(cluster, row.names = cvname, col.names=FALSE, output=medmod_file);
    }
    if ((model > 3) && (seed != 0))  {
      seedt=seed;
      write_output("\nCustom seed:", output=medmod_file)
      write_output(seedt, row.names=FALSE, col.names=FALSE, output=medmod_file);
    }
    for (bt in 1:(boot+1)) {
      if ((bt == 2) && (savboot == 1))  {
        bootstrp=matrix(-999,boot,ncol(bootcoef));
      }
      bootcoef=0;
      if (bt > 1)  {
        rk=1;
        while (rk == 1) {
          v=floor((runif(matrix(seed,n,1)))*n)+1;
          datayed=dataty[v,, drop=F]
          detcheck=det(t(datayed) %*% datayed);
          rk=(detcheck==0);
          if (model > 3)  {
            datamed=datatm[v,, drop=F];
            detcheck=det(t(datamed) %*% datamed);
            if (rk==1) rk=(detcheck==0)
          }
          sigma=(n*t(datayed) %*% datayed)-(t(matrix(colSums(datayed),nrow=1)) %*% matrix(colSums(datayed),nrow=1));
          sigma=sigma/(n*(n-1));
          temp=as.matrix(diag(sigma));
          bad=bad+rk;
          false=1;
        }
        stddevy=sqrt(sigma[2,2, drop=F]);
        stddevx=sqrt(sigma[(3+nmeds),(3+nmeds), drop=F]);
        ctot=sigma[2,(3+nmeds), drop=F]/sigma[(3+nmeds),(3+nmeds), drop=F];
        if ((model == 4) && (nmeds == 1) && (cluster == 0) && (ncovs == 0))  {
          r2xy=(sigma[2,4, drop=F]/(stddevy*stddevx)) ^ 2;
          r2my=(sigma[2,3, drop=F]/(stddevy*sqrt(sigma[3,3, drop=F]))) ^ 2;
          sstot=sigma[2,2, drop=F]*(n-1);
          kappaa=sigma[2,3, drop=F]*sigma[2,4, drop=F];
          kappab=sqrt((sigma[3,3, drop=F]*sigma[2,2, drop=F])-(sigma[2,3, drop=F]*sigma[2,3, drop=F]));
          kappac=sqrt((sigma[4,4, drop=F]*sigma[2,2, drop=F])-(sigma[2,4, drop=F]*sigma[2,4, drop=F]));
          kappad=sigma[4,4, drop=F]*sigma[2,2, drop=F];
          kappae=sqrt((sigma[4,4, drop=F]*sigma[3,3, drop=F])-(sigma[3,4, drop=F]*sigma[3,4, drop=F]));
          amm[1,1]=(kappaa+(kappab*kappac))/kappad;
          amm[2,1]=(kappaa-(kappab*kappac))/kappad;
          if (sigma[3,4, drop=F] < 0)  {
            amma=min(amm);
          }
          if (sigma[3,4, drop=F] > 0)  {
            amma=max(amm);
          }
          abmm[1,1]=-amma*(kappac/kappae);
          abmm[2,1]=amma*(kappac/kappae);
        }
      }
      # mediator model;
      if (model > 3)  {
        for (im in 1:nmeds) {
          xm=cbind(cons,datamed[,((mnd+1):mdatacol), drop=F])
          xmnm=rbind("Constant",datanmm[(2+nmeds):nrow(datanmm),1, drop=F])
          # solve for least squares coefficients
          # ols equation, mediator(datamed) is Y
          invxtx=solve(t(xm) %*% xm)
          coeff=invxtx %*% t(xm) %*% datamed[,(2+im), drop=F];
          if (model == 6)  {
            if (im == 1)  {
              xm=cbind(cons,datamed[,((mnd+1):mdatacol), drop=F])
              invxtx=solve(t(xm) %*% xm)
              coeff=invxtx %*% t(xm) %*% datamed[,(2+im), drop=F];
            }
            if (im > 1)  {
              xm=cbind(cons,datamed[,(3:(im+1)), drop=F],datamed[,((mnd+1):mdatacol), drop=F])
              xmnm=rbind("Constant",datanmm[(2:im),1, drop=F],datanmm[(mnd:nrow(datanmm)),1, drop=F])
              invxtx=solve(t(xm) %*% xm)
              coeff=invxtx %*% t(xm) %*% datamed[,(2+im), drop=F];
              mmpaths[(im+1),(2:im)]=t(coeff[(2:im),1, drop=F]);
            }
          }
          bootcoef=cbind(bootcoef,t(coeff[1:(nrow(coeff)-clsdmy),1, drop=F]))
          if (bt == 1)  {
            resid=datamed[,(2+im), drop=F]-xm %*% coeff;
            sse=sum(resid^2)
            mse=sse/(n-ncol(xm));
            mnv=data[,(2+im), drop=F]
            mnv=matrix(colSums(mnv),nrow=1)/n
            mnv=matrix(mnv,n,1);
            sstm=(data[,(2+im), drop=F]-mnv) ^ 2
            sstm=matrix(colSums(sstm),nrow=1)
            k3=nrow(coeff);
            if (hc3 == 1)  {
              h=xm[,1, drop=F];
              for (i3 in 1:n) {
                h[i3,1]=xm[i3,, drop=F] %*% invxtx %*% t(xm[i3,, drop=F])
              }
              for (i3 in 1:k3) {
                #elementwise
                xm[,i3]=(resid[,ncol(resid), drop=F]/(1-h))*(xm[,i3, drop=F]);
              }
            }
            if (hc3 != 1)  {
              for ( i3 in 1 : k3) {
                #elementwise
                xm[,i3]=sqrt(mse)*xm[,i3, drop=F];
              }
            }
            lmat=as.matrix(diag(nrow(coeff)));
            lmat=lmat[,2:ncol(lmat), drop=F];
            hccov=invxtx %*% t(xm) %*% xm %*% invxtx
            mcmats[im,im]=hccov[2,2, drop=F];
            dfnum=nrow(coeff)-1;
            dfden=n-dfnum-1;
            fratio=(t(t(lmat) %*% coeff) %*% solve(t(lmat) %*% hccov %*% lmat) %*% (t(lmat) %*% coeff))/dfnum;
            coeff=coeff[1:(nrow(coeff)-clsdmy),1, drop=F];
            mccoeff[im,1]=coeff[2,1, drop=F];
            standerr=sqrt(as.matrix(diag(invxtx %*% t(xm) %*% xm %*% invxtx)));
            standerr=standerr[1:(nrow(standerr)-clsdmy),1, drop=F];
            tratio=coeff/standerr;
            p=2*(1-pt(abs(tratio),(n-ncol(xm))));
            temp=(n-ncol(xm));
            xd=abs(xp2);
            temp=(temp*(exp((temp-(5/6))*((xd/(temp-(2/3)+(.11/temp)))*(xd/(temp-(2/3)+(.11/temp)))))-1));
            temp1=coeff-sqrt(abs(temp))*standerr;
            temp2=coeff+sqrt(abs(temp))*standerr;
            op=cbind(coeff,standerr,tratio,p,temp1,temp2)
            sobel[im,1]=coeff[2,1, drop=F];
            sobel[im,2]=standerr[2,1, drop=F];
            temp=mnames[1,im, drop=F];
            r2full=1-(sse/sstm);
            pfr=1-pf(fratio,dfnum,dfden);
            summ=cbind(sqrt(r2full),r2full,fratio,dfnum,dfden,pfr)
            if (detail == 1)  {
              write_output("*****************************************************************************************", output=medmod_file);
              write_output(paste("Outcome Variable: ",temp), output=medmod_file);
              clnm = matrix(c("R","R-sq","F","df1","df2","p"),nrow=1)
              write_output("Model Summary", output=medmod_file)
              write_output(summ, col.names = clnm, output=medmod_file);
              if (coeffci==0)  {
                op=op[,1:(ncol(op)-2), drop=F];
              }
              clnm = matrix(c("coeff","se","t","p","LLCI","ULCI"), nrow=1)
              write_output("Model", output=medmod_file)
              write_output(op, col.names = clnm, row.names = xmnm, output=medmod_file);
              if (covcoeff==1)  {
                hccovtmp=hccov[1:nrow(op),1:nrow(op), drop=F];
                cnamestp=t(xmnm)
                write_output("Covariance matrix of regression parameter estimates", output=medmod_file)
                write_output(hccovtmp, row.names=xmnm, col.names=cnamestp, output=medmod_file);
              }
              if ((nmods > 0) && (nrow(mintkey) > 1))  {
                write_output("Interactions:", output=medmod_file)
                write_output(mintkey, row.names=FALSE, col.names=FALSE, output=medmod_file);
              }
            }
          }
          ymat[1,im]=coeff[2,1, drop=F];
          if (wm == 1)  {
            ymat[2,im]=coeff[4,1, drop=F];
            if (zm == 1)  {
              ymat[3,im]=coeff[6,1, drop=F];
              if (wzm == 1)  {
                ymat[4,im]=coeff[8,1, drop=F];
              }
            }
          }
          if (model == 6)  {
            mmpaths[(im+1),1]=coeff[(im+1),1, drop=F];
          }
        }
      } 
      
      # estimate model of outcome;
      for (totlp in 1:(1+(toteff*(bt==1)))) {
        #mediators and x on y
        xy=cbind(cons,datayed[,3:ydatacol, drop=F])
        if ((toteff==1) && (totlp == 2))  {
          xy=cbind(cons,datayed[,(3+nmeds):ydatacol, drop=F])
        }
        if (dichy == 1)  {
          meany=datayed[,2, drop=F];
          meany=matrix(colSums(meany),nrow=1)/n
          pt2=matrix(meany,nrow(datayed[,2, drop=F]),1);
          #elementwise
          LL3 = datayed[,2, drop=F] * log(pt2)+(1-datayed[,2, drop=F]) * log(1-pt2)
          LL3 = -2*matrix(colSums(LL3),nrow=1)
          pt1=matrix(0.5,n,1);
          bt1=matrix(0,ncol(xy),1);
          LL1=0;
          LL2=LL3;
          xy22=xy;
          converged = FALSE
          for (jjj in 1:iterate) {
            LL1=LL2;
            for ( ijk in 1 : ncol(xy)) {
              #elementwise
              xy22[,ijk]=xy[,ijk, drop=F] * pt1 * (1-pt1);
            }
            coeff=bt1+solve(t(xy22) %*% xy) %*% t(xy) %*% (datayed[,2, drop=F]-pt1);
            pt1 = 1/(1+exp(-(xy %*% coeff)));
            temp1=((pt1 < .00000000000001) | (pt1 > .9999999999999))
            itprob=matrix(colSums(temp1),nrow=1)
            if (itprob == 0)  {
              LL=datayed[,2, drop=F] * log(pt1)+(1-datayed[,2, drop=F]) * log(1-pt1);
              LL2=-2*matrix(colSums(LL),nrow=1)
            }
            bt1=coeff;
            if (abs(LL1-LL2) < converge) {
              converged = TRUE
              break()
            }
          }
          if ((converged != TRUE) && (iterr == 0))  {
            errs=errs+1
            runerrs[err,1]=22
            iter=1
          }
          for (ijk in 1:ncol(xy)) {
            xy22[,ijk]=xy[,ijk, drop=F] * pt1 * (1-pt1);
          }
          covmat=solve(t(xy22) %*% xy);
          if (totlp != 2)  {
            bootcoef=cbind(bootcoef,t(coeff[1:(nrow(coeff)-clsdmy),1, drop=F]))
          }
        }
        if (dichy == 0)  {
          invxtx=solve(t(xy) %*% xy)
          coeff=invxtx %*% t(xy) %*% datayed[,2, drop=F]
          if (totlp != 2)  {
            bootcoef=cbind(bootcoef,t(coeff[1:(nrow(coeff)-clsdmy),1, drop=F]))
          }
          if ((nmeds == 1) && (ncovs == 0) && (cluster == 0) && (model == 4) && (bt > 1))  {
            resid=datayed[,2, drop=F] - xy %*% coeff
            sse=sum(resid ^ 2)
            r2full=1-(sse/sstot)
          }
          if (bt == 1)  {
            resid=data[,2, drop=F] - xy %*% coeff;
            k3=nrow(coeff)
            sse=sum(resid ^ 2)
            mse=sse/(n-ncol(xy))
            if (hc3 == 1)  {
              h=xy[,1, drop=F]
              for (i3 in 1:n) {
                h[i3,1]=xy[i3,, drop=F] %*% invxtx %*% t(xy[i3,, drop=F]);
              }
              for ( i3 in 1 : k3) {
                xy[,i3]=(resid[,ncol(resid), drop=F]/(1-h))*xy[,i3, drop=F]
              }
            }
            if (hc3 != 1)  {
              for ( i3 in 1 : k3) {
                xy[,i3]=sqrt(mse)*xy[,i3, drop=F];
              }
            }
            covmat=invxtx %*% t(xy) %*% xy %*% invxtx
          }
        }
        if (bt == 1)  {
          if (model==2)  {
            xy2=cbind(cons,datayed[,3:ydatacol, drop=F])
            temp=ncol(xy2)
            if (temp > 6)  {
              xy3=xy2[,7:temp, drop=F]
            }
            xy2=cbind(xy2[,1:3, drop=F],xy2[,5, drop=F])
            if (temp > 6)  {
              xy2=cbind(xy2,xy3)
            }
            invxtx=solve(t(xy2) %*% xy2)
            coeff2=invxtx %*% t(xy2) %*% datayed[,2, drop=F];
            ssem2=sum((datayed[,2, drop=F] - xy2 %*% coeff2)^2)
          }
          standerr=sqrt(as.matrix(diag(covmat)));
          if (totlp==1)  {
            mcmats[(nmeds+1):ncol(mcmats),(nmeds+1):ncol(mcmats)]=covmat[2:(1+nmeds),2:(1+nmeds), drop=F];
          }
          standerr=standerr[1:(nrow(standerr)-clsdmy),1, drop=F];
          coeffplt=coeff;
          lmat=as.matrix(diag(nrow(coeff)))
          lmat=lmat[,2:ncol(lmat), drop=F]
          dfnum=nrow(coeff)-1
          dfden=n-dfnum-1
          fratio=(t(t(lmat) %*% coeff) %*% solve(t(lmat) %*% covmat %*% lmat) %*% (t(lmat) %*% coeff))/dfnum
          coeff=coeff[1:(nrow(coeff)-clsdmy),1, drop=F]
          if (totlp==1)  {
            mccoeff[(nmeds+1):nrow(mccoeff)]=coeff[2:(1+nmeds),1, drop=F];
          }
          bbbb=coeff[2,1, drop=F]
          if (totlp == 1)  {
            deco[1,1]=2+nmeds;
            deco=deco[1:decoc,1, drop=F]
            covdirt=matrix(0,(nrow(covmat)-clsdmy),(ncol(covmat)-clsdmy));
            covdirt=covmat[deco,, drop=F];
            covdir=matrix(0,nrow(covdirt),nrow(covdirt));
            covdir=covdirt[,t(deco), drop=F]
            deco=coeff[deco,1, drop=F];
            if (ttt > 0)  {
              sedir=sqrt(as.matrix(diag(directv %*% covdir %*% t(directv))))
              directv=directv %*% deco;
            }
            sobel[,3]=coeff[2:(1+nmeds),1, drop=F];
            sobel[,4]=standerr[2:(1+nmeds),1, drop=F];
            #elementwise
            sobel2=sobel*sobel;
            if (varorder != 2)  {
              #elementwise
              sobel[,2]=sqrt(sobel2[,1, drop=F]*sobel2[,4, drop=F]+sobel2[,3, drop=F]*sobel2[,2, drop=F]);
            }
            if (varorder == 2)  {
              #elementwise
              sobel[,2]=sqrt(sobel2[,1, drop=F]*sobel2[,4, drop=F]+sobel2[,3, drop=F]*sobel2[,2, drop=F]+sobel2[,2, drop=F]*sobel2[,4, drop=F]);
            }
            #elementwise
            sobel[,1]=sobel[,1, drop=F]*sobel[,3, drop=F];
            sobel[,3]=sobel[,1, drop=F]/sobel[,2, drop=F];
            sobel[,4]=2*(1-pnorm(abs(sobel[,3, drop=F])));
          }
          if (dichy==0)  {
            tratio=coeff/standerr;
            p=2*(1-pt(abs(tratio),(n-ncol(xy))));
            cnms=cbind("coeff","se","t","p","LLCI","ULCI")
            op=cbind(coeff,standerr,tratio,p)
          }
          if(dichy==1) {
            tratio=coeff/standerr;  
            p=2*(1-pnorm(abs(tratio)));
            #elementwise
            wald=tratio*tratio;
            cnms=cbind("coeff","se","Z","p","LLCI","ULCI")
            temp=coeff-abs(xp2)*standerr;
            op=cbind(coeff,standerr,tratio,p,temp)
            temp=coeff+abs(xp2)*standerr;
            op=cbind(op,temp)
          }
          if (detail == 1)  {
            if (totlp==2)  {
              write_output("********************************* TOTAL EFFECT MODEL *********************************", output=medmod_file);
              write_output(paste("Outcome Variable: ",yname), output=medmod_file);
            }
            if (totlp != 2)  {
              write_output("*****************************************************************************************", output=medmod_file);
              write_output(paste("Outcome Variable: ",yname), output=medmod_file);
            }
          }
          if ((dichy==1) && (bt==1) && (totlp==1))  {
            nmsd=cbind(yname,"Analysis")
            write_output("Coding of binary DV for analysis", output=medmod_file)
            write_output(rcd, col.names = nmsd, output=medmod_file);
          }
          if (dichy == 0)  {
            r2full=1-(sse/ssty);
            pfr=1-pf(fratio,dfnum,dfden);
            jndf=dfden;
            xd=abs(xp2);
            jncrit=(dfden*(exp((dfden-(5/6))*((xd/(dfden-(2/3)+(.11/dfden)))*(xd/(dfden-(2/3)+(.11/dfden)))))-1));
            summ=cbind(sqrt(r2full),r2full,fratio,dfnum,dfden,pfr)
            temp1=coeff-sqrt(jncrit)*standerr;
            temp2=coeff+sqrt(jncrit)*standerr;
            op=cbind(coeff,standerr,tratio,p,temp1,temp2)
            if (detail == 1)  {
              clnm = matrix(c("R","R-sq","F","df1","df2","p"), nrow=1)
              write_output("Model Summary", output=medmod_file)
              write_output(summ, col.names = clnm, output=medmod_file);
            }
          }
          if (dichy == 1)  {
            LLdiff=LL3-LL2;
            mcF = LLdiff/LL3;
            cox=1-exp(-LLdiff/n);
            nagel=cox/(1-exp(-(LL3)/n));
            pf=cbind(LL2,LLdiff,mcF,cox,nagel,n)
            if (detail == 1)  {
              clnm = matrix(c("-2LL","Model LL","McFadden","CoxSnell","Nagelkrk","n"),nrow=1)
              write_output("Logistic Regression Summary", output=medmod_file)
              write_output(pf, col.names = clnm, output=medmod_file);
            }
          }
          if (totlp==2)  {
            datanmy=rbind("constant",datanmy[(nmeds+2):nrow(datanmy),1, drop=F])
          }
          if (detail == 1)  {
            if (coeffci == 0)  {
              op=op[,1:(ncol(op)-2), drop=F];
            }
            write_output("Model", output=medmod_file)
            write_output(op, row.names = datanmy, col.names=cnms, output=medmod_file);
            if (covcoeff==1)  {
              covmattp=covmat[1:nrow(op),1:nrow(op), drop=F];
              cnamestp=t(datanmy)
              label="Covariance matrix of regression parameter estimates"
              write_output(covmattp, row.names=datanmy, col.names=cnamestp, output=medmod_file);
            }
          }
          if ((ttt == 0) && (totlp == 1))  {
            deco=op[(nmeds+2),, drop=F];
          }
          if ((ttt == 0) && (totlp == 2))  {
            decotot = op[2,, drop=F];
          }
          if ((nmods > 0) && (model > 4) && (detail == 1) && (nrow(yintkey) > 1))  {
            write_output("Interactions:", output=medmod_file)
            write_output(yintkey, row.names=FALSE, col.names=FALSE, output=medmod_file);
          }
          if ((nmods > 0) && (model < 4) && (detail == 1))  {
            write_output("Interactions:", output=medmod_file)
            write_output(yintkey2, row.names=FALSE, col.names=FALSE, output=medmod_file);
            if (((model == 1) || (model == 2)) && (dichy == 0) && (hc3 == 0))  {
              temp = cbind((((op[4,3, drop=F]*op[4,3, drop=F])*(1-r2full))/dfden),(op[4,3, drop=F]*op[4,3, drop=F]),1,dfden,op[4,4, drop=F])
              rnms=yintkey2[2,1, drop=F];
              if (model == 2)  {
                temp2=cbind((((op[6,3, drop=F]*op[6,3, drop=F])*(1-r2full))/dfden),(op[6,3, drop=F]*op[6,3, drop=F]),1,dfden,op[6,4, drop=F])
                temp = rbind(temp,temp2)
                frat2=(dfden*(r2full-(1-(ssem2/ssty))))/(2*(1-r2full));
                temp2=cbind((r2full-(1-(ssem2/ssty))),frat2,2,dfden,(1-pf(frat2,2,dfden)))
                temp = rbind(temp,temp2)
                rnms=rbind(rnms,yintkey2[3,1, drop=F],"Both")
              }
              clnm = matrix(c("R2-chng","F","df1","df2","p"), nrow=1)
              write_output("R-square increase due to interaction(s):", output=medmod_file)
              write_output(temp, row.names=rnms, col.names=clnm, output=medmod_file);
            }
            if ((model == 3) && (dichy == 0) && (hc3 == 0))  {
              temp = cbind((((op[8,3, drop=F]*op[8,3, drop=F])*(1-r2full))/dfden),(op[8,3, drop=F]*op[8,3, drop=F]),dfden,op[8,4, drop=F]);
              rnms=yintkey2[5,1, drop=F];
              clnm = matrix(c("R2-chng","F(1,df2)","df2","p"),nrow=1)
              write_output("R-square increase due to three-way interaction:", output=medmod_file)
              write_output(temp, row.names = rnms, col.names = clnm, output=medmod_file);
            }
          }
        }
        if ((model == 6) && (totlp == 1))  {
          mmpaths[nrow(mmpaths),1]=coeff[nrow(mmpaths),1, drop=F];
          mmpaths[nrow(mmpaths),(2:(nmeds+1))]=t(coeff[2:(nmeds+1),1, drop=F])
        }
        if (totlp == 1)  {
          for (im in 1:nmeds) {
            if (model < 4)  {
              ymat[1,im]=coeff[3,1]
              ymat[2,im]=coeff[4,1]
              cmat[1,im]=covmat[3,3]
              cmat[2,im]=covmat[4,4]
              cmat[5,im]=covmat[3,4]
              jnb1=coeff[3,1]
              jnb3=coeff[4,1]
              jnsb1=covmat[3,3]
              jnsb3=covmat[4,4]
              jnsb1b3=covmat[3,4]
              if ((model == 2) || (model == 3))  {
                ymat[3,im]=coeff[6,1]
                cmat[3,im]=covmat[6,6]
                cmat[6,im]=covmat[3,6]
                cmat[8,im]=covmat[4,6]
              }
              if (model == 3)  {
                ymat[4,im]=coeff[8,1]
                cmat[4,im]=covmat[8,8]
                cmat[7,im]=covmat[3,8]
                cmat[9,im]=covmat[4,8]
                cmat[10,im]=covmat[6,8]
                jnb1=coeff[4,1]
                jnb3=coeff[8,1]
                jnsb1=covmat[4,4]
                jnsb3=covmat[8,8]
                jnsb1b3=covmat[4,8];
              }
            }
            if (model > 3)  {
              ymat[5,im]=coeff[(1+im),1];
            }
            if (xmy == 1)  {
              ymat[6,im]=coeff[(2+nmeds+im),1];
            }
            if (vy == 1)  {
              ymat[6,im]=coeff[(3+nmeds+im),1];
            }
            if ((qy == 1) && (vy == 1))  {
              ymat[6,im]=coeff[(5+nmeds+((im-1)*2)),1];
              ymat[7,im]=coeff[(6+nmeds+((im-1)*2)),1];
            }
            if (vqy == 1)  {
              ymat[6,im]=coeff[(6+nmeds+((im-1)*3)),1];
              ymat[7,im]=coeff[(7+nmeds+((im-1)*3)),1];
              ymat[8,im]=coeff[(8+nmeds+((im-1)*3)),1];
            }
            if (wmy == 1)  {
              ymat[7,im]=coeff[(3+nmeds+im-wy),1];
            }
            if ((wmy == 1) && (vy == 1))  {
              ymat[7,im]=coeff[(4+(nmeds*2)+im-wy),1];
            }
            if ((wmy == 1) && (vy == 1) && (wvmy == 1))  {
              ymat[7,im]=coeff[(6+(nmeds*2)+((im-1)*2)-wy),1];
              ymat[8,im]=coeff[(7+(nmeds*2)+((im-1)*2)-wy),1];
            }
            if ((wmy == 1) && (zmy == 1))  {
              ymat[6,im]=coeff[((6-(wzy*3)+(wzm-1)+(nmeds*2)+((im-1)*2)*wzm)+((im-1)*(1-wzm))-((zy-wzy)*2)),1];
              ymat[7,im]=coeff[(3-zy+im+nmeds),1];
              if (wzmy == 1)  {
                ymat[8,im]=coeff[(7-(wzy*3)+(nmeds*2)+((im-1)*2)),1];
              }
            }
            if ((nmods > 0) && (model != 5))  {
              for ( indlp in 1 : nrow(modvals)) {
                temp1=ymat[1:4,im, drop=F] * vmat[1:4,indlp, drop=F]
                temp1=matrix(colSums(temp1),nrow=1)
                indeff[indlp,1]=temp1;
                if (model > 6)  {
                  temp2=ymat[5:8,im, drop=F] * vmat[5:8,indlp, drop=F]
                  temp2=matrix(colSums(temp2),nrow=1)
                  indeff[indlp,1]=temp1*temp2;
                }
              }
              indboot[(bt+(im-1)*(boot+1)),]=t(indeff);
              if ((model == 8) || (model == 7))  {
                indbootp[bt,im]=ymat[2,im, drop=F]*ymat[5,im, drop=F];
                if (wdich == 1)  {
                  indbootp[bt,im]=indbootp[bt,im, drop=F]*(cmaxw-cminw);
                }
              }
              if ((model == 14) || (model == 15) || (model == 74))  {
                indbootp[bt,im]=ymat[1,im, drop=F]*ymat[6,im, drop=F];
                if (wvdich == 1)  {
                  indbootp[bt,im]=indbootp[bt,im, drop=F]*(cmaxv-cminv);
                }
              }
              if (model == 12)  {
                indbootp[bt,im]=ymat[4,im, drop=F]*ymat[5,im, drop=F];
              }
              if (((model == 58) || (model == 59)) && (wvdich==1))  {
                indbootp[bt,im]=(ymat[1,im, drop=F]*ymat[7,im, drop=F]*(cmaxw-cminw))+(ymat[2,im, drop=F]*ymat[5,im, drop=F]*(cmaxw-cminw));
                indbootp[bt,im]=indbootp[bt,im, drop=F]+(ymat[2,im, drop=F]*ymat[7,im, drop=F]*((cmaxw*cmaxw)-(cminw*cminw)));
              }
            }
            if ((model == 4) || (model == 5))  {
              temp1=ymat[1:4,im, drop=F] * vmat[1:4,1, drop=F]
              temp1=matrix(colSums(temp1),nrow=1)
              temp2=ymat[5:8,im, drop=F] * vmat[5:8,1, drop=F]
              temp2=matrix(colSums(temp2),nrow=1)
              indboot[bt,im]=temp1*temp2;     
              if ((effsize==1) && (dichy==0) && (ncovs == 0))  {
                if (ctot == 0)  ctot=.00000000000001
                pmeff[bt,(im+1)]=indboot[bt,im, drop=F]/ctot;
                rmeff[bt,(im+1)]=indboot[bt,im, drop=F]/coeff[(2+nmeds),1, drop=F];
                abpseff[bt,(im+1)]=indboot[bt,im, drop=F]/stddevy;
                abcseff[bt,(im+1)]=abpseff[bt,(im+1), drop=F]*stddevx;
                if ((nmeds == 1) && (ncovs == 0) && (cluster == 0) && (model == 4))  {
                  r245[bt,1]=r2my-(r2full-r2xy);
                  abmmr=1;
                  if (indboot[bt,im, drop=F] < 0)  {
                    abmmr=min(abmm);
                  }
                  if (indboot[bt,im, drop=F] > 0)  {
                    abmmr=max(abmm);
                  }
                  kappa2[bt,1]=indboot[bt,im, drop=F]/abmmr;
                  tmp=indboot[bt,im, drop=F]/abmmr;
                }
              }
            }
            if (model == 6)  {
              if (nmeds == 2)  {
                indboot[bt,1]=mmpaths[2,1, drop=F]*mmpaths[4,2, drop=F];
                indboot[bt,2]=mmpaths[2,1, drop=F]*mmpaths[3,2, drop=F]*mmpaths[4,3, drop=F];
                indboot[bt,3]=mmpaths[3,1, drop=F]*mmpaths[4,3, drop=F];
              }
              if (nmeds == 3)  {
                indboot[bt,1]=mmpaths[2,1, drop=F]*mmpaths[5,2, drop=F];
                indboot[bt,2]=mmpaths[2,1, drop=F]*mmpaths[3,2, drop=F]*mmpaths[5,3, drop=F];
                indboot[bt,3]=mmpaths[2,1, drop=F]*mmpaths[4,2, drop=F]*mmpaths[5,4, drop=F];
                indboot[bt,4]=mmpaths[2,1, drop=F]*mmpaths[3,2, drop=F]*mmpaths[4,3, drop=F]*mmpaths[5,4, drop=F];
                indboot[bt,5]=mmpaths[3,1, drop=F]*mmpaths[5,3, drop=F];
                indboot[bt,6]=mmpaths[3,1, drop=F]*mmpaths[4,3, drop=F]*mmpaths[5,4, drop=F];
                indboot[bt,7]=mmpaths[4,1, drop=F]*mmpaths[5,4, drop=F];
              }
              if (nmeds == 4)  {
                indboot[bt,1]=mmpaths[2,1, drop=F]*mmpaths[6,2, drop=F];
                indboot[bt,2]=mmpaths[2,1, drop=F]*mmpaths[3,2, drop=F]*mmpaths[6,3, drop=F];
                indboot[bt,3]=mmpaths[2,1, drop=F]*mmpaths[4,2, drop=F]*mmpaths[6,4, drop=F];
                indboot[bt,4]=mmpaths[2,1, drop=F]*mmpaths[5,2, drop=F]*mmpaths[6,5, drop=F];
                indboot[bt,5]=mmpaths[2,1, drop=F]*mmpaths[3,2, drop=F]*mmpaths[4,3, drop=F]*mmpaths[6,4, drop=F];
                indboot[bt,6]=mmpaths[2,1, drop=F]*mmpaths[3,2, drop=F]*mmpaths[5,3, drop=F]*mmpaths[6,5, drop=F];
                indboot[bt,7]=mmpaths[2,1, drop=F]*mmpaths[4,2, drop=F]*mmpaths[5,4, drop=F]*mmpaths[6,5, drop=F];
                indboot[bt,8]=mmpaths[2,1, drop=F]*mmpaths[3,2, drop=F]*mmpaths[4,3, drop=F]*mmpaths[5,4, drop=F]*mmpaths[6,5, drop=F];
                indboot[bt,9]=mmpaths[3,1, drop=F]*mmpaths[6,3, drop=F];
                indboot[bt,10]=mmpaths[3,1, drop=F]*mmpaths[4,3, drop=F]*mmpaths[6,4, drop=F];
                indboot[bt,11]=mmpaths[3,1, drop=F]*mmpaths[5,3, drop=F]*mmpaths[6,5, drop=F];
                indboot[bt,12]=mmpaths[3,1, drop=F]*mmpaths[4,3, drop=F]*mmpaths[5,4, drop=F]*mmpaths[6,5, drop=F];
                indboot[bt,13]=mmpaths[4,1, drop=F]*mmpaths[6,4, drop=F];
                indboot[bt,14]=mmpaths[4,1, drop=F]*mmpaths[5,4, drop=F]*mmpaths[6,5, drop=F];
                indboot[bt,15]=mmpaths[5,1, drop=F]*mmpaths[6,5, drop=F];
              }
              if ((effsize==1) && (dichy==0) && (ncovs == 0))  {
                if (ctot == 0)  ctot=.00000000000001
                pmeff[bt,]=indboot[bt,, drop=F]/ctot;
                rmeff[bt,]=indboot[bt,, drop=F]/mmpaths[nrow(mmpaths),1, drop=F];
                abpseff[bt,]=indboot[bt,, drop=F]/stddevy;
                abcseff[bt,]=(stddevx*indboot[bt,, drop=F])/stddevy;
                if ((nmeds == 1) && (ncovs == 0) && (cluster == 0) && (model == 4))  {
                  r245[bt,]=r2my-(r2full-r2xy);
                }
              }
            }
          } 
        } 
      }
      if ((savboot == 1) && (bt > 1)) bootstrp[(bt-1),]=bootcoef
    } 
    if (savboot==1)  {
      bootstrp=bootstrp[,2:ncol(bootstrp), drop=F]
      write.csv(x=bootstrp, row.names=FALSE, file= "medmod_bootstrap_results.csv")
    }
    
    if (mc > 0)  {
      x1=sqrt(-2*log(runif(matrix(seed,mc,nrow(mcmats)))))*cos((2*3.14159265358979)*(runif(matrix(seed,mc,nrow(mcmats)))));
      x1=x1*root(mcmats);
      for ( i in 1 : nrow(x1)) {
        x1[i,]=x1[i,, drop=F]+t(mccoeff)
      }
      for (i in 1:nmeds) {
        #elementwise
        indboot[2:nrow(indboot),i]=x1[,i, drop=F] * x1[,(i+nmeds), drop=F];
      }
    }
    if ((ttt == 0) && (model > 3))  {
      if (toteff == 0)  {
        write_output("****************************** DIRECT AND INDIRECT EFFECTS *******************************", output=medmod_file);
      }
      if (toteff != 0)  {
        write_output("*************************** TOTAL, DIRECT AND INDIRECT EFFECTS ***************************", output=medmod_file);
      }
      if (model != 74)  {
        if (dichy == 0)  {
          clnm = c("Effect","SE","t","p","LLCI","ULCI")
          if (toteff == 1)  {
            write_output("Total effect of X on Y", output=medmod_file)
            write_output(decotot, col.names = clnm, output=medmod_file);
          }
          write_output("Direct effect of X on Y", output=medmod_file)
          write_output(deco, col.names = clnm, output=medmod_file);
        }
        if (dichy == 1)  {
          clnm = c("Effect","SE","Z","p","LLCI","ULCI")
          if (toteff == 1)  {
            write_output("Total effect of X on Y", output=medmod_file)
            write_output(decotot, col.names = clnm, output=medmod_file);
          }
          write_output("Direct effect of X on Y", output=medmod_file)
          write_output(deco, col.names = clnm, output=medmod_file);
        }
      }
    }
    if (ttt > 0)  {
      write_output("****************************** DIRECT AND INDIRECT EFFECTS *******************************", output=medmod_file);
      clbs=cbind(modvnm2,"Effect","SE","t","p","LLCI","ULCI")
      tratio=directv/sedir;
      p=2*(1-pt(abs(tratio),(n-ncol(xy))));
      outp=cbind(modvalsd,directv,sedir,tratio,p)
      if (dichy == 0)  {
        temp1=directv-sqrt(jncrit)*sedir;
        temp2=directv+sqrt(jncrit)*sedir;
        outp=cbind(outp,temp1,temp2)
      }
      if (dichy == 1)  {
        p=2*(1-pnorm(abs(tratio)));
        temp=directv-abs(xp2)*sedir;
        outp=cbind(outp,temp)
        temp=directv+abs(xp2)*sedir;
        outp=cbind(outp,temp)
        clbs=cbind(modvnm2,"Effect","SE","Z","p","LLCI","ULCI")
      }
      if (coeffci==0)  {
        outp=outp[,1:(ncol(outp)-2), drop=F];
      }
      write_output("Conditional direct effect(s) of X on Y at values of the moderator(s)", output=medmod_file)
      write_output(outp,  col.names = clbs, output=medmod_file);
    }
    if ((nmods > 0) && (model != 5))  {
      if (model < 4)  {
        write_output("*****************************************************************************************", output=medmod_file);  
        zmat[1,1]=1;
        cfse=matrix(0,nrow(modvals),1);
        for (mmm in 1:nrow(modvals)) {
          if (model == 1)  {
            zmat[2,1]=modvals[mmm,1, drop=F]^2;
            zmat[5,1]=2*modvals[mmm,1, drop=F];
          }
          if ((model == 2) || (model == 3))  {
            zmat[2,1]=modvals[mmm,2, drop=F]^2;
            zmat[3,1]=modvals[mmm,1, drop=F]^2;
            zmat[4,1]=(modvals[mmm,1, drop=F]^2)*(modvals[mmm,2, drop=F]^2);
            zmat[5,1]=2*modvals[mmm,2, drop=F];
            zmat[6,1]=2*modvals[mmm,1, drop=F];
            zmat[7,1]=2*modvals[mmm,1, drop=F]*modvals[mmm,2, drop=F];
            zmat[8,1]=2*modvals[mmm,1, drop=F]*modvals[mmm,2, drop=F];
            zmat[9,1]=2*modvals[mmm,1, drop=F]*(modvals[mmm,2, drop=F]^2);
            zmat[10,1]=2*(modvals[mmm,1, drop=F]^2)*modvals[mmm,2, drop=F];
          }
          temp=zmat * cmat
          cfse[mmm,1]=sqrt(matrix(colSums(temp),nrow=1))
        }
      }
      if (nmods > 0)  { 
        clbs=cbind(modvnm,"Effect")
        for (im in 1:nmeds) {
          obs=t(indboot[(1+(im-1)*(boot+1)),, drop=F])
          outp=cbind(modvals,obs)
          if (model < 4)  {
            tstat=obs/cfse;
            if (dichy==0)  {
              pval=2*(1-pt(abs(tstat),(n-ncol(xy))));
              temp=obs-sqrt(jncrit)*cfse;
              outp=cbind(outp,cfse,tstat,pval,temp)
              temp=obs+sqrt(jncrit)*cfse;
              outp=cbind(outp,temp)
              clbs = cbind(clbs,"se","t","p","LLCI","ULCI")
              jnclbs=clbs;
            }
            if (dichy==1)  {
              pval=2*(1-pnorm(abs(tstat)));
              temp=obs-abs(xp2)*cfse;
              outp=cbind(outp,cfse,tstat,pval,temp)
              temp=obs+abs(xp2)*cfse;
              outp=cbind(outp,temp)
              clbs=cbind(clbs,"se","Z","p","LLCI","ULCI")
              jnclbs=clbs;
            }
          }
          if (boot > 0)  {
            ones=matrix(1,boot,1);
            estmte=indboot[(1+(im-1)*(boot+1)),, drop=F];
            indboot2=indboot[(2+(im-1)*(boot+1)):(1+(im-1)*(boot+1)+boot),, drop=F];
            mnind=matrix(colSums(indboot2),nrow=1)/boot
            mnind=t(mnind)
            tmp=indboot2 ^ 2
            tmp=matrix(colSums(tmp),nrow=1)
            stdind=t(sqrt((tmp-((matrix(colSums(indboot2),nrow=1) ^2)/boot))/(boot-1)));
            llci=matrix(-999,1,ncol(indboot2));
            ulci=matrix(-999,1,ncol(indboot2));
            for (eee in 1:ncol(indboot2)) {
              inpt=indboot2[,eee, drop=F]
              inpt2=(estmte[1,eee, drop=F]*bconoff)+(9999*(1-bconoff));
              bcboot_res = bcboot(databcbt=inpt,estmte=inpt2)
              llcit = bcboot_res[[1]]
              ulcit = bcboot_res[[2]]
              llci[1,eee]=llcit;
              ulci[1,eee]=ulcit;
              if ((badlo==1) && (llcit != priorlo))  {
                badend=cbind(badend,llcit)
                priorlo=llcit;
              }
              if ((badhi==1) && (ulcit != priorhi))  {
                badend=cbind(badend,ulcit)
                priorhi=ulcit;
              }
            }
            outp=cbind(modvals,obs,stdind,t(llci),t(ulci))
            clbs=cbind(modvnm,"Effect","Boot SE","BootLLCI","BootULCI")
          }
          mtemp=mnames[1,im, drop=F];
          rlbs=matrix(mnames[1,im, drop=F],nrow(modvals),1);
          if (model < 4)  {
            if (coeffci == 0)  {
              outp=outp[,1:(ncol(outp)-2), drop=F];
            }
            write_output("Conditional effect of X on Y at values of the moderator(s)", output=medmod_file)
            write_output(outp, col.names = clbs, output=medmod_file);
          }
          if ((model > 5) && (mod74dic != 1))  {
            if (im == 1)  {
              write_output("Conditional indirect effect(s) of X on Y at values of the moderator(s)", output=medmod_file);
            }
            write_output(outp, row.names = rlbs, col.names = clbs, output=medmod_file);
          }
          if ((model == 74) && (mod74dic == 1))  {
            if (im == 1)  {
              write_output("Indirect effect(s) of X on Y:", output=medmod_file)
              clbs3=clbs[1,2:ncol(clbs), drop=F];
            }
            outp3=outp[1,2:ncol(outp), drop=F];
            write_output(outp3, row.names = rlbs, col.names=clbs3, output=medmod_file);
          }
        }
        for (i in notes:1) {
          if (note[i,1]==4)  {
            write_output("Values for quantitative moderators are the 10th, 25th, 50th, 75th, and 90th percentiles.", output=medmod_file);
            write_output("Values for dichotomous moderators are the two values of the moderator.", output=medmod_file);
          }
          if (note[i,1]==5)  {
            write_output("Values for quantitative moderators are the mean and plus/minus one SD from mean.", output=medmod_file);
            write_output("Values for dichotomous moderators are the two values of the moderator.", output=medmod_file);
          }
          if (note[i,1]==14)  {
            write_output("NOTE: Too low.", output=medmod_file);
          }
          if (note[i,1]==15)  {
            write_output("NOTE: Too high.", output=medmod_file);
          }
        }
        if (model == 3)  {
          jnvals=matrix(0,nrow(matw),7);
          jnvals[,1]=matw;
          jnvals[,2]=jnb1+jnb3*jnvals[,1, drop=F];
          jnvals[,3]=sqrt(jnsb1+2*jnvals[,1, drop=F]*jnsb1b3+(jnvals[,1, drop=F] ^2)*jnsb3);
          jnvals[,4]=jnvals[,2, drop=F]/jnvals[,3, drop=F];
          if (dichy==0)  {
            jnvals[,5]=2*(1-pt(abs(jnvals[,4, drop=F]),jndf));
          }
          if (dichy==1)  {
            jnvals[,5]=2*(1-pnorm(abs(jnvals[,4, drop=F])));
          }
          #elementwise
          jnvals[,6]=jnvals[,2, drop=F]-sqrt(jncrit)*jnvals[,3, drop=F];
          jnvals[,7]=jnvals[,2, drop=F]+sqrt(jncrit)*jnvals[,3, drop=F];
          clbs=cbind(clbs[,1, drop=F],clbs[,3:ncol(clbs), drop=F])
          if (coeffci == 0)  {
            jnvals=jnvals[,1:(ncol(jnvals)-2), drop=F];
          }
          write_output("Conditional effect of X*M interaction at values of W", output=medmod_file)
          write_output(jnvals, col.names = clbs, output=medmod_file);
        }
        if ((jn == 1) && ((model == 1) || (model == 3)) && (jndich==0))  {
          ajn=(jncrit*jnsb3)-(jnb3*jnb3)
          bjn=2*((jncrit*jnsb1b3)-(jnb1*jnb3))
          cjn=(jncrit*jnsb1)-(jnb1*jnb1)
          radarg=(bjn*bjn)-(4*ajn*cjn)
          den=2*ajn
          nrts=0
          write_output("******************************** JOHNSON-NEYMAN TECHNIQUE ********************************", output=medmod_file);
          if ((radarg >= 0) && (den != 0))  {
            x21=(-bjn+sqrt(radarg))/den;
            x22=(-bjn-sqrt(radarg))/den;
            roots = 0;
            if ((x21 >= jnmin) && (x21 <= jnmax))  {
              nrts=1
              roots=rbind(roots,x21)
            }
            if ((x22 >= jnmin) && (x22 <= jnmax))  {
              nrts=nrts+1
              roots=rbind(roots,x22)
            }
            roots=cbind(as.matrix(roots),matrix(0,nrow(as.matrix(roots)),2))
            modtemp=m;
            if (model==3)  modtemp=w
            if (nrts > 0)  {
              roots = roots[2:nrow(roots),1:3, drop=F];
              rootsum=(modtemp < roots[1,1]);
              roots[1,2]=(sum(rootsum)/n)*100;
              rootsum=(modtemp > roots[1,1]);
              roots[1,3]=(sum(rootsum)/n)*100;
              if (nrow(roots)==2)  {
                rootsum=(modtemp < roots[2,1]);
                roots[2,2]=(sum(rootsum)/n)*100;
                rootsum=(modtemp > roots[2,1]);
                roots[2,3]=(sum(rootsum)/n)*100;
              }
              lohilbs=cbind("Value","% below","% above")
              write_output("Moderator values(s) defining Johnson-Neyman significance region(s)", output=medmod_file)
              write_output(roots, col.names=lohilbs, output=medmod_file);
              jnvals=matrix(0,(21+nrts),7);
              for (i in 0:20) {
                jnvals[(i+1),1]=jnmin+(i*((jnmax-jnmin)/20));
              }
              for (i in 1:nrts) {
                for ( j in 2 : nrow(jnvals)) {
                  if ((roots[i,1, drop=F] > jnvals[(j-1),1, drop=F]) && (roots[i,1, drop=F] < jnvals[j,1, drop=F]))  {
                    jnvals[(j+1):(21+i),1]=jnvals[j:(20+i),1, drop=F];
                    jnvals[j,1]=roots[i,1, drop=F];
                  }
                }
              }
              for ( i in 1 : nrow(jnvals)) {
                jnvals[i,2]=jnb1+jnb3*jnvals[i,1, drop=F];
                jnvals[i,3]=sqrt(jnsb1+2*jnvals[i,1, drop=F]*jnsb1b3+(jnvals[i,1, drop=F]^2)*jnsb3);
                jnvals[i,4]=jnvals[i,2, drop=F]/jnvals[i,3, drop=F];
                if (dichy == 0)  {
                  jnvals[i,5]=2*(1-pt(abs(jnvals[i,4, drop=F]),jndf));
                }
                if (dichy == 1)  {
                  jnvals[i,5]=2*(1-pnorm(abs(jnvals[i,4, drop=F])));
                }
                jnvals[i,6]=jnvals[i,2, drop=F]-sqrt(jncrit)*jnvals[i,3, drop=F];
                jnvals[i,7]=jnvals[i,2, drop=F]+sqrt(jncrit)*jnvals[i,3, drop=F];
              }
              if (model == 1)  {
                write_output("Conditional effect of X on Y at values of the moderator (M)", output=medmod_file)
                write_output(jnvals,  col.names=jnclbs, output=medmod_file)
              }
              if (model == 3)  {
                jnclbs=cbind(jnclbs[,1, drop=F],jnclbs[,3:ncol(jnclbs), drop=F])
                write_output("Conditional effect of X*M on Y at values of the moderator (W)", output=medmod_file)
                write_output(jnvals,  col.names=jnclbs, output=medmod_file);
              }
            }
          }
          if (nrts == 0)  {
            write_output("There are no statistical significance transition points within the observed range of the moderator", output=medmod_file);
          }
        }
      } 
      if ((model < 4) && (plot == 1))  {
        dataplot=matrix(0,(nrow(modvals)*nrow(matx)),(ncol(modvals)+1));
        tmp=1;
        for ( i in 1 : nrow(modvals)) {
          for ( j in 1 : nrow(matx)) {
            dataplot[tmp,]=cbind(matx[j,1, drop=F],modvals[i,, drop=F])
            tmp=tmp+1;
          }
        }
        dataplot=cbind(dataplot,matrix(0,nrow(dataplot),(1+dichy)))
        dataplo2=matrix(1,nrow(dataplot),1);
        if (model == 1)  {
          #elementwise
          dataplo2=cbind(dataplo2,dataplot[,2, drop=F],dataplot[,1, drop=F],(dataplot[,1, drop=F]*dataplot[,2, drop=F]))
        }
        if ((model == 2) || (model == 3))  {
          #elementwise
          dataplo2=cbind(dataplo2,dataplot[,3, drop=F],dataplot[,1, drop=F],(dataplot[,1, drop=F]*dataplot[,3, drop=F]),dataplot[,2, drop=F],(dataplot[,1, drop=F]*dataplot[,2, drop=F]))
          if (model == 3)  {
            #elementwise
            dataplo2=cbind(dataplo2,(dataplot[,2, drop=F]*dataplot[,3, drop=F]),(dataplot[,1, drop=F]*dataplot[,2, drop=F]*dataplot[,3, drop=F]))
          }
        }
        for ( i in 1 : nrow(dataplot)) {
          tmp=dataplo2[i,, drop=F];
          if (ncovs > 0)  {
            tmp=cbind(tmp,covmeans)
          }
          if (cluster > 0)  {
            tmp=cbind(tmp,cldmeans)
          }
          dataplot[i,(ncol(dataplot)-(dichy))]=tmp %*% coeffplt;
          if (dichy==1)  {
            dataplot[i,(ncol(dataplot))]=exp(tmp %*% coeffplt)/(1+exp(tmp %*% coeffplt));
          }
        }
        clbs=cbind(xname,modvnm,"yhat")
        if (dichy == 1)  {
          clbs=cbind(xname,modvnm,"ln(odds)","prob")
        }
        write_output("*****************************************************************************************", output=medmod_file); 
        write_output("Data for visualizing conditional effect of X on Y", output=medmod_file)
        write_output(dataplot, col.names = clbs, output=medmod_file);
        if (ncovs > 0)  {
          write_output("Estimates in this table are based on setting covariates to their sample means", output=medmod_file);
        }
      }
    }
    if (((model == 8) || (model == 12) || (model == 7) || (model==14) || (model==15) || ((model==74) && (mod74dic==0))) || (((model == 58) || (model == 59)) && (wvdich == 1)))  {
      obsprod=t(indbootp[1,, drop=F])
      if (boot > 0)  {
        ones=matrix(1,boot,1);
        estmte=indbootp[1,, drop=F];
        indbootp=indbootp[2:(boot+1),, drop=F];
        mnindp=matrix(colSums(indbootp),nrow=1)/boot
        mnindp=t(mnindp)
        tmp=matrix(colSums(indbootp^2),nrow=1)
        stdindp=t(sqrt((tmp-((matrix(colSums(indbootp),nrow=1)^2)/boot))/(boot-1)))
        llcip=matrix(-999,1,ncol(indbootp));
        ulcip=matrix(-999,1,ncol(indbootp));
        for (eee in 1:ncol(indbootp)) {
          inpt=indbootp[,eee, drop=F]
          inpt2=(estmte[1,eee, drop=F]*bconoff)+(9999*(1-bconoff));
          bcboot_res = bcboot(databcbt=inpt,estmte=inpt2)
          llcit = bcboot_res[[1]]
          ulcit = bcboot_res[[2]]
          llcip[1,eee]=llcit;
          ulcip[1,eee]=ulcit;
          if ((badlo==1) && (llcit != priorlo))  {
            badend=cbind(badend,llcit)
            priorlo=llcit;
          }
          if ((badhi==1) && (ulcit != priorhi))  {
            badend=cbind(badend,ulcit)
            priorhi=ulcit;
          }
        }
        outp=cbind(obsprod,stdindp,t(llcip),t(ulcip))
        clbs=cbind("Effect","Boot SE","BootLLCI","BootULCI")
        if ((model==8) || (model==12))  {
          write_output("Indirect effect of highest order interaction", output=medmod_file)
          write_output(outp, col.names = clbs, row.names = mnames, output=medmod_file);
        }
        if (model != 12)  {
          write_output("****************************** INDEX OF MODERATED MEDIATION ********************************", output=medmod_file);
          clbs=cbind("Index","Boot SE","BootLLCI","BootULCI")
          write_output(outp, col.names = clbs, row.names= mnames, output=medmod_file);
          if (wvdich==1)  {
            write_output("When the moderator is dichotomous, this is a test of equality of the indirect effects in the two groups", output=medmod_file);
          }
        }
      }
      if (boot == 0)  {
        if ((model == 8) || (model == 12))  {
          
          clnm43="Effect";
          write_output("Indirect effect of highest order interaction", output=medmod_file)
          write_output(obsprod, col.names = clnm43, row.names = mnames, output=medmod_file);
        }
        if (model != 12)  {
          write_output("******************************INDEX OF MODERATED MEDIATION ********************************", output=medmod_file);
          clnm43="Index";
          write_output(obsprod, col.names = clnm43, row.names = mnames, output=medmod_file);
        }
      }
    }
    conmake=0
    concols=0;
    if (((model > 3) && (model < 7)) && (contrast==1) && (nmods == 0) && (nmeds > 1))  {
      concols=(ncol(indboot)*(ncol(indboot)-1))/2;
      indcon=matrix(-999,nrow(indboot),concols);
      conkey=cbind(" "," "," ")
      temp=1;
      conmake=1;
      for (i in 1:(ncol(indboot)-1)) {
        for (j in (i+1):(ncol(indboot))) {
          indcon[,temp]=indboot[,i, drop=F]-indboot[,j, drop=F];
          if (model != 6)  {
            conkeyt=cbind(mnames[1,i, drop=F],"minus",mnames[1,j, drop=F])
            conkey=rbind(conkey,conkeyt)
          }
          if (model == 6)  {
            conkeyt=cbind(indlbl2[i,1, drop=F],"minus",indlbl2[j,1, drop=F])
            conkey=rbind(conkey,conkeyt)
          }
          temp=temp+1;
        }
      }
    }
    if ((model == 4 ) || (model == 5))  {
      clbs="Effect";
      rlbs=rbind("TOTAL",t(mnames))
      obs=t(indboot[1,, drop=F])
      obs=rbind(matrix(colSums(obs),nrow=1),obs)
      if (conmake==1)  {
        obs=rbind(obs,t(indcon[1,, drop=F]))
        rlbs=rbind(rlbs,cntname[1:ncol(indcon),1, drop=F])
      }
      outp=obs;
      outp2=outp;
      if ((effsize==1) && (dichy==0) && (ncovs == 0))  {
        tmp=pmeff[,2:ncol(pmeff), drop=F]
        pmeff[,1]=as.matrix(rowSums(tmp))
        tmp=rmeff[,2:ncol(rmeff), drop=F]
        rmeff[,1]=as.matrix(rowSums(tmp))
        tmp=abpseff[,2:ncol(abpseff), drop=F]
        abpseff[,1]=as.matrix(rowSums(tmp))
        tmp=abcseff[,2:ncol(abcseff), drop=F]
        abcseff[,1]=as.matrix(rowSums(tmp))
        eff=cbind(pmeff,rmeff,abpseff,abcseff)
        if ((nmeds==1) && (ncovs == 0) && (cluster == 0) && (model == 4))  {
          eff=cbind(eff,r245,kappa2)
          r245obs=rbind(r245[1,1, drop=F],r245[1,1, drop=F])
          kappa2ob=kappa2[1,1, drop=F];
        }
        pmobs=t(pmeff[1,1:(nmeds+1), drop=F]);
        rmobs=t(rmeff[1,1:(nmeds+1), drop=F]);
        psobs=t(abpseff[1,1:(nmeds+1), drop=F]);
        csobs=t(abcseff[1,1:(nmeds+1), drop=F]);
        if (contrast == 0)  {
          outp2=cbind(obs,psobs,csobs,pmobs,rmobs)
        }
        if (contrast == 1)  {
          obs2=obs[1:nrow(psobs),, drop=F];
          outp2=cbind(obs2,psobs,csobs,pmobs,rmobs)
        }
        clbs=cbind("ab","ab_ps","ab_cs","ab/c","ab/c'")
        if ((nmeds == 1) && (ncovs == 0) && (cluster == 0) && (model == 4))  {
          outp2=cbind(outp2,r245obs,(obs/abmmr))
          clbs=cbind(clbs,"R-sq_med","kappa2")
        }
      }
      if ((boot == 0) && (mc == 0))  {
        if (nmeds == 1)  {
          outp2=outp2[2,, drop=F];
          rlbs=rlbs[2,1, drop=F];
        }
        write_output("Indirect effect(s) of X on Y", output=medmod_file)
        write_output(outp2, row.names = rlbs, col.names = clbs, output=medmod_file);
        if ((contrast==1) && (effsize==1) && (nmeds > 1))  {
          outp2=t(indcon[1,, drop=F])
          rlbs2=cntname[1:ncol(indcon),1, drop=F];
          write_output("Contrast(s) between indirect effects", output=medmod_file)
          write_output(outp2, row.names = rlbs2, col.names = clbs, output=medmod_file);
        }
      }
      if ((boot > 0) || (mc > 0))  {
        temp=as.matrix(rowSums(indboot))
        indboot=cbind(temp,indboot)
        bootsz=boot;
        if (mc > 0)  bootsz=mc
        ones=matrix(1,bootsz,1);
        if (conmake==1)  {
          indboot=cbind(indboot,indcon)
        }
        estmte=indboot[1,, drop=F];
        indboot=indboot[2:(bootsz+1),, drop=F];
        mnind=t(matrix(colSums(indboot),nrow=1)/bootsz)
        tmp=matrix(colSums(indboot^2),nrow=1)
        stdind=t(sqrt((tmp-((matrix(colSums(indboot),nrow=1)^2)/bootsz))/(bootsz-1)));
        llci=matrix(-999,1,ncol(indboot));
        ulci=matrix(-999,1,ncol(indboot));
        for ( eee in 1 : ncol(indboot)) {
          inpt=indboot[,eee, drop=F]
          inpt2=(estmte[1,eee, drop=F]*bconoff)+(9999*(1-bconoff));
          bcboot_res = bcboot(databcbt=inpt,estmte=inpt2)
          llcit = bcboot_res[[1]]
          ulcit = bcboot_res[[2]]
          llci[1,eee]=llcit;
          ulci[1,eee]=ulcit;
          if ((badlo==1) && (llcit != priorlo))  {
            badend=cbind(badend,llcit)
            priorlo=llcit;
          }
          if ((badhi==1) && (ulcit != priorhi))  {
            badend=cbind(badend,ulcit)
            priorhi=ulcit;
          }
        }
        if ((effsize==1) && (dichy==0) && (ncovs == 0))  {
          estmte=eff[1,, drop=F];
          eff=eff[2:nrow(eff),, drop=F];
          tmp=matrix(colSums(eff^2),nrow=1)
          stdindf=t(sqrt((tmp-((matrix(colSums(eff),nrow=1)^2)/boot))/(boot-1)))
          llcif=matrix(-999,1,ncol(eff));
          ulcif=matrix(-999,1,ncol(eff));
          for (eee in 1 : ncol(eff)) {
            inpt=eff[,eee, drop=F]
            inpt2=(estmte[1,eee, drop=F]*bconoff)+(9999*(1-bconoff));
            bcboot_res = bcboot(databcbt=inpt,estmte=inpt2)
            llcit = bcboot_res[[1]]
            ulcit = bcboot_res[[2]]
            llcif[1,eee]=llcit;
            ulcif[1,eee]=ulcit;
            if ((badlo==1) && (llcit != priorlo))  {
              badend=cbind(badend,llcit)
              priorlo=llcit;
            }
            if ((badhi==1) && (ulcit != priorhi))  {
              badend=cbind(badend,ulcit)
              priorhi=ulcit;
            }
          }
        }
      }
      if ((boot > 0) || (mc > 0))  {
        outp=cbind(obs,stdind,t(llci),t(ulci))
        if (nmeds == 1)  {
          outp=outp[2,, drop=F]
          rlbs=rlbs[2,1, drop=F];
        }
        clbs=cbind("Effect","Boot SE","BootLLCI","BootULCI")
        if (mc > 0)  {
          clbs=cbind("Effect","MC SE","MC LLCI","MC ULCI")
        }
        write_output("Indirect effect of X on Y", output=medmod_file)
        write_output(outp, row.names = rlbs, col.names = clbs, output=medmod_file);
        if ((dichy==0) && (effsize == 1) && (ncovs == 0))  {
          outp=cbind(psobs,stdindf[(2*(nmeds+1)+1):(3*(nmeds+1)),1, drop=F],t(llcif[1,(2*(nmeds+1)+1):(3*(nmeds+1)), drop=F]),t(ulcif[1,(2*(nmeds+1)+1):(3*(nmeds+1)), drop=F]))
          if (nmeds == 1)  {
            outp=outp[2,, drop=F];
          }
          write_output("Partially standardized indirect effect of X on Y", output=medmod_file)
          write_output(outp, row.names = rlbs, col.names = clbs, output=medmod_file);
          outp=cbind(csobs,stdindf[(3*(nmeds+1)+1):(4*(nmeds+1)),1, drop=F],t(llcif[1,(3*(nmeds+1)+1):(4*(nmeds+1)), drop=F]),t(ulcif[1,(3*(nmeds+1)+1):(4*(nmeds+1)), drop=F]))
          if (nmeds == 1)  {
            outp=outp[2,, drop=F];
          }    
          write_output("Completely standardized indirect effect of X on Y", output=medmod_file)
          write_output(outp, row.names = rlbs, col.names = clbs, output=medmod_file);
          outp=cbind(pmobs,stdindf[1:(nmeds+1),1, drop=F],t(llcif[1,1:(nmeds+1), drop=F]),t(ulcif[1,1:(nmeds+1), drop=F]))
          if (nmeds == 1)  {
            outp=outp[2,, drop=F];
          }    
          write_output("Ratio of indirect to total effect of X on Y", output=medmod_file)
          write_output(outp, row.names = rlbs, col.names = clbs, output=medmod_file);
          outp=cbind(rmobs,stdindf[((nmeds+1)+1):(2*(nmeds+1)),1, drop=F],t(llcif[1,((nmeds+1)+1):(2*(nmeds+1)), drop=F]),t(ulcif[1,((nmeds+1)+1):(2*(nmeds+1)), drop=F]))
          if (nmeds == 1)  {
            outp=outp[2,, drop=F];
          }    
          write_output("Ratio of indirect to direct effect of X on Y", output=medmod_file)
          write_output(outp, row.names = rlbs, col.names = clbs, output=medmod_file);
          if ((nmeds == 1) && (cluster == 0) && (ncovs == 0) && (model == 4))  {
            r245obs=r245obs[1,1, drop=F];
            outp=cbind(r245obs,stdindf[(4*(nmeds+1)+1):(4*(nmeds+1)+1),1, drop=F],t(llcif[1,(4*(nmeds+1)+1):(4*(nmeds+1)+1), drop=F]),t(ulcif[1,(4*(nmeds+1)+1):(4*(nmeds+1)+1), drop=F]))
            write_output("R-squared mediation effect size", output=medmod_file)
            write_output(outp, row.names = rlbs, col.names = clbs, output=medmod_file);
            outp=cbind(kappa2ob,stdindf[nrow(stdindf),1, drop=F],llcif[1,ncol(llcif), drop=F],ulcif[1,ncol(ulcif), drop=F])
            write_output("Preacher and Kelley (2011) Kappa-squared", output=medmod_file)
            write_output(outp, row.names = rlbs, col.names = clbs, output=medmod_file);
          }
        }
      }
      if (normal == 1)  {
        clbs2=c("Effect","se","Z","p")
        if (nmeds == 1)  {
          write_output("Normal theory test for indirect effect", output=medmod_file)
          write_output(sobel, col.names = clbs2, output=medmod_file);
        }
        if (nmeds > 1)  {
          rlbs2=rlbs[2:nrow(rlbs),1, drop=F];
          if (conmake == 1) {
            rlbs2=rlbs[2:(nmeds+1),1, drop=F];
          }
          write_output("Normal theory tests for specific indirect effects", output=medmod_file)
          write_output(sobel, row.names = rlbs2, col.names = clbs2, output=medmod_file);
        }
      }
      if (conmake == 1)  {
        conkey=conkey[2:nrow(conkey),, drop=F];
        conlbs=cntname[1:ncol(indcon),1, drop=F];
        write_output("Specific indirect effect contrast definitions", output=medmod_file)
        write_output(conkey, row.names = conlbs, col.names=FALSE, output=medmod_file);
      }
    } 
    if (model == 6)  {
      clbs="Effect"
      rlbs=rbind("TOTAL",t(mnames))
      obs=t(indboot[1,, drop=F])
      obs=rbind(matrix(colSums(obs),nrow=1),obs)
      indlbl=indlbl[1:nrow(obs),1, drop=F];
      if (conmake == 1)  {
        obs=rbind(obs,t(indcon[1,, drop=F]))
        indlbl=rbind(indlbl,cntname[1:ncol(indcon),1, drop=F])
      }
      obs2=obs;
      if (boot == 0)  {
        if ((dichy==0) && (effsize == 1) && (ncovs==0))  {
          obs=cbind(obs,obs/stddevy,obs*stddevx/stddevy,obs/ctot,obs/mmpaths[nrow(mmpaths),1, drop=F])
          clbs=cbind("eff","eff_ps","eff_cs","eff/c","eff/c'")
          obs2=obs;
          if (contrast==1)  {
            obs2=obs[1:(nrow(obs)-concols),, drop=F];
          }
        }
        write_output("Indirect effect(s) of X on Y", output=medmod_file)
        write_output(obs2, row.names = indlbl, col.names = clbs, output=medmod_file);
        if ((contrast==1) && (effsize==1))  {
          outp2=t(indcon[1,, drop=F]);
          rlbs2=cntname[1:ncol(indcon),1, drop=F];
          write_output("Contrast(s) between indirect effects", output=medmod_file)
          write_output(outp2, row.names=rlbs2, col.names=clbs, output=medmod_file);
        }
      }
      if (boot > 0)  {
        ones=matrix(1,boot,1);
        indboot=cbind(as.matrix(rowSums(indboot)),indboot)
        if (conmake==1)  {
          indboot=cbind(indboot,indcon)
        }
        estmte=indboot[1,, drop=F];
        mnind=matrix(colSums(indboot),nrow=1)/boot
        mnind=t(mnind)
        tmp=matrix(colSums(indboot^2),nrow=1)
        stdind=t(sqrt((tmp-((matrix(colSums(indboot),nrow=1)^2)/boot))/(boot-1)))
        temp=nrow(indboot);
        llci=matrix(-999,1,ncol(indboot));
        ulci=matrix(-999,1,ncol(indboot));
        for ( eee in 1 : ncol(indboot)) {
          inpt=indboot[,eee, drop=F]
          inpt2=(estmte[1,eee, drop=F]*bconoff)+(9999*(1-bconoff));
          bcboot_res = bcboot(databcbt=inpt,estmte=inpt2)
          llcit = bcboot_res[[1]]
          ulcit = bcboot_res[[2]]
          llci[1,eee]=llcit;
          ulci[1,eee]=ulcit;
          if ((badlo==1) && (llcit != priorlo))  {
            badend=cbind(badend,llcit)
            priorlo=llcit;
          }
          if ((badhi==1) && (ulcit != priorhi))  {
            badend=cbind(badend,ulcit)
            priorhi=ulcit;
          }
        }
        obs=cbind(obs,stdind,t(llci),t(ulci))
        clbs=cbind("Effect","Boot SE","BootLLCI","BootULCI")
        write_output("Indirect effect(s) of X on Y", output=medmod_file)
        write_output(obs, row.names = indlbl, col.names = clbs, output=medmod_file);
        if ((effsize==1) && (dichy==0) && (ncovs==0))  {
          indboot=indboot[,1:(ncol(indboot)-concols), drop=F];
          eff=cbind(as.matrix(rowSums(abpseff)),abpseff,as.matrix(rowSums(abcseff)),abcseff,as.matrix(rowSums(pmeff)),pmeff,as.matrix(rowSums(rmeff)),rmeff)
          effobs=eff[1,, drop=F];
          tmp=matrix(colSums(eff^2),nrow=1)
          stdindf=t(sqrt((tmp-((matrix(colSums(eff),nrow=1)^2)/boot))/(boot-1)));
          llcif=matrix(-999,1,ncol(eff));
          ulcif=matrix(-999,1,ncol(eff));
          for (eee in 1:ncol(eff)) {
            inpt=eff[,eee, drop=F]
            inpt2=(effobs[1,eee, drop=F]*bconoff)+(9999*(1-bconoff));
            bcboot_res = bcboot(databcbt=inpt,estmte=inpt2)
            llcit = bcboot_res[[1]]
            ulcit = bcboot_res[[2]]
            llcif[1,eee]=llcit;
            ulcif[1,eee]=ulcit;
            if ((badlo==1) && (llcit != priorlo))  {
              badend=cbind(badend,llcit)
              priorlo=llcit;
            }
            if ((badhi==1) && (ulcit != priorhi))  {
              badend=cbind(badend,ulcit)
              priorhi=ulcit;
            }
          }
          temp2=stdindf[1:ncol(indboot),1, drop=F];
          temp3=effobs[,1:ncol(indboot), drop=F];
          templow=llcif[1,1:ncol(indboot), drop=F];
          temphi=ulcif[1,1:ncol(indboot), drop=F];
          outp=cbind(t(temp3),temp2,t(templow),t(temphi))
          write_output("Partially standardized indirect effect of X on Y", output=medmod_file)
          write_output(outp, col.names = clbs, row.names = indlbl, output=medmod_file);
          temp2=stdindf[(ncol(indboot)+1):(2*ncol(indboot)),1, drop=F];
          temp3=effobs[,(ncol(indboot)+1):(2*ncol(indboot)), drop=F];
          templow=llcif[1,(ncol(indboot)+1):(2*ncol(indboot)), drop=F];
          temphi=ulcif[1,(ncol(indboot)+1):(2*ncol(indboot)), drop=F];
          outp=cbind(t(temp3),temp2,t(templow),t(temphi))
          write_output("Completely standardized indirect effect of X on Y", output=medmod_file)
          write_output(outp, col.names = clbs, row.names = indlbl, output=medmod_file);
          temp2=stdindf[(2*(ncol(indboot))+1):(3*ncol(indboot)),1, drop=F];
          temp3=effobs[,(2*(ncol(indboot))+1):(3*ncol(indboot)), drop=F];
          templow=llcif[1,(2*(ncol(indboot))+1):(3*ncol(indboot)), drop=F];
          temphi=ulcif[1,(2*(ncol(indboot))+1):(3*ncol(indboot)), drop=F];
          outp=cbind(t(temp3),temp2,t(templow),t(temphi))
          write_output("Ratio of indirect to total effect of X on Y", output=medmod_file)
          write_output(outp, col.names = clbs, row.names = indlbl, output=medmod_file);
          temp=eff[,(3*(ncol(indboot))+1):(4*(ncol(indboot))), drop=F];
          temp2=stdindf[(3*(ncol(indboot))+1):(4*ncol(indboot)),1, drop=F];
          temp3=effobs[,(3*(ncol(indboot))+1):(4*ncol(indboot)), drop=F];
          templow=llcif[1,(3*(ncol(indboot))+1):(4*ncol(indboot)), drop=F];
          temphi=ulcif[1,(3*(ncol(indboot))+1):(4*ncol(indboot)), drop=F];
          outp=cbind(t(temp3),temp2,t(templow),t(temphi))
          write_output("Ratio of indirect to direct effect of X on Y", output=medmod_file)
          write_output(outp, col.names = clbs, row.names = indlbl, output=medmod_file);
        }
      }
      if (nmeds == 2)  {
        effkey=cbind(xname,"->",mnames[1,1, drop=F],"->",yname," "," ")
        tempkey=cbind(xname,"->",mnames[1,1, drop=F],"->",mnames[1,2, drop=F],"->",yname)
        effkey=rbind(effkey,tempkey)
        tempkey=cbind(xname,"->",mnames[1,2, drop=F],"->",yname," "," ")
        effkey=rbind(effkey,tempkey)
        effkey=cbind(indlbl[2:4,1, drop=F],effkey)
      }
      if (nmeds == 3)  {
        effkey=cbind(xname,"->",mnames[1,1, drop=F],"->",yname," "," "," "," ")
        tempkey=cbind(xname,"->",mnames[1,1, drop=F],"->",mnames[1,2, drop=F],"->",yname," "," ")
        effkey=rbind(effkey,tempkey)
        tempkey=cbind(xname,"->",mnames[1,1, drop=F],"->",mnames[1,3, drop=F],"->",yname," "," ")
        effkey=rbind(effkey,tempkey)
        tempkey=cbind(xname,"->",mnames[1,1, drop=F],"->",mnames[1,2, drop=F],"->",mnames[1,3, drop=F],"->",yname)
        effkey=rbind(effkey,tempkey)
        tempkey=cbind(xname,"->",mnames[1,2, drop=F],"->",yname," "," "," "," ")
        effkey=rbind(effkey,tempkey)
        tempkey=cbind(xname,"->",mnames[1,2, drop=F],"->",mnames[1,3, drop=F],"->",yname," "," ")
        effkey=rbind(effkey,tempkey)
        tempkey=cbind(xname,"->",mnames[1,3, drop=F],"->",yname," "," "," "," ")
        effkey=rbind(effkey,tempkey)
        effkey=cbind(indlbl[2:8,1, drop=F],effkey)
      }
      if (nmeds == 4)  {
        effkey=cbind(xname,"->",mnames[1,1, drop=F],"->",yname," "," "," "," "," "," ")
        tempkey=cbind(xname,"->",mnames[1,1, drop=F],"->",mnames[1,2, drop=F],"->",yname," "," "," "," ")
        effkey=rbind(effkey,tempkey)
        tempkey=cbind(xname,"->",mnames[1,1, drop=F],"->",mnames[1,4, drop=F],"->",yname," "," "," "," ")
        effkey=rbind(effkey,tempkey)
        tempkey=cbind(xname,"->",mnames[1,1, drop=F],"->",mnames[1,2, drop=F],"->",mnames[1,3, drop=F],"->",yname," "," ")
        effkey=rbind(effkey,tempkey)
        tempkey=cbind(xname,"->",mnames[1,1, drop=F],"->",mnames[1,2, drop=F],"->",mnames[1,4, drop=F],"->",yname," "," ")
        effkey=rbind(effkey,tempkey)
        tempkey=cbind(xname,"->",mnames[1,1, drop=F],"->",mnames[1,3, drop=F],"->",mnames[1,4, drop=F],"->",yname," "," ")
        effkey=rbind(effkey,tempkey)
        tempkey=cbind(xname,"->",mnames[1,1, drop=F],"->",mnames[1,2, drop=F],"->",mnames[1,3, drop=F],"->",mnames[1,4, drop=F],"->",yname)
        effkey=rbind(effkey,tempkey)
        tempkey=cbind(xname,"->",mnames[1,2, drop=F],"->",yname," "," "," "," "," "," ")
        effkey=rbind(effkey,tempkey)
        tempkey=cbind(xname,"->",mnames[1,2, drop=F],"->",mnames[1,3, drop=F],"->",yname," "," "," "," ")
        effkey=rbind(effkey,tempkey)
        tempkey=cbind(xname,"->",mnames[1,2, drop=F],"->",mnames[1,4, drop=F],"->",yname," "," "," "," ")
        effkey=rbind(effkey,tempkey)
        tempkey=cbind(xname,"->",mnames[1,2, drop=F],"->",mnames[1,3, drop=F],"->",mnames[1,4, drop=F],"->",yname," "," ")
        effkey=rbind(effkey,tempkey)
        tempkey=cbind(xname,"->",mnames[1,3, drop=F],"->",yname," "," "," "," "," "," ")
        effkey=rbind(effkey,tempkey)
        tempkey=cbind(xname,"->",mnames[1,3, drop=F],"->",mnames[1,4, drop=F],"->",yname," "," "," "," ")
        effkey=rbind(effkey,tempkey)
        tempkey=cbind(xname,"->",mnames[1,4, drop=F],"->",yname," "," "," "," "," "," ")
        effkey=rbind(effkey,tempkey)
        effkey=cbind(indlbl[2:16,1, drop=F],effkey)
      }
      write_output("Indirect effect key", output=medmod_file)
      write_output(effkey, row.names=FALSE, col.names=FALSE, output=medmod_file);
      if (conmake == 1)  {
        conkey=conkey[2:nrow(conkey),, drop=F]
        conlbs=cntname[1:ncol(indcon),1, drop=F];
        write_output("Specific indirect effect contrast definitions", output=medmod_file)
        write_output(conkey, row.names = conlbs, col.names=FALSE, output=medmod_file);
      }
    }
  }
  if (bad > 0)  {
    note[notes,1]=9
    notes=notes+1
  }
  write_output("****************************** ANALYSIS NOTES AND WARNINGS ******************************", output=medmod_file);
  if (errs > 0) {
    for (i in 1:errs) {
      if (runerrs[i,1]==1)  {
        write_output("ERROR: One of your declared mediators is dichotomous. This procedure cannot be used.", output=medmod_file);
      }
      if (runerrs[i,1]==2)  {
        write_output("ERROR: For model 6, this procedure limits the number of mediators to four.", output=medmod_file);
      }
      if (runerrs[i,1]==3)  {
        write_output("ERROR: For models 1, 2, and 3, only a single variable can be listed in the M list.", output=medmod_file);
      }
      if (runerrs[i,1]==4)  {
        write_output("ERROR: You requested a model involving W but did not provide a valid W variable name.", output=medmod_file);
      }
      if (runerrs[i,1]==5)  {
        write_output("ERROR: You requested a model involving Z but did not provide a valid Z variable name.", output=medmod_file);
      }
      if (runerrs[i,1]==6)  {
        write_output("ERROR: You requested a model involving Q but did not provide a valid Q variable name.", output=medmod_file);
      }
      if (runerrs[i,1]==7)  {
        write_output("ERROR: You requested a model involving V but did not provide a valid V variable name.", output=medmod_file);
      }
      if (runerrs[i,1]==8)  {
        write_output("ERROR: You specified a W variable for a model that does not need it.", output=medmod_file);
      }
      if (runerrs[i,1]==9)  {
        write_output("ERROR: You specified a Z variable for a model that does not need it.", output=medmod_file);
      }
      if (runerrs[i,1]==10)  {
        write_output("ERROR: You specified a Q variable for a model that does not need it.", output=medmod_file);
      }
      if (runerrs[i,1]==11)  {
        write_output("ERROR: You specified a V variable for a model that does not need it.", output=medmod_file);
      }
      if (runerrs[i,1]==12)  {
        write_output("ERROR: The variable specified for W has already been assigned.", output=medmod_file);
      }
      if (runerrs[i,1]==13)  {
        write_output("ERROR: The variable specified for Z has already been assigned.", output=medmod_file);
      }
      if (runerrs[i,1]==14)  {
        write_output("ERROR: The variable specified for Q has already been assigned.", output=medmod_file);
      }
      if (runerrs[i,1]==15)  {
        write_output("ERROR: The variable specified for V has already been assigned.", output=medmod_file);
      }
      if (runerrs[i,1]==16)  {
        write_output("ERROR: You did not provide a valid Y variable name.", output=medmod_file);
      }
      if (runerrs[i,1]==17)  {
        write_output("ERROR: The variable specified for Y has already been assigned.", output=medmod_file);
      }
      if (runerrs[i,1]==18)  {
        write_output("ERROR: Model 6 requires more than one mediator.", output=medmod_file);
      }
      if (runerrs[i,1]==19)  {
        write_output("ERROR: You have not specified a valid model number.", output=medmod_file);
      }
      if (runerrs[i,1]==20)  {
        write_output("ERROR: At least one and only one variable must be listed for X.", output=medmod_file);
      }
      if (runerrs[i,1]==21)  {
        write_output("ERROR: At least one and only one variable must be listed for Y.", output=medmod_file);
      }
      if (runerrs[i,1]==22)  {
        write_output("ERROR: Iteration didn't converge to a solution. Interpret results with caution.", output=medmod_file);
      }
      if (runerrs[i,1]==23)  {
        write_output("ERROR: You specified a clustering variable that does not exist in your variable list.", output=medmod_file);
      }
      if (runerrs[i,1]==24)  {
        write_output("ERROR: You specified a clustering variable that has already been assigned.", output=medmod_file);
      }
      if (runerrs[i,1]==25)  {
        write_output("ERROR: One of more of your M variables is not listed in the variables list.", output=medmod_file);
      }
      if (runerrs[i,1]==26)  {
        write_output("ERROR: A maximum of 20 cluster units is allowed. Use multilevel modeling instead.", output=medmod_file);
      }
      if (runerrs[i,1]==27)  {
        write_output("ERROR: One of the variables in your model is a constant.", output=medmod_file);
      }
      if (runerrs[i,1]==28)  {
        write_output("ERROR: You did not provide a valid X variable name.", output=medmod_file);
      }
      if (runerrs[i,1]==29)  {
        write_output("ERROR: You cannot include your X variable as a mediator.", output=medmod_file);
      } 
      if (runerrs[i,1]==30)  {
        write_output("ERROR: You cannot include your Y variable as a mediator.", output=medmod_file);
      }       
    }
  }
  if (errs == 0)  {
    if ((boot > 1) || (mc > 0))  {
      if ((bconoff == 1) && (boot > 0))  {
        write_output("Number of bootstrap samples for bias corrected bootstrap confidence intervals:", output=medmod_file)
        write_output(boot, row.names=FALSE, col.names=FALSE, output=medmod_file);
      }
      if ((bconoff == 0) && (boot > 0))  {
        write_output("Number of bootstrap samples for percentile bootstrap confidence intervals:", output=medmod_file)
        write_output(boot, row.names=FALSE, col.names=FALSE, output=medmod_file);
      }
      if (mc > 1)  {
        write_output("Number of samples for Monte Carlo confidence intervals:", output=medmod_file)
        write_output(mc, row.names=FALSE, col.names=FALSE, output=medmod_file);
      }
      if (booterr == 1)  {
        badend=badend[1,2:(ncol(badend)), drop=F];
        badend=t(badend);
        write_output("WARNING: Bootstrap CI endpoints below not trustworthy. Decrease confidence or increase bootstraps", output=medmod_file)
        write_output(badend, row.names=FALSE, col.names=FALSE, output=medmod_file);
      }
    }
    write_output("Level of confidence for all confidence intervals in output:", output=medmod_file)
    write_output(conf, row.names=FALSE, col.names=FALSE, output=medmod_file);
    if ((center == 1) && (ncol(centvar) > 0))  {
      centvar=centvar[1,2:ncol(centvar), drop=F];
      write_output("NOTE: The following variables were mean centered prior to analysis:", output=medmod_file)
      write_output(centvar, row.names=FALSE, col.names=FALSE, output=medmod_file); 
    }
    for (i in 1:notes) {
      if (note[i,1]==1)  {
        write_output("NOTE: Confidence level restricted to between 50 and 99.9999%.  95% confidence is provided.", output=medmod_file);
      }
      if (note[i,1]==2)  {
        write_output("NOTE: Effect size measures not available for models with dichotomous outcomes.", output=medmod_file);
      }
      if (note[i,1]==3)  {
        write_output("NOTE: All standard errors for continuous outcome models are based on the HC3 estimator.", output=medmod_file);
      }
      if (note[i,1]==6)  {
        write_output("NOTE: The number of bootstrap samples was adjusted upward given your desired confidence.", output=medmod_file);
      }
      if (note[i,1]==7)  {
        write_output("NOTE: The Johnson-Neyman method is available only for models 1 and 3.", output=medmod_file);
      }
      if (note[i,1]==8)  {
        write_output("NOTE: The Johnson-Neyman method cannot be used with a dichotomous moderator.", output=medmod_file);
      }
      if (note[i,1]==9)  {
        write_output("NOTE: Some bootstrap samples had to be replaced. The number of such replacements was:", output=medmod_file)
        write_output(bad, row.names=FALSE, col.names=FALSE, output=medmod_file);
      }
      if (note[i,1]==10)  {
        write_output("NOTE: Effect size measures for indirect effects not available for models with covariates.", output=medmod_file);
      }
      if (note[i,1]==11)  {
        write_output("NOTE: Some cases were deleted due to missing data. The number of such cases was:", output=medmod_file)
        write_output(nmiss, row.names=FALSE, col.names=FALSE, output=medmod_file);
      }
      if (note[i,1]==12)  {
        write_output("NOTE: Monte Carlo method available only for models 4 and 5.  Bootstrapping was used instead.", output=medmod_file);
      }
      if (note[i,1]==13)  {
        write_output("NOTE: The number of Monte Carlo samples was adjusted upward given your desired confidence.", output=medmod_file);
      }
    }
  }
}

bcboot <- function(databcbt,estmte=9999, env = parent.frame()) {
  
  # bcboot runs and writes out a Bias-Corrected Bootstrap
  
  temp=databcbt[order(databcbt),, drop=F]
  badlo <<- 0;
  badhi <<- 0;
  if (estmte != 9999) {
    boot=env$boot
    xp2=env$xp2
    p0=-.322232431088;
    p1 = -1;
    p2 = -.342242088547;
    p3 = -.0204231210245;
    p4 = -.0000453642210148;
    q0 = .0993484626060;
    q1 = .588581570495;
    q2 = .531103462366;
    q3 = .103537752850;
    q4 = .0038560700634;
    pv = length(temp[(temp < as.vector(estmte))]) / boot
    ppv=pv;
    if (pv > 0.5) ppv=1-pv
    y5=sqrt(-2*log(ppv));
    xp=y5+((((y5*p4+p3)*y5+p2)*y5+p1)*y5+p0)/((((y5*q4+q3)*y5+q2)*y5+q1)*y5+q0);
    if (pv <= .5) xp=-xp
    cilow=round(boot*(pnorm(2*xp+xp2)));
    cihigh=trunc(boot*(pnorm(2*xp+(-xp2))))+1;
    if (cilow < 1) {
      cilow=1
      booterr=1
      badlo=1
    }
    if (cihigh > boot) {
      cihigh=boot
      booterr=1
      badhi=1
    }
    return(list(temp[cilow,1, drop=F], temp[cihigh,1, drop=F]))
  }
  if (estmte==9999) {
    cilow=env$cilow
    cihigh=env$cihigh
    return(list(temp[cilow,1, drop=F], temp[cihigh,1, drop=F]))
  }
}
