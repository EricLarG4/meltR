

#libraries----

library(shiny)
library(shinydashboard)
library(shinydashboardPlus)
library(shinyWidgets)
library(shinyBS)
library(shinysky)
library(shinythemes)

library(tidyverse)
library(readxl)
library(writexl)
library(DT)
library(QWUtils)
library(zoo)
library(data.table)
library(DescTools)
library(hablar)
library(assertive.types)
library(magrittr)
library(pspline)
library(prospectr)

library(ggsci)
library(ggpmisc)
library(ggpubr)
library(ggthemes)
library(ggrepel)
library(plotly)

#sources----
source('meltR_functions.R')

#UI-----------
ui <- dashboardPagePlus(
    #header--------------
    dashboardHeaderPlus(
        tags$li(class = 'dropdown',
                tags$style(".main-header {max-height: 120px}"),
                tags$style(".main-header .logo {height: 60px}"),
                tags$style('.control-sidebar {padding-top: 80px}'),
                tags$style('.main-sidebar {padding-top: 80px}')),
        title = "meltR",
        enable_rightsidebar = TRUE,
        rightSidebarIcon = 'question-circle'),
    ##rightsidebar----
    rightsidebar = rightSidebar(
        id = 1,
        background = 'dark',
        rightSidebarTabContent(
            id = 2,
            title = 'Non linear fitting',
            icon = 'thermometer-half',
            h4('I. Data preparation'),
            tags$a(href = 'https://github.com/EricLarG4/meltR/raw/master/demo_input.xlsx',
                   '1. Download the input file and replace the demo data with yours'),
            p("2. Make sure to fill in the header cells"),
            p('3. Upload the data using the raw data box in the left sidebar'),
            p('4. Select the data to process with the input data hover panel'),
            h4('II. Data processing'),
            p('1. Derivate the data with the blue button'),
            p('2. Initialize the fitting with the orange button'),
            p('3. If necessary, correc the initialization. In particular,
          make sure the initial Tm is close to its expected value'),
          p('4. Start the fitting with the red button'),
          h4('III. Results'),
          p('1. Review the fitting. In particular, check that the baselines are consistent with the data'),
          p('2. ...')
        ),
        rightSidebarTabContent(
            id = 3,
            title = 'Baseline explorer',
            icon = 'binoculars',
            p('placeholder')
        )
    ),
    ##sidebar-------------
    sidebar_fullCollapse = TRUE,
    dashboardSidebar(
        ## a-meltR----
        conditionalPanel(
            condition = "input.tabs == 'meltR'",
            boxPlus(
                width = "100%",
                title = "Raw data",
                status = 'info',
                solidHeader = F,
                collapsible = T,
                fileInput(
                    'raw.data.input',
                    'Select .xlsx file'
                ),
                switchInput(inputId = "melt.blank", #toggles baseline on/off
                            label = "Blank",
                            onLabel = 'subtract',
                            offLabel = 'ignore',
                            value = TRUE,
                            size = 'normal',
                            width = 'auto')
            ),
            boxPlus(
                width = "100%",
                title = "Fitting",
                status = 'danger',
                solidHeader = F,
                collapsible = T,
                switchInput(
                    inputId = 'drv.type',
                    label = 'derivative algorithm',
                    onLabel = 'auto poly',
                    offLabel = 'Savitzky–Golay',
                    value = TRUE,
                    size = 'normal',
                    width = 'auto'
                ),
                sliderInput(
                    inputId = "sav.window",
                    label = "Savitzky–Golay window",
                    min = 3,
                    max = 21,
                    value = 5,
                    step = 2
                ),
                actionBttn(
                    inputId = "bttn.deriv.melt", #initiates fit
                    label = "Plot derivatives",
                    icon = icon('calculator', class = 'regular'),
                    style = "simple",
                    color = "primary",
                    size = "sm",
                    block = F,
                    no_outline = TRUE
                ),
                sliderInput(
                    "nb.it.melt.fit",
                    "Max iterations",
                    min = 500,
                    max = 100000,
                    value = 5000,
                    step = 500
                ),
                actionBttn(
                    inputId = "bttn.init.melt", #initiates fit
                    label = "Initialize fitting",
                    icon = icon('sign-out-alt', class = 'regular'),
                    style = "simple",
                    color = "warning",
                    size = "sm",
                    block = F,
                    no_outline = TRUE
                ),
                actionBttn(
                    inputId = "bttn.fit.melt", #initiates fit
                    label = "Launch fitting",
                    icon = icon('sign-in-alt', class = 'regular'),
                    style = "simple",
                    color = "danger",
                    size = "sm",
                    block = F,
                    no_outline = TRUE
                ),
                switchInput(
                    inputId = "toggle.baseline", #toggles baseline on/off
                    label = "toggle baselines",
                    value = TRUE
                ),
                sliderInput(
                    "temp.ff",
                    "Temperature (°C) for folded fraction", #Temperature for deltaG calculation
                    min = 0,
                    max = 100,
                    value = 37
                ),
                sliderInput(
                    "temp.therm",
                    "Temperature (K) for DeltaG", #Temperature for deltaG calculation
                    min = 273,
                    max = 373,
                    value = 273.15 + 22
                )
            ),
            boxPlus(
                title = "Download figures",
                id = "melt.dl",
                collapsible = T,
                solidHeader = F,
                width = '100%',
                downloadBttn(
                    outputId = "dwn.melt.fit",
                    label = "Fit data",
                    style = "material-flat",
                    size = 'xs'
                ),
                downloadBttn(
                    outputId = "dwn.melt.model",
                    label = "Model data",
                    style = "material-flat",
                    size = 'xs'
                ),
                downloadBttn(
                    outputId = "dwn.melt.folded",
                    label = "Folded fraction",
                    style = "material-flat",
                    size = 'xs'
                ),
                downloadBttn(
                    outputId = "dwn.melt.Tm",
                    label = "Tm summary",
                    style = "material-flat",
                    size = 'xs'
                )
            )
        ),
        ## b-baseline exploR----
        conditionalPanel(
            condition = "input.tabs == 'Baseline exploR'",
            boxPlus(
                width = '100%',
                title = 'baseline range selector',
                status = 'info',
                solidHeader = F,
                collapsible = T,
                sliderInput(
                    'start.low',
                    'Low-T baseline range start',
                    min = 0, max = 100,
                    step = 1,
                    value = c(4, 25)
                ),
                sliderInput(
                    'end.low',
                    'Low-T baseline range end',
                    min = 0, max = 100,
                    step = 1,
                    value = c(9, 30)
                ),
                sliderInput(
                    'start.high',
                    'High-T baseline range start',
                    min = 0, max = 100,
                    step = 1,
                    value = c(75, 87)
                ),
                sliderInput(
                    'end.high',
                    'High-T baseline range end',
                    min = 0, max = 100,
                    step = 1,
                    value = c(80, 92)
                ),
                sliderInput(
                    't.range',
                    'Minimum baseline width (K)',
                    min = 2, max = 10,
                    step = 1,
                    value = 5
                ),
                sliderInput(
                    'nb.spl',
                    'Number of samples',
                    min = 0, max = 100,
                    step = 1,
                    value = 10
                ),
                sliderInput(
                    'theta.base.temp',
                    'Temp for theta (°C)',
                    min = 0,
                    max = 100,
                    step = 1,
                    value = 25
                ),
                actionBttn(
                    inputId = "bttn.baselines", #initiates fit
                    label = "Plot baselines",
                    icon = icon('align-justify'),
                    style = "simple",
                    color = "primary",
                    size = "sm",
                    block = F,
                    no_outline = TRUE
                )
            )
        )
    ),
    #Body----
    dashboardBody(
        #color of selected datatable rows
        tags$style(HTML('table.dataTable tr.selected td,
                        table.dataTable td.selected {background-color: pink !important;}')),
        tags$style('label{color: steelblue}'),
        tags$style('.irs-bar{background: linear-gradient(to right, steelblue, tomato)}'),
        tags$style('.irs-bar-edge{background: steelblue}'),
        navbarPage(
            'Navigation',
            id = 'tabs',
            theme = shinytheme("sandstone"),
            ## a-Panel: meltR---------
            tabPanel(
                title = 'meltR',
                icon = icon("thermometer-half"),
                fluidRow(
                    column(12,
                           collapsible_tabBox(
                               title = 'Input data',
                               id = 'tabbox.1',
                               width = 6,
                               selected = NULL,
                               side = 'left',
                               tabPanel(
                                   title = 'Input data',
                                   plotOutput("p.melt.filtered"),
                                   icon = icon('server', class = 'regular'),
                               ),
                               tabPanel(
                                   title = 'Filtered data table',
                                   DTOutput("melt.filtered"),
                                   icon = icon('filter')
                               ),
                               tabPanel(
                                   title = 'Derivative plot',
                                   plotOutput("p.melt.derivative"),
                                   icon = icon('calculator', class = 'regular'),
                               ),
                               tabPanel(
                                   title = 'Folded fraction',
                                   width = 6,
                                   plotOutput("p.folded.melt.fit")
                               ),
                               tabPanel(
                                   title = 'Modeled folded fraction',
                                   width = 6,
                                   plotlyOutput("p.folded.modeled")
                               )
                           ),
                           collapsible_tabBox(
                               title = 'Fit',
                               id = 'tabbox.2',
                               width = 6,
                               selected = NULL,
                               side = 'left',
                               tabPanel(
                                   title = 'Approximate Tm',
                                   DTOutput("melt.derivative"),
                                   width = 6,
                                   icon = icon('thermometer-half')
                               ),
                               tabPanel(
                                   title = 'Fit initialization',
                                   hotable("hotable1"),
                                   width = 6,
                                   icon = icon('sign-out-alt')
                               ),
                               tabPanel(
                                   title = 'Fit result',
                                   width = 6,
                                   plotOutput("p.raw.melt.fit"),
                                   icon = icon('sign-out-alt')
                               )
                           )
                    ),
                    column(12,
                           collapsible_tabBox(
                               title = 'Melting temperatures',
                               id = 'tabbox.4',
                               width = 12,
                               selected = NULL,
                               side = 'left',
                               tabPanel(
                                   title = 'Table',
                                   width = 12,
                                   DTOutput("fit.melt.result.summary")
                               ),
                               tabPanel(
                                   title = 'Boxplots',
                                   width = 12,
                                   plotOutput("fit.melt.result.plot")
                               ),
                               tabPanel(
                                   title = 'dtheta/d(1/T)',
                                   width = 12,
                                   plotOutput('theta.deriv')
                               )
                           )
                    ),
                    column(12,
                           boxPlus(id = 'fit.output',
                                   title = 'Fit output',
                                   collapsible = T,
                                   collapsed = T,
                                   width = 12,
                                   DTOutput("nlfit.melt.results")
                           )
                    )

                ),
                ### i.absolute panels----
                absolutePanel(
                    id = "custom.melt",
                    # class = "panel panel-default",
                    top = 150, right = 100,
                    width = 200, height = 'auto',
                    draggable = TRUE, fixed = TRUE,
                    bsCollapse(id = 'bsCollapseTest',
                               open = 'Customisation',
                               bsCollapsePanel(
                                   'Customisation',
                                   uiOutput('select.melting.palette.fam'),
                                   uiOutput('select.melting.palette'),
                                   sliderInput('size.dot.melt', 'Dot size',
                                               min=0, max=10, value=4,
                                               step=0.25, round=0),
                                   sliderInput('alpha.dot.melt', 'Dot transparency',
                                               min = 0, max = 1, value = 0.7,
                                               step=0.05, round=0),
                                   sliderInput('size.line.melt', 'line size',
                                               min=0, max=5, value=1,
                                               step=0.1, round=0),
                                   sliderInput('alpha.line.melt', 'line transparency',
                                               min = 0, max = 1, value = 1,
                                               step=0.05, round=0),
                                   sliderInput('size.baseline.melt', 'baseline size',
                                               min=0, max=5, value=1,
                                               step=0.1, round=0),
                                   sliderInput('alpha.baseline.melt', 'baseline transparency',
                                               min = 0, max = 1, value = 0.75,
                                               step=0.05, round=0),
                                   style = 'primary'
                               )
                    ),
                    style = "opacity: 0.9"
                )
            ),
            ## b-Panel: baselines----
            tabPanel(
                title = 'Baseline exploR',
                icon = icon("binoculars"),
                fluidRow(
                    column(
                        12,
                        collapsible_tabBox(
                            title = 'Baselines',
                            id = 'tabbox.5',
                            width = 6,
                            selected = NULL,
                            side = 'left',
                            tabPanel(
                                title = 'Baseline range selection',
                                plotOutput('p.melt.filtered.base'),
                                icon = icon('sliders-h', class = 'solid')
                            ),
                            tabPanel(
                                title = 'Tm',
                                width = 6,
                                plotOutput('p.tm.median')
                            ),
                            tabPanel(
                                title = 'theta',
                                width = 6,
                                plotOutput('p.theta')
                            )
                        ),
                        collapsible_tabBox(
                            title = 'Data',
                            id = 'tabbox.6',
                            width = 6,
                            selected = NULL,
                            side = 'left',
                            tabPanel(
                                title = 'Baseline ranges',
                                width = 6,
                                DTOutput('baseline.set')
                            ),
                            tabPanel(
                                title = 'Tm',
                                width = 6,
                                DTOutput('tm.median')
                            )
                        ),
                        collapsible_tabBox(
                            title = 'Baselines',
                            id = 'tabbox.7',
                            width = 12,
                            selected = NULL,
                            side = 'left',
                            tabPanel(
                                title = 'Plots',
                                width = 12,
                                uiOutput('p.bases.ui')
                            ),
                            tabPanel(
                                title = 'Data',
                                width = 6,
                                DTOutput('baseline.gen')
                            )
                        )
                    )
                )
            ),
            ## c-Data filter absolute panel----
            absolutePanel(
                id = "filter.melt",
                # class = "panel panel-default",
                top = 150, right = 850,
                width = 300, height = 'auto',
                draggable = TRUE, fixed = TRUE,
                bsCollapse(id = 'bsCollapseMelt',
                           open = 'Data input',
                           bsCollapsePanel(
                               'Data input',
                               sliderInput(
                                   "slider.therm",
                                   "Filter temperatures", #Temperature for deltaG calculation
                                   min = 275,
                                   max = 375,
                                   step = 0.5,
                                   value = c(276, 363)
                               ),
                               uiOutput("select.melting.oligo"),
                               uiOutput("select.melting.ramp"),
                               uiOutput("select.melting.comment"),
                               uiOutput("select.melting.rep"),
                               uiOutput("select.melting.id"),
                               style = 'success'
                           )
                ),
                style = "opacity: 0.9"
            )
        )
    )
)


#SERVER----
server <- function(input, output, session) {

    options(shiny.maxRequestSize=5000*1024^2)

    #1-inputfile----

    input.file <- reactive({
        input$raw.data.input
    })

    #2-blank management----

    #allows to toggle the blank subtraction on/off
    blk.subtract <- reactive({
        if (input$melt.blank == T) {
            blk.subtract = 1
        } else {
            blk.subtract = 0
        }
    })

    #3-raw data processing
    melt.input <- reactive({

        wide.input <- read_excel(input.file()$datapath,
                                 sheet = "UV-melting")

        #extract descriptors
        descriptors <- wide.input %>%
            slice(1:7)

        #extract data
        raw.data <- wide.input %>%
            slice(-1:-7)

        data.collector <- data.frame()

        for (i in 1:(ncol(raw.data)/3)) {

            n <- 1+3*(i-1) #converts i to the temperature column index (starts at 1 then increase by 3)

            buffer <- raw.data %>%
                select(n, n+1, n+2) %>% #select every couple column group
                mutate(descriptors[[1, n+1]], #adds columns for descriptors
                       descriptors[[2, n+1]],
                       descriptors[[3, n+1]],
                       descriptors[[4, n+1]],
                       descriptors[[5, n+1]],
                       descriptors[[6, n+1]]) %>%
                magrittr::set_colnames(c('T.unk', 'abs.raw', 'abs.blk', 'oligo', 'buffer', 'cation', 'rep', 'melt.l', 'melt.c')) %>%
                mutate(comment = ifelse(is.na(cation), buffer, paste(buffer, '+', cation))) %>%
                convert(num('T.unk', 'abs.raw', 'abs.blk', 'melt.l', 'melt.c')) %>% #converts some columns to numeric type
                drop_na('T.unk')

            #binds data
            data.collector <- rbind(data.collector, buffer,
                                    make.row.names = F)

        }

        return(data.collector)

    })

    melt <- reactive({
        melt.input() %>%
            filter(!is.na(oligo)) %>% #removes empty lines
            group_by(oligo, comment, rep) %>%
            mutate(ramp = if_else(lead(T.unk) > T.unk, 'heating', 'cooling')) %>% #ramp determination
            #no ramp found for last row of each spl (because there's no next T value)
            mutate(ramp = if_else(is.na(ramp), lag(ramp), ramp)) %>%
            ungroup() %>%  #necessary to use data at derivative step (not sure why)
            mutate(id = paste(oligo, comment, ramp, rep, sep = '-'))%>% #create an experiment id
            # Detects whether the raw data is supplied in Celsius or Kelvin and converts to Kelvin if necessary
            mutate(T.K = if_else(abs.raw < 100, T.unk + 273.15, T.unk)) %>%
            add_column(blk.sub = blk.subtract()) %>%
            group_by(id) %>%
            #subtract the blank column is values are provided and toggle activated + converts to molar absorbtion coeff
            mutate(abs.melt = if_else(is.na(abs.blk), abs.raw/(melt.c/1E6 * melt.l),
                                      if_else(blk.sub == 1, (abs.raw - abs.blk)/(melt.c/1E6 * melt.l), abs.raw/(melt.c/1E6 * melt.l)))) %>%
            ungroup()


    })

    #3-data selection-----

    file.toggle <- reactive({
        if(is.null(input.file())){
            return('no')
        } else {
            return('yes')
        }
    })

    output$select.melting.oligo <- renderUI({
        if(file.toggle()=='no') {
            pickerInput("select.melting.oligo",
                        label = "Choose oligos",
                        choices = "upload data first",
                        multiple = T,
                        options = pickerOptions(
                            noneSelectedText = 'upload data first'
                        )
            )
        } else {
            pickerInput("select.melting.oligo",
                        label = "Choose oligos",
                        choices = unique(melt.input()$oligo),
                        selected = unique(melt.input()$oligo),
                        multiple = T,
                        options = pickerOptions(
                            actionsBox = T,
                            liveSearch = T
                        )
            )
        }
    })

    output$select.melting.ramp <- renderUI({
        if(file.toggle()=='no') {
            pickerInput("select.melting.oligo",
                        label = "Choose ramps",
                        choices = "upload data first",
                        multiple = T,
                        options = pickerOptions(
                            noneSelectedText = 'upload data first'
                        )
            )

        } else {
            pickerInput("select.melting.ramp",
                        label = "Choose ramps",
                        choices = unique(melt()$ramp),
                        selected = unique(melt()$ramp),
                        multiple = T,
                        options = pickerOptions(
                            actionsBox = T,
                            liveSearch = T
                        )
            )
        }
    })

    output$select.melting.comment <- renderUI({
        if(file.toggle()=='no') {
            pickerInput("select.melting.oligo",
                        label = "Choose comments",
                        choices = "upload data first",
                        multiple = T,
                        options = pickerOptions(
                            noneSelectedText = 'upload data first'
                        )
            )

        } else {
            pickerInput("select.melting.comment",
                        label = "Choose buffer",
                        choices = unique(melt()$comment),
                        selected = unique(melt()$comment),
                        multiple = T,
                        options = pickerOptions(
                            actionsBox = T,
                            liveSearch = T
                        )
            )
        }
    })

    output$select.melting.rep <- renderUI({
        if(file.toggle()=='no') {
            pickerInput("select.melting.oligo",
                        label = "Choose replicates",
                        choices = "upload data first",
                        multiple = T,
                        options = pickerOptions(
                            noneSelectedText = 'upload data first'
                        )
            )

        } else {
            pickerInput("select.melting.rep",
                        label = "Choose replicates",
                        choices = unique(melt()$rep),
                        selected = unique(melt()$rep),
                        multiple = T,
                        options = pickerOptions(
                            actionsBox = T,
                            liveSearch = T
                        )
            )
        }
    })

    output$select.melting.id <- renderUI({
        if(file.toggle()=='no') {
            pickerInput("select.melting.oligo",
                        label = "Choose id",
                        choices = "upload data first",
                        multiple = T,
                        options = pickerOptions(
                            noneSelectedText = 'upload data first'
                        )
            )

        } else {
            pickerInput("select.melting.id",
                        label = "Choose id",
                        choices = unique(melt()$id),
                        selected = unique(melt()$id),
                        multiple = T,
                        options = pickerOptions(
                            actionsBox = T,
                            liveSearch = T
                        )
            )
        }
    })

    #4-selected data display----

    melt.filtered <- reactive({

        if(file.toggle()=='no') {
            return(NULL)
        } else {

            melt.filtered.buffer <-  melt() %>% #input data filtering
                filter(oligo %in% input$select.melting.oligo) %>%
                filter(ramp %in% input$select.melting.ramp) %>%
                filter(comment %in% input$select.melting.comment) %>%
                filter(rep %in% input$select.melting.rep) %>%
                filter(id %in% input$select.melting.id) %>%
                filter(T.K > min(input$slider.therm) & T.K < max(input$slider.therm))

            return(melt.filtered.buffer)
        }

    })

    output$melt.filtered <- DT::renderDT(server=FALSE,{
        if(file.toggle()=='no') {
            return(NULL)
        } else {
            melt.filtered() %>%
                select(-blk.sub) %>%
                setcolorder(
                    c('id', 'oligo', 'buffer', 'cation', 'comment', 'rep',
                      'ramp', 'T.unk', 'T.K', 'abs.raw', 'abs.blk', 'abs.melt',
                      'melt.l', 'melt.c')) %>%
                datatable(
                    extensions = c('Buttons', 'Responsive', 'Scroller'),
                    colnames = c(
                        "T (K)" = "T.K",
                        'T (input unit)' = 'T.unk',
                        'raw A' = 'abs.raw',
                        'blank A' = 'abs.blk',
                        'oligonucleotide' = 'oligo',
                        'replicate' = 'rep',
                        'pathlength' = 'melt.l',
                        'concentration' = 'melt.c',
                        'buffer' = 'comment',
                        'electrolyte' = 'buffer',
                        'extinction coefficient
                    ' = 'abs.melt'
                    ),
                    rownames = F,
                    escape = T,
                    filter = 'top',
                    autoHideNavigation = T,
                    options = list(
                        deferRender = TRUE,
                        scrollY = 200,
                        scroller = F,
                        pageLength = 25,
                        autoWidth = F,
                        dom = 'Bfrtip', #button position
                        buttons = c('copy', 'csv', 'excel', 'colvis'), #buttons
                        columnDefs = list(list(visible=FALSE, targets=c(0,2,3,7,9,10,12,13)))
                    )
                )
        }
    })

    output$p.melt.filtered <- renderPlot({
        if(file.toggle()=='no') {return(NULL)}
        else {
            p45 <- ggplot(data = melt.filtered(),
                          aes(x = T.K, y = abs.melt, color = id, shape = ramp)) +
                geom_point(size = input$size.dot.melt, alpha = input$alpha.dot.melt) +
                scale_color_d3() +
                theme_pander() +
                xlab("Temperature (K)") +
                ylab(bquote(epsilon*' ('*M^-1~cm^-1*')'))  #modifies axes titles

            # p45 <- melt.palette.modifier(plot = p45)

            return(p45)
        }
    })

    #5-derivatives--------

    melt.derivative <- eventReactive(input$bttn.deriv.melt,{

        if(input$drv.type == T){

            spl.drv <- melt.filtered() %>%
                group_by(id) %>%
                mutate(emp = abs(
                    predict(
                        sm.spline(T.K, abs.melt), #automated polynomial smoothing
                        T.K, 1) #get deriv against temperature from polynomial function
                ))

            #switches UI tab automatically to derivative when calculating it
            observeEvent(input$bttn.deriv.melt, {
                updateTabsetPanel(session = session,
                                  inputId = "tabbox.1",
                                  selected = 'Derivative plot'
                )
            })

            return(spl.drv)

        } else {

            #Savitzky-Golay function
            sav <- function(input, diff.order, poly.order, win.size){
                input %>%
                    #adapts number of row to the smoothing output
                    slice_min(n = nrow(input)-(win.size-1)/2, order_by = T.K) %>% #removes coldest temp
                    slice_max(n = nrow(input)-(win.size-1), order_by = T.K) %>% #removes hottest temp
                    mutate(emp = abs(#absolute value
                        prospectr::savitzkyGolay(#Savitzky-Golay smooth
                            X = input$abs.melt, #input is absorbance
                            m = diff.order, #differential order
                            p = poly.order, #polynomilal order
                            w = win.size #window size
                        )
                    ))
            }

            #initialize a collection data.frame
            collec <- data.frame()

            #loop across ids
            for (i in unique(melt.filtered()$id)) {

                buffer <- sav(
                    input = melt.filtered() %>%
                        filter(id == i) %>% #select id
                        arrange(-T.K), #important to slice correctly, all data ordered by decreasing T.
                    diff.order = 1, #first derivative
                    poly.order = 2, #poly 2nd order
                    win.size = input$sav.window #user defined window
                )

                collec <- rbind(collec, buffer)
            }

            return(collec)
        }
    })

    #plot derivatives
    output$p.melt.derivative <- renderPlot({

        p46 <- ggplot(melt.derivative(), aes(T.K, emp, color = id, shape = ramp)) +
            geom_point(size = input$size.dot.melt, alpha = input$alpha.dot.melt) +
            theme_pander() +
            scale_color_d3(palette = "category20") +
            xlab("Temperature (K)") +
            ylab(bquote(Delta*epsilon*'/'*Delta*'T ('*M^-1~cm^-1*K^-1*')'))

        # p46 <- melt.palette.modifier(plot = p46)

        return(p46)

    })

    #6-fit initialization----
    Tm.init.deriv <- reactive({
        melt.derivative() %>%
            group_by(id) %>%
            filter(emp == max(emp)) %>%
            select(id, T.K)
    })

    output$melt.derivative <- DT::renderDT({
        Tm.init.deriv()
    })

    tm.init0 <- eventReactive(input$bttn.init.melt, {
        tm.init0 <- Tm.init.deriv() %>%
            rename("Tm.init" = "T.K") %>%
            add_column(P1.init = 1.3e+05,
                       P3.init = 1,
                       P4.init = 0.3,
                       P5.init = 0,
                       P6.init = -0.2)

        tm.init0$legend = tm.init0$id

        return(tm.init0)

    })


    tm.init.change <- reactive({
        as.data.frame(hot.to.df(input$hotable1))
    })

    output$hotable1 <- renderHotable({tm.init0() }, readOnly = F)

    #switches UI tab automatically to hottable when initializing it
    observeEvent(input$bttn.init.melt, {
        updateTabsetPanel(session = session,
                          inputId = "tabbox.2",
                          selected = 'Fit initialization'
        )
    })

    #switches UI tab automatically to hottable when initializing it
    observeEvent(input$bttn.fit.melt, {
        updateTabsetPanel(session = session,
                          inputId = "tabbox.2",
                          selected = 'Fit result'
        )
    })


    #7-fitting----------------------------

    nlfit.melt <- eventReactive(input$bttn.fit.melt, {

        #initialize the data.frame to collect results
        fit.melt.results <- data.frame()

        #loops across all unique selected ids
        for (i in unique(melt.filtered()$id)) {

            #buffers the data to fit
            fit.melt.input.buffer <- data.frame(melt.filtered()) %>%
                filter(id == i)

            #initialize Parameters
            fit.melt.init.par <- subset(tm.init.change(), id == i)

            melt.c <- unique(fit.melt.input.buffer$melt.c) #oligo concentration
            melt.l <- unique(fit.melt.input.buffer$melt.l) #path length

            P1s <- as.vector(fit.melt.init.par$P1.init)
            P2s <- as.vector(fit.melt.init.par$Tm.init)
            P3s <- as.vector(fit.melt.init.par$P3.init)/(melt.c/1E6 * melt.l) #convert initial parameters to molar abs coeff.
            P4s <- as.vector(fit.melt.init.par$P4.init)/(melt.c/1E6 * melt.l)
            P5s <- as.vector(fit.melt.init.par$P5.init)/(melt.c/1E6 * melt.l)
            P6s <- as.vector(fit.melt.init.par$P6.init)/(melt.c/1E6 * melt.l)

            #fit
            ms <- nls(
                data=fit.melt.input.buffer,
                fit.melt.input.buffer$abs.melt~(P3+P4*fit.melt.input.buffer$T.K)*1/(1+exp(-P1*(1-fit.melt.input.buffer$T.K/P2)/(8.31451*fit.melt.input.buffer$T.K)))+
                    (P5+P6*fit.melt.input.buffer$T.K)*exp(-P1*(1-fit.melt.input.buffer$T.K/P2)/(8.31451*fit.melt.input.buffer$T.K))
                /(1+exp(-P1*(1-fit.melt.input.buffer$T.K/P2)/(8.31451*fit.melt.input.buffer$T.K))),
                start = list(P1 = P1s, P2 = P2s, P3=P3s, P4=P4s, P5=P5s, P6=P6s),
                nls.control(maxiter = input$nb.it.melt.fit,
                            warnOnly = T)
            )

            #buffers the fit results
            fit.melt.output.buffer <- data.frame(id = i,
                                                 nb.data.pt = nobs(ms),
                                                 init.Tm =  P2s,
                                                 RSS = sum(residuals(ms)^2),
                                                 SE.residual = sigma(ms),
                                                 P1 = as.vector(coef(ms))[1],
                                                 P1SD = summary(ms)$coefficient[1,2],
                                                 P2 = as.vector(coef(ms))[2],
                                                 P2SD = summary(ms)$coefficient[2,2],
                                                 P3 = as.vector(coef(ms))[3],
                                                 P3SD = summary(ms)$coefficient[3,2],
                                                 P4 = as.vector(coef(ms))[4],
                                                 P4SD = summary(ms)$coefficient[4,2],
                                                 P5 = as.vector(coef(ms))[5],
                                                 P5SD = summary(ms)$coefficient[5,2],
                                                 P6 = as.vector(coef(ms))[6],
                                                 P6SD = summary(ms)$coefficient[6,2],
                                                 fit.Tm.K = round(as.vector(coef(ms))[2], 2),
                                                 fit.Tm.C = round(as.vector(coef(ms))[2] - 273.15, 2),
                                                 DeltaH = -as.vector(coef(ms))[1],
                                                 DeltaS = -as.vector(coef(ms))[1]/as.vector(coef(ms))[2]
                                                 # DeltaG = as.vector(coef(ms))[1] - input$slider.therm * as.vector(coef(ms))[1]/as.vector(coef(ms))[2]
                                                 # DeltaG = -8.314 * input$slider.therm * log(as.vector(coef(ms))[1] * (1 - input$slider.therm/as.vector(coef(ms))[2])/8.314 * input$slider.therm)
            )


            #row bind the results acroos the loop
            fit.melt.results <- rbind(fit.melt.results, fit.melt.output.buffer)
        }

        #switches UI tab automatically to fit results when calculating them
        observeEvent(input$bttn.deriv.melt, {
            updateTabsetPanel(session = session,
                              inputId = "tabbox.1",
                              selected = 'Folded fraction'
            )
        })

        return(fit.melt.results)
    })

    #8-fit output----

    #fit results table output
    output$nlfit.melt.results <- DT::renderDT(server=FALSE,{
        datatable(
            nlfit.melt(),
            extensions = c('Buttons', 'Responsive', 'Scroller'),
            colnames = c("Data points" = "nb.data.pt",
                         'Initial Tm' = 'init.Tm',
                         "P1 sd" = 'P1SD',
                         "P2 sd" = 'P2SD',
                         "P3 sd" = 'P3SD',
                         "P4 sd" = 'P4SD',
                         "P5 sd" = 'P5SD',
                         "P6 sd" = 'P6SD',
                         "RMSE" = "SE.residual"),
            rownames = F,
            escape = T,
            filter = 'top',
            autoHideNavigation = T,
            options = list(
                deferRender = TRUE,
                scrollY = 200,
                scroller = F,
                pageLength = 25,
                autoWidth = F,
                dom = 'Bfrtip', #button position
                buttons = c('copy', 'csv', 'excel', 'colvis'), #buttons
                columnDefs = list(list(visible=FALSE, targets=c(3,6, 8, 10, 12, 14, 16, 17, 18, 19, 20)))
            )
        ) %>%
            formatRound(c("P4", 'P4 sd', "P6", 'P6 sd', 'RMSE'), digits = 5) %>%
            formatRound(c('P1', 'P1 sd'), digits = 0) %>%
            formatRound(c('P2', 'P2 sd'), digits = 2) %>%
            formatRound(c('P3', 'P3 sd', 'P5', 'P5 sd'), digits = 3) %>%
            formatRound(c('RSS'), digits = 6)
    })


    fit.melt.result.df <- reactive({
        # if(is.null(input$input.file)) {return(NULL)}
        # else {
        left_join(melt.filtered(),nlfit.melt(),
                  by = c("id")) %>% #join fit result with raw data (only selected ids)
            mutate(folded.fraction = (1/(1+exp(-P1*(1-T.K/P2)/(8.31451*T.K))))) %>%  #folded fraction
            mutate(folded.fraction.base = (P5+P6*T.K-abs.melt)/(P5+P6*T.K - P3-P4*T.K)) %>% #baseline corrected folded fraction
            #fitted line
            mutate(raw.fit.y = (P3+P4*T.K)*1/(1+exp(-P1*(1-T.K/P2)/(8.31451*T.K)))+(P5+P6*T.K)*exp(-P1*(1-T.K/P2)/(8.31451*T.K))/(1+exp(-P1*(1-T.K/P2)/(8.31451*T.K)))) %>%
            mutate(low.T.baseline = P3+P4*T.K) %>%
            mutate(high.T.baseline = P5+P6*T.K) %>%
            filter(T.K > min(input$slider.therm) & T.K < max(input$slider.therm))
        # }
    })


    #outputs the fitted raw data
    p.raw.melt.fit <- reactive({
        # if(is.null(input$input.file)) {return(NULL)}
        # else {
        p0 <- ggplot(fit.melt.result.df()) +
            geom_point(aes(T.K, abs.melt, color = id), size = input$size.dot.melt, alpha = input$alpha.dot.melt, shape = 16) + #plots the experimental data
            geom_line(aes(x = T.K, y = raw.fit.y, color = id),
                      size = input$size.line.melt, alpha = input$alpha.line.melt) +
            ylab(bquote(epsilon*' ('*M^-1~cm^-1*')')) + #modifies axes titles
            xlab("Temperature (K)") +
            # scale_y_continuous(limits=c(-0.1,1.1), breaks = c(0, 0.25, 0.5, 0.75, 1.0)) +
            labs(color="id") +
            # scale_color_d3(palette = "category20") +
            theme(axis.text=element_text(size=12), axis.title=element_text(size=16,face="bold")) + #axis style
            theme(axis.text.x = element_text(color = "black", size = 14, angle = 0),
                  axis.text.y = element_text(color = "black", size = 14, angle = 0)) + #axis labels style
            theme(legend.position="right",
                  legend.box = "vertical",
                  legend.title = element_text(size=14,
                                              face="bold"),
                  legend.key = element_rect(fill = "white"),
                  legend.text = element_text(size=12,
                                             face="bold")) +
            theme(plot.margin=unit(c(0.5,0.5,0.5,0.5),"cm")) + #adds margins (top, right, bottom, left)
            theme(
                panel.border = element_blank(),
                panel.grid.major = element_blank(),
                panel.grid.minor = element_blank(),
                panel.background = element_blank(),
                axis.line = element_line(colour = "black", size = 0.75)) +
            theme(axis.ticks.length=unit(0.1, "in")) + #Set tick length
            theme(axis.ticks = element_line(size = 0.75))  + #Set tick thickness +
            theme(axis.text.x = element_text(colour="black"), axis.text.y = element_text(colour="black")) +
            coord_cartesian(clip = "off")  #no clipping

        #toggles baselines on and off
        if (input$toggle.baseline == T) {
            p0 <-  p0 + geom_line(aes(x = T.K, y = low.T.baseline, color = id),
                                  size = input$size.baseline.melt, alpha = input$alpha.baseline.melt, linetype = "dashed") +
                geom_line(aes(x = T.K, y = high.T.baseline, color = id),
                          size = input$size.baseline.melt, alpha = input$alpha.baseline.melt, linetype = "dashed")
        } else { p0 }

        p0 <- melt.palette.modifier(plot = p0)

        return(p0)
        # }
    })

    output$p.raw.melt.fit <- renderPlot({
        if(file.toggle()=='no') {return(NULL)}
        else {
            p.raw.melt.fit()
        }
    })

    #outputs a plot of the modeled folded fraction
    p.folded.modeled <- reactive({
        if(file.toggle()=='no') {return(NULL)}
        else {
            p44 <- ggplot(fit.melt.result.df()) +
                geom_point(aes(T.K, folded.fraction, color = id),
                           size = input$size.dot.melt-2, alpha = input$alpha.dot.melt,
                           shape = 16) + #plots the experimental data
                ylab("folded fraction") + #modifies axes titles
                xlab("Temperature (K)") +
                # scale_y_continuous(limits=c(-0.1,1.1), breaks = c(0, 0.25, 0.5, 0.75, 1.0)) +
                labs(color="id") +
                # scale_color_d3(palette = "category20") +
                theme(axis.text=element_text(size=12), axis.title=element_text(size=16,face="bold")) + #axis style
                theme(axis.text.x = element_text(color = "black", size = 14, angle = 0),
                      axis.text.y = element_text(color = "black", size = 14, angle = 0)) + #axis labels style
                theme(legend.position="right",
                      legend.box = "vertical",
                      legend.title = element_text(size=14,
                                                  face="bold"),
                      legend.key = element_rect(fill = "white"),
                      legend.text = element_text(size=12,
                                                 face="bold")) +
                theme(plot.margin=unit(c(0.5,0.5,0.5,0.5),"cm")) + #adds margins (top, right, bottom, left)
                theme(
                    panel.border = element_blank(),
                    panel.grid.major = element_blank(),
                    panel.grid.minor = element_blank(),
                    panel.background = element_blank(),
                    axis.line = element_line(colour = "black", size = 0.75)) +
                theme(axis.ticks.length=unit(0.1, "in")) + #Set tick length
                theme(axis.ticks = element_line(size = 0.75))  + #Set tick thickness +
                theme(axis.text.x = element_text(colour="black"), axis.text.y = element_text(colour="black")) +
                coord_cartesian(clip = "off")  #no clipping

            p44 <- melt.palette.modifier(plot = p44)

            return(p44)
        }
    })

    output$p.folded.modeled <- renderPlotly({
        if(file.toggle()=='no') {return(NULL)}
        else {
            p.folded.modeled()
        }
    })

    #plots the baseline subtracted data
    p.folded.melt.fit <- reactive({
        if(file.toggle()=='no'){return(NULL)}
        else {
            p43 <- ggplot(fit.melt.result.df()) +
                geom_point(aes(T.K, folded.fraction.base, color = id),
                           size = input$size.dot.melt, alpha = input$alpha.dot.melt,
                           shape = 16) + #plots the experimental data
                ylab(bquote(bold("folded fraction"))) + #modifies axes titles
                xlab("Temperature (K)") +
                # scale_y_continuous(limits=c(-0.1,1.1), breaks = c(0, 0.25, 0.5, 0.75, 1.0)) +
                labs(color="id") +
                # scale_color_d3(palette = "category20") +
                theme(axis.text=element_text(size=12), axis.title=element_text(size=16,face="bold")) + #axis style
                theme(axis.text.x = element_text(color = "black", size = 14, angle = 0),
                      axis.text.y = element_text(color = "black", size = 14, angle = 0)) + #axis labels style
                theme(legend.position="right",
                      legend.box = "vertical",
                      legend.title = element_text(size=14,
                                                  face="bold"),
                      legend.key = element_rect(fill = "white"),
                      legend.text = element_text(size=12,
                                                 face="bold")) +
                theme(plot.margin=unit(c(0.5,0.5,0.5,0.5),"cm")) + #adds margins (top, right, bottom, left)
                theme(
                    panel.border = element_blank(),
                    panel.grid.major = element_blank(),
                    panel.grid.minor = element_blank(),
                    panel.background = element_blank(),
                    axis.line = element_line(colour = "black", size = 0.75)) +
                theme(axis.ticks.length=unit(0.1, "in")) + #Set tick length
                theme(axis.ticks = element_line(size = 0.75))  + #Set tick thickness +
                theme(axis.text.x = element_text(colour="black"), axis.text.y = element_text(colour="black")) +
                coord_cartesian(clip = "off")  #no clipping

            p43 <- melt.palette.modifier(plot = p43)

            return(p43)
        }
    })

    output$p.folded.melt.fit <- renderPlot({
        if(file.toggle()=='no') {return(NULL)}
        else {
            p.folded.melt.fit()
        }
    })


    #9-folded fraction intersect----

    theta.intersect <- reactive({
        fit.melt.result.df() %>%
            select(id, T.K, folded.fraction.base) %>%
            group_by(id) %>%
            summarise(
                tm.inter = approxfun(x = folded.fraction.base[T.K>290], y = T.K[T.K>290])(0.5),
                tm.inter.c = tm.inter-273.15,
                theta.temp = approxfun(x = T.K, y = folded.fraction.base)(input$temp.ff+273.15)
            )
    })


    #10-folded fraction derivatives----

    theta.deriv <- reactive({

        theta.deriv <- fit.melt.result.df() %>%
            select(id, ramp, T.K, folded.fraction.base) %>%
            mutate(recip.T = 1/T.K)

        spl.drv <- theta.deriv %>%
            group_by(id) %>%
            mutate(rM = abs(
                predict(
                    sm.spline(recip.T, folded.fraction.base), #automated polynomial smoothing
                    recip.T, 1) #get deriv against temperature from polynomial function
            ))

        return(spl.drv)
    })


    output$theta.deriv <- renderPlot({

        p77 <- ggplot(theta.deriv(), aes(recip.T, rM, color = id, shape = ramp)) +
            geom_point(size = input$size.dot.melt, alpha = input$alpha.dot.melt) +
            theme_pander() +
            scale_color_d3(palette = "category20") +
            xlab("Temperature (K)") +
            ylab(bquote(Delta*theta*'/'*Delta*T^-1~NA))

        # p46 <- melt.palette.modifier(plot = p46)

        return(p77)

    })


    theta.deriv.max <- reactive({
        theta.deriv() %>%
            group_by(id) %>%
            filter(rM == max(rM)) %>%
            summarise(tm.deriv = 1/recip.T,
                      tm.deriv.c = tm.deriv-273.15)
    })



    # 11-baseline generator----

    ## a-choice of baseline ranges----
    output$p.melt.filtered.base <- renderPlot({
        if(file.toggle()=='no') {return(NULL)}
        else {
            p666 <- ggplot(data = melt.filtered(),
                           aes(x = T.K-273.15, y = abs.melt, color = id, shape = ramp)) +
                geom_point(size = input$size.dot.melt, alpha = input$alpha.dot.melt) +
                geom_vline(aes(xintercept = input$start.low[1]),
                           color = 'springgreen4',
                           size = 1) +
                geom_vline(aes(xintercept = input$start.low[2]),
                           linetype = 'dashed',
                           color = 'springgreen4',
                           size = 1) +
                geom_vline(aes(xintercept = input$end.low[1]),
                           linetype = 'dashed',
                           color = 'purple4',
                           size = 1) +
                geom_vline(aes(xintercept = input$end.low[2]),
                           color = 'purple4',
                           size = 1) +
                geom_vline(aes(xintercept = input$start.high[1]),
                           color = 'springgreen4',
                           size = 1) +
                geom_vline(aes(xintercept = input$start.high[2]),
                           linetype = 'dashed',
                           color = 'springgreen4',
                           size = 1) +
                geom_vline(aes(xintercept = input$end.high[1]),
                           linetype = 'dashed',
                           color = 'purple4',
                           size = 1) +
                geom_vline(aes(xintercept = input$end.high[2]),
                           color = 'purple4',
                           size = 1) +
                scale_color_d3() +
                theme_pander() +
                xlab("Temperature (°C)") +
                ylab(bquote(epsilon*' ('*M^-1~cm^-1*')'))

            return(p666)
        }
    })

    ## b-baseline range set generation----

    baseline.set <- reactive({
        basegenR(start.low = input$start.low+273.15,
                 end.low = input$end.low+273.15,
                 start.high = input$start.high+273.15,
                 end.high = input$end.high+273.15,
                 t.range = input$t.range,
                 nb.spl = input$nb.spl)
    })

    ## c-baseline generation----

    output$baseline.set <- renderDT(
        baseline.set() %>%
            mutate(range.low = max.low-min.low,
                   range.high = max.high - min.high) %>%
            datatable(extensions = c('Buttons', 'Responsive', 'Scroller'),
                      rownames = F,
                      escape = T,
                      filter = 'top',
                      autoHideNavigation = T,
                      options = list(
                          deferRender = TRUE,
                          scrollY = 200,
                          scroller = F,
                          pageLength = 25,
                          autoWidth = F,
                          dom = 'Bfrtip', #button position
                          buttons = c('copy', 'csv', 'excel', 'colvis'), #buttons
                          columnDefs = list(list(visible=FALSE, targets=c(0)))
                      )
            )
    )


    melt.filtered.prep <- reactive({
        melt.filtered() %>%
            ungroup() %>%
            mutate(
                temp = T.K,
                abs = abs.melt,
                oligo.id = id,
                group.id = paste0(oligo, "/", comment, ' [', rep,']')
            ) %>%
            # group_by(oligo, rep, ramp) %>%
            # mutate(blob = if_else(
            #     length(unique(id))>1,paste(oligo, comment), oligo
            # )) %>%
            # ungroup() %>%
            select(oligo.id, group.id, oligo, ramp, temp, abs, rep, comment)
    })

    baseline.gen <- eventReactive(input$bttn.baselines,{

        #initialization
        # buffer.oligo <- data.frame()
        # buffer.bases <- data.frame()
        bases <- data.frame()
        melt.filtered <- melt.filtered.prep()

        # for (i in unique(melt.filtered$rep)) {
        #     #loop on replicates
        #     melt.filtered.rep <- melt.filtered %>%
        #         filter(rep == i)
        #
        #     for (j in unique(melt.filtered$oligo)) {
        #         #loop on oligos
        #         melt.filtered.oligo <- melt.filtered.rep %>%
        #             filter(oligo == j)
        #
        #         gen <- base.plottR(
        #             abs.data = melt.filtered.oligo,
        #             temp.df = baseline.set()
        #         )
        #         #data gathering
        #         buffer.oligo <- rbind(buffer.oligo, gen)
        #     }
        #     #data gathering
        #     bases <- rbind(buffer.bases, buffer.oligo)
        # }

        for (i in unique(melt.filtered.prep()$group.id)) {
            melt.filtered.group <- melt.filtered.prep() %>%
                filter(group.id == i)

            gen <- base.plottR(
                abs.data = melt.filtered.group,
                temp.df = baseline.set()
            )

            bases <- rbind(bases, gen)
        }

        #creation of median groups for coloring purpose
        bases <- bases %>%
            group_by(ramp, id, oligo, rep) %>%
            mutate(id.bl = max(med.bl)-min(med.bl))

        return(bases)
    })


    output$baseline.gen <- renderDT(

        baseline.gen() %>%
            datatable(extensions = c('Buttons', 'Responsive', 'Scroller'),
                      rownames = F,
                      escape = T,
                      filter = 'top',
                      autoHideNavigation = T,
                      options = list(
                          deferRender = TRUE,
                          scrollY = 200,
                          scroller = F,
                          pageLength = 25,
                          autoWidth = F,
                          dom = 'Bfrtip', #button position
                          buttons = c('copy', 'csv', 'excel', 'colvis'), #buttons
                          columnDefs = list(list(visible=FALSE, targets=c(0)))
                      )
            )
    )


    p.bases <- reactive({
        baseline.gen() %>%
            # filter(oligo == input.oligo,
            #        rep %in% replicates) %>%
            ggplot(aes(x = temp-273.15)) +
            geom_point(aes(y = abs),
                       color = 'grey',
                       show.legend = FALSE) +
            geom_line(aes(y = med.bl, color = id.bl, group = factor(id.bl)),
                      linetype = 'dashed',
                      size = 1, alpha = 0.75,
                      show.legend = TRUE) +
            geom_line(aes(y = low.bl, color = id.bl, group = (id.bl)),
                      size = 1, alpha = 0.75,
                      show.legend = FALSE) +
            geom_line(aes(y = high.bl, color = id.bl, group = (id.bl)),
                      size = 1, alpha = 0.75,
                      show.legend = FALSE) +
            facet_grid(paste(comment, rep, sep = '/')~paste(oligo, ramp, sep = '/'),
                       scales = 'free_y') +
            theme_pander() +
            scale_color_viridis_c(name = bquote(Delta*epsilon*' ('*M^-1~cm^-1*')')) +
            labs(x = "T (°C)", y = bquote(epsilon*' ('*M^-1~cm^-1*')'))

    })

    output$p.bases <- renderPlot({
        p.bases()
    })


    output$p.bases.ui <- renderUI({
        plotOutput("p.bases",
                   width = '100%',
                   height = 600
        )
    })


    # 12-Tm and theta extraction----

    tm.median <- reactive({

        #initialization
        tms <- data.frame()
        # buffer.oligo <- data.frame()
        # buffer.tm <- data.frame()
        # melt.filtered <- melt.filtered.prep()

        # for (i in unique(melt.filtered$rep)) {
        #     #loop on replicates
        #     melt.filtered.rep <- melt.filtered %>%
        #         filter(rep == i)
        #
        #     for (j in unique(melt.filtered$oligo)) {
        #         #loop on oligos
        #         melt.filtered.oligo <- melt.filtered.rep %>%
        #             filter(oligo == j)
        #
        #         gen <- tm.extractoR(
        #             abs.data = melt.filtered.rep,
        #             temp.df = baseline.set(),
        #             theta.base.temp = input$theta.base.temp
        #         ) %>%
        #             mutate(oligo = j,
        #                    rep = i)
        #         #data gathering
        #         buffer.oligo <- rbind(buffer.oligo, gen)
        #     }
        #     #data gathering
        #     tms <- rbind(buffer.tm, buffer.oligo)
        # }


        for (i in unique(melt.filtered.prep()$group.id)) {
            melt.filtered.group <- melt.filtered.prep() %>%
                filter(group.id == i)

            gen <- tm.extractoR(
                abs.data = melt.filtered.group,
                temp.df = baseline.set(),
                theta.base.temp = input$theta.base.temp
            ) %>%
                mutate(oligo = unique(melt.filtered.group$oligo),
                       rep = unique(melt.filtered.group$rep),
                       group.id = i,
                       comment = unique(melt.filtered.group$comment)
                       # oligo.id = unique(melt.filtered.group$oligo.id)
                )

            tms <- rbind(tms, gen)
        }







        tms <- tms %>%
            mutate(tm.c = tm-273.15)

        return(tms)
    })

    output$tm.median <- renderDT({
        tm.median() %>%
            datatable(
                extensions = c('Buttons', 'Responsive', 'Scroller'),
                rownames = F,
                escape = T,
                filter = 'top',
                autoHideNavigation = T,
                options = list(
                    deferRender = TRUE,
                    scrollY = 200,
                    scroller = F,
                    pageLength = 25,
                    autoWidth = F,
                    dom = 'Bfrtip', #button position
                    buttons = c('copy', 'csv', 'excel', 'colvis'), #buttons
                    columnDefs = list(list(visible=FALSE, targets=c(0)))
                )
            ) %>%
            formatRound(c('tm'),
                        digits = 2)
    })

    output$p.tm.median <- renderPlot({
        tm.median() %>%
            ggplot(aes(x = rep, y = tm.c,
                       fill = ramp)) +
            geom_violin(alpha = 0.7,
                        trim = TRUE,
                        size = 0.75,
                        show.legend = FALSE) +
            # geom_boxplot(alpha = 0.7,
            #              outlier.shape = NA) +
            facet_grid(comment~oligo,
                       scales = 'free_y') +
            theme_pander() +
            scale_fill_d3() +
            labs(x = '',
                 y = bquote(T[m]~(K)))
    })

    output$p.theta <- renderPlot({
        tm.median() %>%
            ggplot(aes(x = rep, y = theta.t,
                       fill = ramp)) +
            geom_violin(alpha = 0.7,
                        trim = TRUE,
                        size = 0.75,
                        show.legend = TRUE) +
            theme_pander() +
            theme(
                legend.position = 'bottom',
                legend.direction = 'horizontal'
            )+
            labs(x = '',
                 y = bquote(theta)) +
            scale_fill_d3() +
            # stat_summary(position = position_dodge(width = .9),
            #              fun.data = data_summary,
            #              color = 'grey30',
            #              size = 0.75,
            #              show.legend = FALSE,
            #              alpha = 0.1) +
            facet_grid(comment~oligo,
                       scales = 'free_y')
    })


    #97-summary----

    fit.melt.result.summary <- reactive({
        # if(is.null(input$input.file)) {return(NULL)}
        # else {
        fit.melt.result.df() %>%
            select(id, oligo, ramp, comment, rep, init.Tm, fit.Tm.K, fit.Tm.C, P2SD, DeltaH, DeltaS) %>%
            left_join(theta.intersect(), by = c('id')) %>%
            left_join(theta.deriv.max(), by = c('id')) %>%
            mutate(init.Tm = init.Tm - 273.15) %>%
            distinct() %>%
            group_by(id) %>%
            mutate(DeltaG = DeltaH - input$temp.therm * DeltaS) %>%
            group_by(oligo, ramp, comment) %>%
            mutate(mean.Tm.C = mean(fit.Tm.C),
                   sd.Tm.C = SD(fit.Tm.C))
        # }
    })


    #summary table output
    output$fit.melt.result.summary <- DT::renderDT(server=FALSE,{
        if(file.toggle()=='no') {return(NULL)}
        else {
            datatable(
                fit.melt.result.summary() %>%
                    setcolorder(
                        c('id', 'oligo', 'comment', 'ramp', 'rep',
                          'init.Tm', 'fit.Tm.K', 'fit.Tm.C', 'tm.inter.c',
                          'tm.deriv.c', 'theta.temp'
                        )
                    ),
                extensions = c('Buttons', 'Responsive', 'Scroller'),
                colnames = c(
                    "Tm: max(dA/dT) (°C)" = "init.Tm",
                    "Tm: fit (K)" = "fit.Tm.K",
                    "Tm: fit (°C)" = "fit.Tm.C",
                    "Tm: intersect (K)" = 'tm.inter',
                    "Tm: intersect (°C)" = 'tm.inter.c',
                    "Tm: derivative (K)" = 'tm.deriv',
                    'Tm: derivative (°C)' = 'tm.deriv.c',
                    "SD Tm (°C)" = "sd.Tm.C",
                    "Mean Tm (°C)" = "mean.Tm.C",
                    'Replicate'= 'rep',
                    'Oligonucleotide' = 'oligo',
                    'Buffer' = 'comment',
                    'Ramp' = 'ramp',
                    'Folded fraction' = 'theta.temp'
                ),
                rownames = F,
                escape = T,
                filter = 'top',
                autoHideNavigation = T,
                options = list(
                    deferRender = TRUE,
                    scrollY = 200,
                    scroller = F,
                    pageLength = 25,
                    autoWidth = F,
                    dom = 'Bfrtip', #button position
                    buttons = c('copy', 'csv', 'excel', 'colvis'), #buttons
                    columnDefs = list(list(visible=FALSE, targets=c(0,11:18)))
                )
            ) %>%
                formatRound(c("Tm: fit (K)", "Tm: fit (°C)",
                              'Tm: intersect (K)', 'Tm: intersect (°C)',
                              'Folded fraction',
                              "DeltaH", "DeltaS", "DeltaG",
                              "Mean Tm (°C)", "SD Tm (°C)"),
                            digits = 2) %>%
                formatRound(c("Tm: derivative (K)", "Tm: derivative (°C)"),
                            digits = 1)
        }
    })

    #output boxplot of summary
    fit.melt.result.plot <- reactive({
        if(file.toggle()=='no') {return(NULL)}
        else {
            p47 <- fit.melt.result.summary() %>%
                select(id, oligo, comment, ramp, rep, fit.Tm.C, tm.inter.c, tm.deriv.c, init.Tm) %>%
                pivot_longer(
                    cols = c(6:9),
                    names_to = 'method',
                    values_to = 'tm'
                ) %>%
                mutate(method = case_when(
                    method == 'init.Tm' ~ 'max(dA/dT)',
                    method == 'fit.Tm.C' ~ 'fit (parameter)',
                    method == 'tm.inter.c' ~ 'fit (intersect)',
                    method == 'tm.deriv.c' ~ 'max(dtheta/dT-1)'
                )) %>%
                ggplot() +
                geom_boxplot(aes(x = paste(oligo, comment, sep = "-"), y = tm),
                             color = "grey75") +
                geom_point(aes(x = paste(oligo, comment, sep = "-"), y = tm, color = ramp, shape = factor(rep)),
                           size = input$size.dot.melt, alpha = input$alpha.dot.melt) +
                theme_pander() +
                xlab("") +
                ylab("Melting temperature (°C)") +
                facet_wrap(~method)

            p47 <- melt.palette.modifier(plot = p47)

            p47 <- p47 + scale_color_discrete(labels = c("cooling", "heating"))

            return(p47)
        }
    })

    output$fit.melt.result.plot <- renderPlot({
        if(file.toggle()=='no') {return(NULL)}
        else {
            fit.melt.result.plot()
        }
    })

    #98-downloads------

    output$dwn.melt.fit <- downloadHandler(
        filename = function() { paste("fit", '.png', sep='') },
        content = function(file) {
            ggsave(file, plot = p.raw.melt.fit(), device = "png")
        }
    )

    output$dwn.melt.model <- downloadHandler(
        filename = function() { paste("model", '.png', sep='') },
        content = function(file) {
            ggsave(file, plot = p.folded.modeled(), device = "png")
        }
    )

    output$dwn.melt.folded <- downloadHandler(
        filename = function() { paste("folded", '.png', sep='') },
        content = function(file) {
            ggsave(file, plot = p.folded.melt.fit(), device = "png")
        }
    )

    output$dwn.melt.Tm <- downloadHandler(
        filename = function() { paste("Tm", '.png', sep='') },
        content = function(file) {
            ggsave(file, plot = fit.melt.result.plot(), device = "png")
        }
    )






    #################X__X#################

    #99-Palettes-------
    #palettes for import data
    palette.modifier <- function(plot = NULL){
        if (input$select.import.palette.fam == 'd3') {
            plot <- plot + scale_color_d3(palette = input$select.import.palette)
        } else {
            if (input$select.import.palette.fam == "Brewer - qualitative") {
                plot <- plot + scale_color_brewer(palette = input$select.import.palette)
            } else{
                if (input$select.import.palette.fam == "Brewer - sequential") {
                    plot <- plot + scale_color_brewer(palette = input$select.import.palette)
                } else {
                    if (input$select.import.palette.fam == "Brewer - diverging") {
                        plot <- plot + scale_color_brewer(palette = input$select.import.palette)
                    } else {
                        if (input$select.import.palette.fam == "NPG") {
                            plot <- plot + scale_color_npg()
                        } else {
                            if (input$select.import.palette.fam == "AAAS") {
                                plot <- plot + scale_color_aaas()
                            } else {
                                if (input$select.import.palette.fam == "NEJM") {
                                    plot <- plot + scale_color_nejm()
                                } else {
                                    if (input$select.import.palette.fam == "Lancet") {
                                        plot <- plot + scale_color_lancet()
                                    } else {
                                        if (input$select.import.palette.fam == "JAMA") {
                                            plot <- plot + scale_color_jama()
                                        } else {
                                            if (input$select.import.palette.fam == "JCO") {
                                                plot <- plot + scale_color_jco()
                                            } else {
                                                if (input$select.import.palette.fam == "UCSCGB") {
                                                    plot <- plot + scale_color_ucscgb()
                                                } else {
                                                    if (input$select.import.palette.fam == "LocusZoom") {
                                                        plot <- plot + scale_color_locuszoom()
                                                    } else {
                                                        if (input$select.import.palette.fam == "IGV") {
                                                            plot <- plot + scale_color_igv(palette = input$select.import.palette)
                                                        } else {
                                                            if (input$select.import.palette.fam == "UChicago") {
                                                                plot <- plot + scale_color_uchicago(palette = input$select.import.palette)
                                                            }
                                                        }
                                                    }
                                                }
                                            }
                                        }
                                    }
                                }
                            }
                        }
                    }
                }
            }
        }
    }

    #palettes for database
    palette.modifier.db <- function(plot = NULL){
        if (input$select.import.palette.fam.db == 'd3') {
            plot <- plot + scale_color_d3(palette = input$select.import.palette.db) +
                scale_fill_d3(palette = input$select.import.palette.db)
        } else {
            if (input$select.import.palette.fam.db == "Brewer - qualitative") {
                plot <- plot + scale_color_brewer(palette = input$select.import.palette.db) +
                    scale_fill_brewer(palette = input$select.import.palette.db)
            } else{
                if (input$select.import.palette.fam.db == "Brewer - sequential") {
                    plot <- plot + scale_color_brewer(palette = input$select.import.palette.db) +
                        scale_fill_brewer(palette = input$select.import.palette.db)
                } else {
                    if (input$select.import.palette.fam.db == "Brewer - diverging") {
                        plot <- plot + scale_color_brewer(palette = input$select.import.palette.db) +
                            scale_fill_brewer(palette = input$select.import.palette.db)
                    } else {
                        if (input$select.import.palette.fam.db == "NPG") {
                            plot <- plot + scale_color_npg() +
                                scale_fill_npg()
                        } else {
                            if (input$select.import.palette.fam.db == "AAAS") {
                                plot <- plot + scale_color_aaas() +
                                    scale_fill_aaas()
                            } else {
                                if (input$select.import.palette.fam.db == "NEJM") {
                                    plot <- plot + scale_color_nejm() +
                                        scale_fill_nejm()
                                } else {
                                    if (input$select.import.palette.fam.db == "Lancet") {
                                        plot <- plot + scale_color_lancet() +
                                            scale_fill_lancet()
                                    } else {
                                        if (input$select.import.palette.fam.db == "JAMA") {
                                            plot <- plot + scale_color_jama() +
                                                scale_fill_jama()
                                        } else {
                                            if (input$select.import.palette.fam.db == "JCO") {
                                                plot <- plot + scale_color_jco() +
                                                    scale_fill_jco()
                                            } else {
                                                if (input$select.import.palette.fam.db == "UCSCGB") {
                                                    plot <- plot + scale_color_ucscgb() +
                                                        scale_fill_ucscgb()
                                                } else {
                                                    if (input$select.import.palette.fam.db == "LocusZoom") {
                                                        plot <- plot + scale_color_locuszoom() +
                                                            scale_fill_locuszoom()
                                                    } else {
                                                        if (input$select.import.palette.fam.db == "IGV") {
                                                            plot <- plot + scale_color_igv(palette = input$select.import.palette.db) +
                                                                scale_fill_igv(palette = input$select.import.palette.db)
                                                        } else {
                                                            if (input$select.import.palette.fam.db == "UChicago") {
                                                                plot <- plot + scale_color_uchicago(palette = input$select.import.palette.db) +
                                                                    scale_fill_uchicago(palette = input$select.import.palette.db)
                                                            }
                                                        }
                                                    }
                                                }
                                            }
                                        }
                                    }
                                }
                            }
                        }
                    }
                }
            }
        }
    }

    #Palette family selector
    #for import data
    output$select.import.palette.fam <- renderUI({
        pickerInput("select.import.palette.fam",
                    label = "Choose palette family",
                    choices = list("d3",
                                   "Brewer - qualitative", "Brewer - diverging", "Brewer - sequential",
                                   "AAAS", "IGV", "JAMA", "JCO", "Lancet", 'LocusZoom', 'NEJM', "NPG",
                                   "UChicago", "UCSCGB"),
                    multiple = F
        )
    })

    #for database
    output$select.import.palette.fam.db <- renderUI({
        pickerInput("select.import.palette.fam.db",
                    label = "Choose palette family",
                    choices = list("d3",
                                   "Brewer - qualitative", "Brewer - diverging", "Brewer - sequential",
                                   "AAAS", "IGV", "JAMA", "JCO", "Lancet", 'LocusZoom', 'NEJM', "NPG",
                                   "UChicago", "UCSCGB"),
                    multiple = F
        )
    })

    #Palette subcategory selector
    #for import data
    output$select.import.palette <- renderUI({
        if (input$select.import.palette.fam == 'd3') {
            pickerInput("select.import.palette",
                        label = "Choose palette",
                        choices = list("d3a" = "category20",
                                       "d3b" = "category20b",
                                       "d3c" = "category20c"),
                        multiple = F
            )
        } else {
            if (input$select.import.palette.fam == 'Brewer - qualitative') {
                pickerInput("select.import.palette",
                            label = "Choose palette",
                            choices = list("Accent", "Dark2", "Paired",
                                           "Pastel1", "Pastel2", "Set1",
                                           "Set2", "Set3"),
                            multiple = F
                )
            } else {
                if (input$select.import.palette.fam == 'Brewer - diverging') {
                    pickerInput("select.import.palette",
                                label = "Choose palette",
                                choices = list("BrBG", 'PiYG', 'PRGn',
                                               'PuOr', 'RdBu', 'RdGy',
                                               'RdYlBu', 'RdYlGn', 'Spectral'),
                                multiple = F
                    )
                } else {
                    if (input$select.import.palette.fam == 'Brewer - sequential') {
                        pickerInput("select.import.palette",
                                    label = "Choose palette",
                                    choices = list('Blues', 'BuGn', 'BuPu', 'GnBu',
                                                   'Greens', 'Greys', 'Oranges', 'OrRd',
                                                   'PuBu', 'PuBuGn', 'PuRd', 'Purples', 'RdPu',
                                                   'Reds', 'YlGn', 'YlGnBu', 'YlOrBr', 'YlOrRd'),
                                    multiple = F
                        )
                    } else {
                        if (input$select.import.palette.fam == 'IGV') {
                            pickerInput("select.import.palette",
                                        label = "Choose palette",
                                        choices = list("default", "alternating"),
                                        multiple = F
                            )
                        } else {
                            if (input$select.import.palette.fam == 'UChicago') {
                                pickerInput("select.import.palette",
                                            label = "Choose palette",
                                            choices = list("default", "light", "dark"),
                                            multiple = F
                                )
                            } else {
                                return(NULL)
                            }
                        }
                    }
                }
            }
        }
    })

    #for database
    output$select.import.palette.db <- renderUI({
        if (input$select.import.palette.fam.db == 'd3') {
            pickerInput("select.import.palette.db",
                        label = "Choose palette",
                        choices = list("d3a" = "category20",
                                       "d3b" = "category20b",
                                       "d3c" = "category20c"),
                        multiple = F
            )
        } else {
            if (input$select.import.palette.fam.db == 'Brewer - qualitative') {
                pickerInput("select.import.palette.db",
                            label = "Choose palette",
                            choices = list("Accent", "Dark2", "Paired",
                                           "Pastel1", "Pastel2", "Set1",
                                           "Set2", "Set3"),
                            multiple = F
                )
            } else {
                if (input$select.import.palette.fam.db == 'Brewer - diverging') {
                    pickerInput("select.import.palette.db",
                                label = "Choose palette",
                                choices = list("BrBG", 'PiYG', 'PRGn',
                                               'PuOr', 'RdBu', 'RdGy',
                                               'RdYlBu', 'RdYlGn', 'Spectral'),
                                multiple = F
                    )
                } else {
                    if (input$select.import.palette.fam.db == 'Brewer - sequential') {
                        pickerInput("select.import.palette.db",
                                    label = "Choose palette",
                                    choices = list('Blues', 'BuGn', 'BuPu', 'GnBu',
                                                   'Greens', 'Greys', 'Oranges', 'OrRd',
                                                   'PuBu', 'PuBuGn', 'PuRd', 'Purples', 'RdPu',
                                                   'Reds', 'YlGn', 'YlGnBu', 'YlOrBr', 'YlOrRd'),
                                    multiple = F
                        )
                    } else {
                        if (input$select.import.palette.fam.db == 'IGV') {
                            pickerInput("select.import.palette.db",
                                        label = "Choose palette",
                                        choices = list("default", "alternating"),
                                        multiple = F
                            )
                        } else {
                            if (input$select.import.palette.fam.db == 'UChicago') {
                                pickerInput("select.import.palette.db",
                                            label = "Choose palette",
                                            choices = list("default", "light", "dark"),
                                            multiple = F
                                )
                            } else {
                                return(NULL)
                            }
                        }
                    }
                }
            }
        }
    })

    # #MELTING PALETTE
    melt.palette.modifier <- function(plot = NULL){
        if (input$select.melting.palette.fam == 'd3') {
            plot <- plot + scale_color_d3(palette = input$select.melting.palette,
                                          labels = tm.init.change()$legend)
        } else {
            if (input$select.melting.palette.fam == "Brewer - qualitative") {
                plot <- plot + scale_color_brewer(palette = input$select.melting.palette,
                                                  labels = tm.init.change()$legend)
            } else{
                if (input$select.melting.palette.fam == "Brewer - sequential") {
                    plot <- plot + scale_color_brewer(palette = input$select.melting.palette,
                                                      labels = tm.init.change()$legend)
                } else {
                    if (input$select.melting.palette.fam == "Brewer - diverging") {
                        plot <- plot + scale_color_brewer(palette = input$select.melting.palette,
                                                          labels = tm.init.change()$legend)
                    } else {
                        if (input$select.melting.palette.fam == "NPG") {
                            plot <- plot + scale_color_npg(labels = tm.init.change()$legend)
                        } else {
                            if (input$select.melting.palette.fam == "AAAS") {
                                plot <- plot + scale_color_aaas(labels = tm.init.change()$legend)
                            } else {
                                if (input$select.melting.palette.fam == "NEJM") {
                                    plot <- plot + scale_color_nejm(labels = tm.init.change()$legend)
                                } else {
                                    if (input$select.melting.palette.fam == "Lancet") {
                                        plot <- plot + scale_color_lancet(labels = tm.init.change()$legend)
                                    } else {
                                        if (input$select.melting.palette.fam == "JAMA") {
                                            plot <- plot + scale_color_jama(labels = tm.init.change()$legend)
                                        } else {
                                            if (input$select.melting.palette.fam == "JCO") {
                                                plot <- plot + scale_color_jco(labels = tm.init.change()$legend)
                                            } else {
                                                if (input$select.melting.palette.fam == "UCSCGB") {
                                                    plot <- plot + scale_color_ucscgb(labels = tm.init.change()$legend)
                                                } else {
                                                    if (input$select.melting.palette.fam == "LocusZoom") {
                                                        plot <- plot + scale_color_locuszoom(labels = tm.init.change()$legend)
                                                    } else {
                                                        if (input$select.melting.palette.fam == "IGV") {
                                                            plot <- plot + scale_color_igv(palette = input$select.melting.palette,
                                                                                           labels = tm.init.change()$legend)
                                                        } else {
                                                            if (input$select.melting.palette.fam == "UChicago") {
                                                                plot <- plot + scale_color_uchicago(palette = input$select.melting.palette,
                                                                                                    labels = tm.init.change()$legend)
                                                            }
                                                        }
                                                    }
                                                }
                                            }
                                        }
                                    }
                                }
                            }
                        }
                    }
                }
            }
        }
    }

    #Palette family selector
    output$select.melting.palette.fam <- renderUI({
        pickerInput("select.melting.palette.fam",
                    label = "Choose palette family",
                    choices = list("d3",
                                   "Brewer - qualitative", "Brewer - diverging", "Brewer - sequential",
                                   "AAAS", "IGV", "JAMA", "JCO", "Lancet", 'LocusZoom', 'NEJM', "NPG",
                                   "UChicago", "UCSCGB"),
                    multiple = F
        )
    })

    #Palette subcategory selector
    output$select.melting.palette <- renderUI({

        if (input$select.melting.palette.fam == 'd3') {
            pickerInput("select.melting.palette",
                        label = "Choose palette",
                        choices = list("d3a" = "category20",
                                       "d3b" = "category20b",
                                       "d3c" = "category20c"),
                        multiple = F
            )
        } else {
            if (input$select.melting.palette.fam == 'Brewer - qualitative') {
                pickerInput("select.melting.palette",
                            label = "Choose palette",
                            choices = list("Accent", "Dark2", "Paired",
                                           "Pastel1", "Pastel2", "Set1",
                                           "Set2", "Set3"),
                            multiple = F
                )
            } else {
                if (input$select.melting.palette.fam == 'Brewer - diverging') {
                    pickerInput("select.melting.palette",
                                label = "Choose palette",
                                choices = list("BrBG", 'PiYG', 'PRGn',
                                               'PuOr', 'RdBu', 'RdGy',
                                               'RdYlBu', 'RdYlGn', 'Spectral'),
                                multiple = F
                    )
                } else {
                    if (input$select.melting.palette.fam == 'Brewer - sequential') {
                        pickerInput("select.melting.palette",
                                    label = "Choose palette",
                                    choices = list('Blues', 'BuGn', 'BuPu', 'GnBu',
                                                   'Greens', 'Greys', 'Oranges', 'OrRd',
                                                   'PuBu', 'PuBuGn', 'PuRd', 'Purples', 'RdPu',
                                                   'Reds', 'YlGn', 'YlGnBu', 'YlOrBr', 'YlOrRd'),
                                    multiple = F
                        )
                    } else {
                        if (input$select.melting.palette.fam == 'IGV') {
                            pickerInput("select.melting.palette",
                                        label = "Choose palette",
                                        choices = list("default", "alternating"),
                                        multiple = F
                            )
                        } else {
                            if (input$select.melting.palette.fam == 'UChicago') {
                                pickerInput("select.melting.palette",
                                            label = "Choose palette",
                                            choices = list("default", "light", "dark"),
                                            multiple = F
                                )
                            } else {
                                return(NULL)
                            }
                        }
                    }
                }
            }
        }

    })

}

# Run the application
shinyApp(ui = ui, server = server)

