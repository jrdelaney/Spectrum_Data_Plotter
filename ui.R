library(shiny)
library(ggplot2)
library(shinyjs)
library(shinyBS)
library(bslib)

fluidPage(
  
  theme = bs_theme(version = 5, bootswatch = "sketchy"),
  
  tags$head(
    tags$style(HTML('hr {border-top: 1px solid #666666;}'))
  ),
  
  titlePanel("Spectrum Data Plotter"),
  
  div(class="row",

  sidebarPanel(
    
    tags$head(
      tags$style(HTML('#plot_data{background-color:#9999FF; text-color:black; border-color:black; font-weight:bold}'))
    ),
    
    textAreaInput('pasted_data',"Paste your data here, with each sample in a column. Header will be used as sample name.", value = "" ,width = "67%", height = "67%", resize = "both"),
    fileInput('uploaded_data', 'Or upload your data', multiple = FALSE),
    downloadLink("example_data_download", "Download example input data"),
    checkboxInput('fairsubset_use',"FairSubset into equal sample sizes"),
      conditionalPanel(
        condition = "input.fairsubset_use == true ",
        selectInput('fairsubset_subset_setting',"Type of subset", choices = c("ks","mean","median"), selected = "ks"),
        textInput('fairsubset_manualN', "Optional: define N in each subset. If empty, uses smallest dataset"),
        helpText("For more info on FairSubset, please visit:"),
        uiOutput("fairsubset_url"),
        helpText("If you use FairSubset, please cite it in your publication.")
        ),
    
    hr(),
    h3("Desired Plots"),
    checkboxInput('dotplot_wanted',"Dot plot"),
    checkboxInput('average_error_plot',"Plot average +/- error"),
    checkboxInput('boxplot_wanted',"Boxplot"),
    bsTooltip(id = "boxplot_wanted", 
              title =  "Box represents interquartile range (IQR 25-75%), with median. Whiskers are last observation within 1.5*IQR, with outliers plotted beyond this range.",
              placement = "right", options = NULL
    ),
    checkboxInput('violin_wanted',"Violin plot"),


    
    actionButton('plot_data', 'Plot Data or Update with Settings'),
    actionButton("refresh", "Refresh All Inputs"),
    br(),
    hr(),
    h3("Optional plot settings"),
    sliderInput('plot_width', "Plot width", min = 0, max = 1000, value = 600),
    sliderInput('plot_height', "Plot height", min = 0, max = 2000, value = 375),
    checkboxInput('logarithmic_y_wanted',"Scale y-axis logarithmic"),
      conditionalPanel(
        condition = "input.logarithmic_y_wanted == true ",
        selectInput('logarithmic_scale',"Log base",choices = c("log10","log2"), selected ="10"),
        checkboxInput('logarithmic_ticks',"Display sub-log ticks", value = TRUE)
      ),
    checkboxInput('scientific_y_wanted',"Use scientific notation for y-axis values"),
    textInput('manual_ylims','Manually Set Y-limits. Format: "# to #", eg -2 to 20'),
    textInput('horizontal_lines','Manually create horizontal guide line(s). Format: "#,#", eg 0,4'),
    textInput('yaxis_title',"Y-axis title", value ="Data value (#)"),
    textInput('yaxis_label_size' ,"Y-axis label size", value ="16"),
    textInput('xaxis_title',"X-axis title", value ="Sample"),
    textInput('xaxis_rotation' ,"X-axis label rotation", value ="0"),
    textInput('xaxis_label_size' ,"X-axis label size", value ="16"),
    textInput('plot_title',"Plot title", value =""),
    textInput('axis_linewidth',"Axis line width", value ="1.25"),
    textInput('axis_linecolor',"Axis line color", value ="black"),
    selectInput('axis_linetype' ,"Axis line type", choices = c("solid","dashed","dotted","dotdash","longdash","twodash", "blank") , selected ="solid"),
    checkboxInput('y_ticks',"Display Y-axis ticks", value = TRUE),
    checkboxInput('y_tick_labels',"Display Y-axis tick labels", value = TRUE)
    
  ),
  

  mainPanel(
    shinythemes::themeSelector(), 
    conditionalPanel(
      condition = "output.executed != 'done' ",
      helpText('Please input your data.  For an example of how the data should be formatted, please click "Download example input data"'),
      uiOutput("delaneyapps_url"),
      br()
      
    ),
    
    conditionalPanel(
      condition = "output.executed == 'done' ",
      span(textOutput('warning'), style="color:red"),
      br(),
      uiOutput('ui_plot'),
      br()
    ),
    
    conditionalPanel(
      condition = "input.dotplot_wanted == true ",
      hr(),
      h3("Dotplot Options"),
      fluidRow(
        column(width = 2,
               textInput('dotplot_dotsize', "Dot size", value = "1"),
               textInput('dotplot_binwidth', "Stat binning", value = "0.05"),
               textInput('dotplot_stackratio', "Dot overlap", value = "1")
        ),
        column(width = 2,
               textInput('dotplot_fill', "Fill color(s)", value = "black"),
               textInput('dotplot_color', "Outline color(s)", value = "black"),
               selectInput('dotplot_stackdir',"Stacking choice", choices = c("center","centerwhole","up","down"), selected = "center")
        ),
        column(width = 2,
               textInput('dotplot_transparency', "Transparency (0-1)", value = "0")
        )
      ),
      br()
    ),
    
      conditionalPanel(
        condition = "input.average_error_plot == true ",
        hr(),
        h3("Average +/- Error Options"),
        fluidRow(
          column(width = 2,
                 selectInput('average_error_plot_average', "Average metric", choices = c("median","mean"), selected = "median"),
                 selectInput('average_error_plot_error', "Error metric", choices = c("standard deviation","standard error"), selected = "standard deviation"),
                 textInput('average_error_xshift', "Shift x", value = "0")
          ),
          column(width = 2,
                 textInput('average_error_mark_width', "Average mark width", value = "0.3"),
                 textInput('average_error_mark_thickness', "Average mark thickness", value = "1"),
                 textInput('average_error_mark_color', "Average mark color(s)", value = "black"),
                 textInput('average_error_mark_transparency', "Average mark transparency (0-1)", value = "0")
          ),
          column(width = 2,
                 textInput('average_error_line_width', "Error bar width", value = "0.2"),
                 textInput('average_error_line_thickness', "Error bar thickness", value = "0.5"),
                 textInput('average_error_line_color', "Error bar color(s)", value = "black"),
                 textInput('average_error_line_transparency', "Error bar transparency (0-1)", value = "0")
          )
        ),
        br()
      ),
    
      conditionalPanel(
        condition = "input.boxplot_wanted == true ",
        hr(),
        h3('Boxplot Options'),
        fluidRow(
          column(width = 2,
                 textInput('boxplot_width',"Width", value ="0.3"),
                 textInput('boxplot_color',"Fill color(s)", value ="white"),
                 textInput('boxplot_transparency',"Transparency (0-1)", value ="0"),
                 checkboxInput('boxplot_notch', "Notched", value = FALSE),
                    conditionalPanel(
                    condition = "input.boxplot_notch == true ",
                    textInput('boxplot_notch_width',"Notch width", value = "0.5")
                    )
          ),
          column(width = 2,
                 textInput('boxplot_linewidth',"Line width", value ="1"),
                 textInput('boxplot_linecolor',"Line color(s)", value ="black"),
                 selectInput('boxplot_linetype',"Line type", choices = c("solid","dashed","dotted","dotdash","longdash","twodash", "blank") , selected ="solid"),
                 textInput('boxplot_fatten',"Median thickness", value ="1")
          ),
          column(width = 2,
                 checkboxInput('boxplot_outliers', "Plot outliers", value = FALSE),
                    conditionalPanel(
                      condition = "input.boxplot_outliers == true ",
                      textInput('boxplot_outlier_size',"Outlier size", value = "1.5"),
                      textInput('boxplot_outlier_color',"Outlier color", value = "black"),
                      textInput('boxplot_outlier_fill',"Outlier fill", value = "black"),
                      textInput('boxplot_outlier_transparency',"Outlier transparency (0-1)", value = "0")
                    )
                 
          )
        ),
        br()
    ),
    conditionalPanel(
      condition = "input.violin_wanted == true ",
      hr(),
      h3('Violin Options'),
      fluidRow(
        column(width = 2,
               textInput('violin_fill',"Fill color(s)", value ="white"),
               textInput('violin_transparency',"Transparency (0-1)", value ="0"),
               checkboxInput('violin_trim',"Trim tails to range", value = TRUE),
               selectInput('violin_area',"Area normalization",choices=c("area","width","count"), selected = "area")
        ),
        column(width = 2,
               textInput('violin_linewidth',"Line width", value ="1"),
               textInput('violin_linecolor',"Line color(s)", value ="black"),
               selectInput('violin_linetype' ,"Line type", choices = c("solid","dashed","dotted","dotdash","longdash","twodash", "blank") , selected ="solid"),
               textInput('violin_adjust',"Adjust smoothness", value = "1")
        )

      ),
      br()
    ),
    
    conditionalPanel(
      condition = "input.horizontal_lines != '' ",
      hr(),
      h3('Horizontal Guide(s) Options'),
      textInput('hguide_linewidth',"Line width", value ="1"),
      textInput('hguide_linecolor',"Line color", value ="grey"),
      selectInput('hguide_linetype' ,"Line type", choices = c("solid","dashed","dotted","dotdash","longdash","twodash", "blank") , selected ="solid")
    )
    
  )
  )
)


