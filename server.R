library(shiny)
library(data.table)
library(ggplot2)
library(stringr)
library(shinythemes)

options(shiny.maxRequestSize=300*1024^2) #allow large custom file upload

shapes_ggplot2 <- 21:25
names(shapes_ggplot2) <- c("circle","square","diamond","triangle up","triangle down") #note: dotplots are only circles in ggplot2

color_vector_fn <- function(colortext){ #colortext <- "black, red"
  spaces_removed <- str_replace_all(colortext," ","")
  ordered_colors <- str_split(spaces_removed, ",", simplify = TRUE)[1,]
  return(as.character(ordered_colors))
}

function(input, output, session) {
 # bs_themer() #for non-shiny UI theme
  
  
  observeEvent(input$refresh, {
    shinyjs::reset("all_inputs")
    session$reload()
  })
  
  fairsubset_url <- a("FairSubset publication", href="https://pubmed.ncbi.nlm.nih.gov/31583263/")
  output$fairsubset_url <- renderUI({
    tagList("", fairsubset_url)
  })
  
  delaneyapps_url <- a("DelaneyApps", href="http://www.delaneyapps.com/")
  output$delaneyapps_url <- renderUI({
    tagList("Please visit our website for more free science apps:", delaneyapps_url)
  })
  
  example_data_download <- as.data.frame(fread(paste0("data_plotter_example.csv"), header= TRUE, stringsAsFactors = FALSE))
  output$example_data_download <- downloadHandler(
    filename = function() {
      paste("data_plotter_example.csv", sep="")
    },
    content = function(file) {
      write.csv(example_data_download, file,row.names=FALSE)
    },
    contentType = "text/csv"
  )
  
  plotter_shiny <- eventReactive(input$plot_data,{
    
    
    
   # data_df <- as.data.frame(fread(input$pasted_data, header = TRUE, stringsAsFactors = FALSE))
    warning <- ""

    data_df <- NULL
    if(!is.null(input$uploaded_data)){
      data_df <- as.data.frame(fread(input$uploaded_data$datapath, stringsAsFactors = FALSE))
    } else {
      data_df <- as.data.frame(fread(input$pasted_data, stringsAsFactors = FALSE))
    }
    
    sample_names <- as.character(colnames(data_df))
    data_df <- lapply(1:ncol(data_df), function(column){unlist(data_df[,column])[!is.na(unlist(data_df[,column]))]})
    
    if(sum(sapply(unlist(data_df), is.numeric)) != length(unlist(data_df))){
      warning <- "Your data may contain non-numeric values.  These have been removed prior to plotting."
      suppressWarnings(
        data_df <- lapply(1:length(data_df), function(list_item){
          as.numeric(unlist(data_df[[list_item]]))[!is.na(as.numeric(unlist(data_df[[list_item]])))]
        })
      )
    }
    
    data_df <- as.data.frame(data.table::transpose(data_df), row.names = sample_names) #awkward syntax but keeps NAs in right spots for disparate data lengths
    colnames(data_df) <- 1:ncol(data_df)
    data_df <- as.data.frame(t(data_df))
    
    if(input$fairsubset_use == TRUE){
      library(matrixStats)
      
      fairSubset <- function(input_list, subset_setting = "mean", manual_N = NULL, random_subsets = 1000){
        
        input_data <- NULL
        list_names <- NULL
        warning <- ""
        
        if( is.list(input_list) == TRUE ){
          if(!is.null(names(input_list))){list_names <- names(input_list)}
          max_N <- max(sapply(1:length(input_list), function(list_item){length(input_list[[list_item]])}),na.rm=TRUE)
          input_list <- data.frame(matrix(unlist(
            lapply(1:length(input_list), function(list_item){c(input_list[[list_item]], rep(NA, max_N - length(input_list[[list_item]])))})
          ), nrow = max_N, byrow = FALSE),stringsAsFactors=FALSE)
          if(!is.null(list_names)){names(input_list) <- list_names}
          input_data <- input_list
        } else {
          input_data <- as.data.frame(input_list, stringsAsFactors = FALSE)
        }
        
        pasted_data_header <- colnames(input_data)
        input_data <- lapply(1:ncol(input_data), function(column){unlist(input_data[,column])[!is.na(unlist(input_data[,column]))]})
        
        if(length(input_data[sapply(unlist(input_data), is.numeric)]) != length(unlist(input_data))){
          suppressWarnings(
            input_data <- lapply(1:length(input_data), function(list_item){
              as.numeric(unlist(input_data[[list_item]]))[!is.na(as.numeric(unlist(input_data[[list_item]])))]
            })
          )
          warning <- "Your data may contain non-numeric values.  These have been removed prior to calculations."
        }
        
        shortest_data <- min(sapply(1:length(input_data), function(list_item){length(input_data[[list_item]])}))
        if(!is.null(manual_N)){shortest_data <- as.integer(manual_N)}
        sampled_data_report <- as.data.frame(matrix(0, nrow=2, ncol = length(input_data)))
        colnames(sampled_data_report) <- pasted_data_header
        row.names(sampled_data_report) <- c("average","standard deviation")
        
        sampled_data_best_indices <- as.data.frame(matrix(0, nrow=shortest_data, ncol = length(input_data)))
        colnames(sampled_data_best_indices) <- pasted_data_header
        
        sampled_data <- as.data.frame(matrix(0, nrow=shortest_data, ncol = length(input_data)))
        colnames(sampled_data) <- pasted_data_header
        
        data_vector <- rep(0,shortest_data)
        average_value <- 0
        standard_deviation_value <- 0
        
        if(subset_setting == "mean"){
          average_values_all <- sapply(1:length(input_data), function(list_item){mean(input_data[[list_item]])})
        }
        
        if(subset_setting %in% c("median", "ks") ){
          average_values_all <- sapply(1:length(input_data), function(list_item){median(input_data[[list_item]])})
        }
        
        standard_deviation_values_all <- sapply(1:length(input_data), function(list_item){sd(input_data[[list_item]])})
        
        all_sampled_data <- lapply(1:random_subsets, function(iteration){
          for(column in 1:length(input_data)){
            sampled_data[,column] <- sample(input_data[[column]],size = shortest_data, replace = FALSE)
          }
          return(sampled_data)
        })
        
        if(subset_setting == "mean"){
          average_values_randomized <- lapply(1:random_subsets, function(iteration){
            abs(colMeans(all_sampled_data[[iteration]]) - average_values_all) #subtract to get distance vector from original
          })
        }
        if(subset_setting %in% c("median", "ks")){
          average_values_randomized <- lapply(1:random_subsets, function(iteration){
            abs(colMedians(as.matrix(all_sampled_data[[iteration]])) - average_values_all)
          })
        }
        
        standard_deviation_values_randomized <- lapply(1:random_subsets, function(iteration){
          abs(sapply(1:ncol(sampled_data), function(column){sd(all_sampled_data[[iteration]][,column])}) - standard_deviation_values_all) #subtract to get distance vector from original
        })
        
        average_vector <- rep(0,random_subsets)
        standard_deviation_vector <- rep(0,random_subsets)
        sum_vector <- rep(0,random_subsets)
        
        if(subset_setting == "ks"){
          KS_pvals <- lapply(1:random_subsets, function(iteration){
            sapply(1:ncol(sampled_data), function(column){
              suppressWarnings({ #otherwise, R fills with tie warnings
                ks.test(input_data[[column]],all_sampled_data[[iteration]][,column], alternative = "two.sided", exact = NULL)$p.value
              })
            })
          })
          
          best_and_worst_simulations <- lapply(1:length(input_data), function(list_item){
            
            p_val_vector <- sapply(1:random_subsets, function(iteration){return(KS_pvals[[iteration]][list_item])})
            max_pval <- max(p_val_vector)
            min_pval <- min(p_val_vector)
            
            best_simulations_for_further_testing  <- as.integer(which(p_val_vector == max(p_val_vector)))
            worst_simulations_for_further_testing <- as.integer(which(p_val_vector == min(p_val_vector)))
            
            if(length(best_simulations_for_further_testing) > 1 | length(worst_simulations_for_further_testing) > 1){
              
              average_vector <-
                sapply(1:random_subsets, function(iteration){
                  average_values_randomized[[iteration]][list_item]
                }) /
                sum(sapply(1:random_subsets, function(iteration){ #dividing by sum results in equal weight of average and standard deviation
                  average_values_randomized[[iteration]][list_item]
                })) * random_subsets
              
              standard_deviation_vector <-
                sapply(1:random_subsets, function(iteration){
                  standard_deviation_values_randomized[[iteration]][list_item]
                }) /
                sum(sapply(1:random_subsets, function(iteration){ #dividing by sum results in equal weight of average and standard deviation
                  standard_deviation_values_randomized[[iteration]][list_item]
                })) *random_subsets
              
              sum_vector <- sapply(1:random_subsets, function(iteration){sum( c(average_vector[iteration],standard_deviation_vector[iteration]), na.rm=TRUE)}) #determine best simulation for given column
              
              sum_vector_best  <- sum_vector[best_simulations_for_further_testing]
              sum_vector_worst <- sum_vector[worst_simulations_for_further_testing]
              
              return(list(
                best  = min(as.integer(which(sum_vector  == min(sum_vector_best))),na.rm=TRUE )
                ,worst = min(as.integer(which(sum_vector == min(sum_vector_worst))),na.rm=TRUE )
              ))
              
            } else {
              return(list(
                best  = as.integer(which(p_val_vector == max(p_val_vector)))
                ,worst = as.integer(which(p_val_vector == min(p_val_vector)))
              ))
            }
            
          })
          best_simulations <- as.numeric(unlist(sapply(1:length(input_data), function(list_item){best_and_worst_simulations[[list_item]]$best})))
          worst_simulations <-   as.numeric(unlist(sapply(1:length(input_data), function(list_item){best_and_worst_simulations[[list_item]]$worst})))
        }
        
        best_simulations <- unlist(sapply(1:length(input_data), function(list_item){
          
          if(sum(sapply(1:random_subsets, function(iteration){
            average_values_randomized[[iteration]][list_item]
          })) == 0
          ){return(1)} else {
            
            average_vector <-
              sapply(1:random_subsets, function(iteration){
                average_values_randomized[[iteration]][list_item]
              }) /
              sum(sapply(1:random_subsets, function(iteration){ #dividing by sum results in equal weight of average and standard deviation
                average_values_randomized[[iteration]][list_item]
              })) * random_subsets
            
            standard_deviation_vector <-
              sapply(1:random_subsets, function(iteration){
                standard_deviation_values_randomized[[iteration]][list_item]
              }) /
              sum(sapply(1:random_subsets, function(iteration){ #dividing by sum results in equal weight of average and standard deviation
                standard_deviation_values_randomized[[iteration]][list_item]
              })) *random_subsets
            
            sum_vector <- average_vector + standard_deviation_vector #determine best simulation for given column
            
            return(as.integer(which(sum_vector == min(sum_vector))))
          }
          
        }))
        
        worst_simulations <- unlist(sapply(1:length(input_data), function(list_item){
          
          if(sum(sapply(1:random_subsets, function(iteration){
            average_values_randomized[[iteration]][list_item]
          })) == 0
          ){return(1)} else {
            
            average_vector <-
              sapply(1:random_subsets, function(iteration){
                average_values_randomized[[iteration]][list_item]
              }) /
              sum(sapply(1:random_subsets, function(iteration){ #dividing by sum results in equal weight of average and standard deviation
                average_values_randomized[[iteration]][list_item]
              })) *random_subsets
            
            standard_deviation_vector <-
              sapply(1:random_subsets, function(iteration){
                standard_deviation_values_randomized[[iteration]][list_item]
              }) /
              sum(sapply(1:random_subsets, function(iteration){ #dividing by sum results in equal weight of average and standard deviation
                standard_deviation_values_randomized[[iteration]][list_item]
              })) *random_subsets
            
            sum_vector <- average_vector + standard_deviation_vector #determine best simulation for given column
            
            return(as.integer(which(sum_vector == max(sum_vector))))
          }
          
        }))
        
        for(column in 1:ncol(sampled_data)){
          sampled_data[,column] <- all_sampled_data[[best_simulations[column]]][,column]
        }
        
        worst_sampled_data <- sampled_data
        for(column in 1:ncol(worst_sampled_data)){
          worst_sampled_data[,column] <- all_sampled_data[[worst_simulations[column]]][,column]
        }
        
        report <- as.data.frame(matrix(0,nrow = 6, ncol=length(input_data)))
        colnames(report) <- pasted_data_header
        if(subset_setting == "mean"){
          row.names(report) <- c("Mean of original data", "Mean of best subset of data", "Mean of worst subset of data",
                                 "Standard deviation of original data", "Standard deviation of best subset of data", "Standard deviation of worst subset of data")
          report["Mean of original data",] <- average_values_all
          report["Mean of best subset of data",] <- colMeans(sampled_data)
          report["Mean of worst subset of data",] <- colMeans(worst_sampled_data)
          report["Standard deviation of original data",] <- standard_deviation_values_all
          report["Standard deviation of best subset of data",] <- sapply(1:length(input_data), function(column){sd(sampled_data[,column])})
          report["Standard deviation of worst subset of data",] <- sapply(1:length(input_data), function(column){sd(worst_sampled_data[,column])})
        } else { #Median or KS
          row.names(report) <- c("Median of original data", "Median of best subset of data", "Median of worst subset of data",
                                 "Standard deviation of original data", "Standard deviation of best subset of data", "Standard deviation of worst subset of data")
          report["Median of original data",] <- average_values_all
          report["Median of best subset of data",] <- colMedians(as.matrix(sampled_data))
          report["Median of worst subset of data",] <- colMedians(as.matrix(worst_sampled_data))
          report["Standard deviation of original data",] <- standard_deviation_values_all
          report["Standard deviation of best subset of data",] <- sapply(1:length(input_data), function(column){sd(sampled_data[,column])})
          report["Standard deviation of worst subset of data",] <- sapply(1:length(input_data), function(column){sd(worst_sampled_data[,column])})
        }
        
        if(warning != ""){warning(warning)}
        
        return(list(
          best_subset = sampled_data
          ,report = report
          ,worst_subset = worst_sampled_data
          ,warning = as.character(warning)
          
        ))
        
      } #end fairSubset function
      
      
      
      if(input$fairsubset_manualN != ""){
        manual_N <- as.integer(input$fairsubset_manualN)
      } else {
        manual_N <- NULL
      }
      data_df <- fairSubset(input_list = data_df
                                         , subset_setting = as.character(input$fairsubset_subset_setting)
                                         , manual_N = manual_N)$best_subset
    }
    
    plot_data <- data.frame( #tidy data for ggplot2
       sample = unlist(lapply(1:ncol(data_df), function(sample){rep(sample_names[sample], sum(!is.na(data_df[,sample])))}))
      ,data = unlist(lapply(1:ncol(data_df), function(sample){unlist(data_df[,sample])[which(!is.na(data_df[,sample]) & is.numeric(data_df[,sample]))]}))
      ,x_val = unlist(lapply(1:ncol(data_df), function(sample){rep(sample, sum(!is.na(data_df[,sample])))}))
      ,fill = "white"
      ,outline = "black"
      ,dotplot_fill = "white"
      ,dotplot_outline = "black"
      ,average_error_mark_color = "black"
      ,average_error_line_color = "black"
      ,boxplot_fill = "white"
      ,boxplot_outline = "black"
      ,violin_fill = "white"
      ,violin_color = "black"
    )
    plot_data$sample <- factor(plot_data$sample, levels = sample_names)

      #put together!
      
      violin_wanted <- TRUE
      boxplot_wanted <- TRUE
      dotplot_wanted <- TRUE
      
      if(input$manual_ylims != ""){
        ylim_char <- as.character(input$manual_ylims)
        ylim_low  <- as.numeric(strsplit(ylim_char, "to")[[1]][1])
        ylim_high <- as.numeric(strsplit(ylim_char, "to")[[1]][2])
        ylim_gg <- c(ylim_low, ylim_high)
        if(ylim_high < ylim_low){ylim_gg <- c(ylim_high, ylim_low)}
      } else {
        ylim_gg <- c(min(c(plot_data$data,0), na.rm=TRUE),NA)
      }
      
      if(input$horizontal_lines != ""){
        hguide_ys <- as.character(input$horizontal_lines)
        hguide_ys <- strsplit(hguide_ys, ",")
        hguide_ys <- as.numeric(unlist(hguide_ys))
      } else {
        hguide_ys <- NULL
      }
      
      data_lengths <- sapply(1:length(unique(plot_data$sample)),function(sample){
       length({unlist(data_df[,sample])[which(!is.na(data_df[,sample]) & is.numeric(data_df[,sample]))]})
      })
    
      
      if(input$dotplot_wanted == TRUE){
        if(length(color_vector_fn(input$dotplot_fill))>1){
        plot_data$dotplot_fill <- unlist(lapply(1:length(data_lengths), function(sample){
          rep(color_vector_fn(input$dotplot_fill)[sample],data_lengths[sample])
        })) 
        } else {plot_data$dotplot_fill <- color_vector_fn(input$dotplot_fill)}
        
        if(length(color_vector_fn(input$dotplot_color))>1){
          plot_data$dotplot_color <- unlist(lapply(1:length(data_lengths), function(sample){
            rep(color_vector_fn(input$dotplot_color)[sample],data_lengths[sample])
          })) 
        } else {plot_data$dotplot_color <- color_vector_fn(input$dotplot_color)}
      }


      if(input$violin_wanted == TRUE){
        if(length(color_vector_fn(input$violin_fill))==1){
          violin_fill_vector <- rep( color_vector_fn(input$violin_fill), length(data_lengths))
        } else {violin_fill_vector <- color_vector_fn(input$violin_fill)}
        
        if(length(color_vector_fn(input$violin_linecolor))==1){
          violin_color_vector <- rep( color_vector_fn(input$violin_linecolor), length(data_lengths))
        } else {violin_color_vector <- color_vector_fn(input$violin_linecolor)}
      } else {
        violin_color_vector <- "black"
        violin_fill_vector <- "black"
      }
      
      output_plot <- ggplot(plot_data) +
        geom_violin(data = plot_data, aes(x = factor(sample, levels = sample_names), y = data, fill = factor(sample, levels = sample_names), color = factor(sample, levels = sample_names)),
                    trim = as.logical(input$violin_trim), scale = as.character(input$violin_area),
                    size = as.numeric(input$violin_linewidth), linetype = as.character(input$violin_linetype),
                    alpha = (1-as.numeric(input$violin_transparency)), adjust = as.numeric(input$violin_adjust)
                    ) +
        scale_fill_manual(values = violin_fill_vector) +
        scale_color_manual(values = violin_color_vector) +

        geom_boxplot(data = plot_data, aes(x = factor(sample, levels = sample_names), y = data),
                     fill = color_vector_fn(input$boxplot_color), alpha = (1-as.numeric(input$boxplot_transparency)), width = as.numeric(input$boxplot_width),
                     notch = as.logical(input$boxplot_notch), notchwidth = as.numeric(input$boxplot_notch_width),
                     color = color_vector_fn(input$boxplot_linecolor), size = as.numeric(input$boxplot_linewidth), linetype = as.character(input$boxplot_linetype), fatten = as.numeric(input$boxplot_fatten),
                     outlier.shape = if(input$boxplot_outliers == FALSE){NA} else {21} , outlier.size = as.numeric(input$boxplot_outlier_size), outlier.color = as.character(input$boxplot_outlier_color), outlier.fill = as.character(input$boxplot_outlier_fill), outlier.alpha = (1-as.numeric(input$boxplot_outlier_transparency))
        ) +
        geom_dotplot(data = plot_data, aes(x = factor(sample, levels = sample_names), y = data, fill = plot_data$dotplot_fill, color = plot_data$dotplot_color),
                         binaxis='y', stackdir=as.character(input$dotplot_stackdir), 
                         stackratio=as.numeric(input$dotplot_stackratio), dotsize = as.numeric(input$dotplot_dotsize), binwidth = as.numeric(input$dotplot_binwidth),
                         fill = plot_data$dotplot_fill, color = plot_data$dotplot_color,alpha = (1-as.numeric(input$dotplot_transparency))
                         ) +
        theme( plot.background = element_rect(fill='transparent', colour=NA_character_)
              ,panel.grid.major = element_blank()
              ,panel.grid.minor = element_blank()
              ,panel.border = element_blank()
              ,panel.background = element_rect(fill='transparent', colour=NA_character_)
              ,legend.position="none"
              ,axis.line.x = element_blank()
              ,axis.title.x= element_text(size=rel(1.7), color = "#000000")
              ,axis.text.y= element_text(size=as.numeric(input$yaxis_label_size))
              ,axis.title.y= element_text(size=rel(1.7))
              ,axis.line.y = element_blank()
              ,axis.text.x= element_text(size=as.numeric(input$xaxis_label_size), color ="#000000", angle = as.numeric(input$xaxis_rotation))
              ,axis.ticks.x= element_blank()
              ,axis.ticks.y= element_line()
              ,plot.title = element_text(hjust=0.5, size=rel(2))
        ) +
        labs(x = as.character(input$xaxis_title), y = as.character(input$yaxis_title), title = as.character(input$plot_title))+
        expand_limits(x = -.111*as.numeric(input$axis_linewidth), y = 0) +
        ylim(ylim_gg)+
        geom_hline(yintercept = 0,      color = as.character(input$axis_linecolor), size = as.numeric(input$axis_linewidth), linetype = as.character(input$axis_linetype) ) +
      geom_vline(xintercept = -.1*as.numeric(input$axis_linewidth),   color = as.character(input$axis_linecolor), size = as.numeric(input$axis_linewidth), linetype = as.character(input$axis_linetype) ) +
        geom_hline(yintercept = hguide_ys,     color = as.character(input$hguide_linecolor), size = as.numeric(input$hguide_linewidth), linetype = as.character(input$hguide_linetype) ) 
        
      if(input$average_error_plot == TRUE){
        means <- sapply(1:ncol(data_df), function(column){mean(data_df[,column], na.rm = TRUE)})
        medians <- sapply(1:ncol(data_df), function(column){median(data_df[,column], na.rm = TRUE)})
        standard_deviations <- sapply(1:ncol(data_df), function(column){sd(data_df[,column], na.rm = TRUE)})
        sems <- standard_deviations / sapply(1:ncol(data_df), function(column){sum(!is.na(unlist(data_df[,column]))) })
        dynamite_data <- data.frame(average = means, error = standard_deviations)
        if(input$average_error_plot_average == "median") {dynamite_data$average <- medians}
        if(input$average_error_plot_error == "standard error") {dynamite_data$error <-sems}

        
        dynamite_data$sample <- colnames(data_df)
        dynamite_data$x_val <- 1:nrow(dynamite_data)
        
        error_data <- dynamite_data
        error_data$y_min <- error_data$average - error_data$error
        error_data$y_max <- error_data$average + error_data$error
        
        output_plot <- output_plot + geom_errorbar(data = error_data,
                                            aes(x = x_val-0.01+as.numeric(input$average_error_xshift), ymin=y_min, ymax= y_max),
                                            width = as.numeric(input$average_error_line_width), size = as.numeric(input$average_error_line_thickness),
                                            color = color_vector_fn(input$average_error_line_color), alpha = (1-as.numeric(input$average_error_line_transparency)))
        output_plot <- output_plot + geom_errorbar(data = error_data,
                                               aes(x = x_val-0.01+as.numeric(input$average_error_xshift), ymin=average, ymax= average),
                                               width = as.numeric(input$average_error_mark_width), size = as.numeric(input$average_error_mark_thickness),
                                               color = color_vector_fn(input$average_error_mark_color), alpha = (1-as.numeric(input$average_error_mark_transparency)))
          
      }
      
      if(input$logarithmic_y_wanted){
        library(scales)
        
        if(input$manual_ylims != ""){
          ylim_char <- as.character(input$manual_ylims)
          ylim_low  <- as.numeric(strsplit(ylim_char, "to")[[1]][1])
          ylim_high <- as.numeric(strsplit(ylim_char, "to")[[1]][2])
          ylim_gg <- c(ylim_low, ylim_high)
          if(ylim_high < ylim_low){ylim_gg <- c(ylim_high, ylim_low)}
        } else {
          ylim_gg <- c(NA,NA)
        }
        
        
        if(input$logarithmic_scale == "log10"){
        output_plot <- output_plot +
          scale_y_log10(breaks = trans_breaks("log10", function(value) 10^value),
                        labels = trans_format("log10", math_format(10^.x)),
                        limits = ylim_gg)
        if(input$logarithmic_ticks == TRUE){
          output_plot <- output_plot +
            annotation_logticks(sides="l", base = 10)
        }

        } else {
          output_plot <- output_plot +
            scale_y_continuous(trans = log2_trans(),
                               breaks = trans_breaks("log2", function(value) 2^value),
                               labels = trans_format("log2", math_format(2^.x)),
                               limits = ylim_gg)
          if(input$logarithmic_ticks == TRUE){
            output_plot <- output_plot +
              annotation_logticks(sides="l", base = 2)
          }
        }

      }
      
      if(input$scientific_y_wanted){
        output_plot <- output_plot + 
          scale_y_continuous(labels = scientific)
      }
      
      if(input$y_ticks == FALSE){
        output_plot <- output_plot + theme(
          axis.ticks.y= element_blank()
        )
      }
      if(input$y_tick_labels == FALSE){
        output_plot <- output_plot + theme(
          axis.text.y= element_blank()
        )
      }
      
      if(input$boxplot_wanted == FALSE){ #These are removed rather than added due to ggplot2 syntax limitations
        ggplot_layers <- sapply(1:length(output_plot$layers), function(lyr){class(output_plot$layers[[lyr]]$geom)[1]})
        output_plot$layers[[which(ggplot_layers == "GeomBoxplot")]] <- NULL
      }
      if(input$violin_wanted == FALSE){
        ggplot_layers <- sapply(1:length(output_plot$layers), function(lyr){class(output_plot$layers[[lyr]]$geom)[1]})
        output_plot$layers[[which(ggplot_layers == "GeomViolin")]] <- NULL
      }
      if(input$dotplot_wanted == FALSE){
        ggplot_layers <- sapply(1:length(output_plot$layers), function(lyr){class(output_plot$layers[[lyr]]$geom)[1]})
        output_plot$layers[[which(ggplot_layers == "GeomDotplot")]] <- NULL
      }
      
      if( (input$dotplot_wanted == FALSE) & (input$violin_wanted == FALSE) & (input$boxplot_wanted == FALSE) ){
        output_plot <- output_plot +
          scale_x_discrete(breaks = 1:length(levels(plot_data$sample)),labels = levels(plot_data$sample), limits = 1:length(levels(plot_data$sample)))
      }
      
      
      if(sum(
        input$dotplot_wanted,
        input$boxplot_wanted,
        input$violin_wanted,
        input$average_error_plot
      ) == 0 ){
        output_plot <- ggplot(data = data.frame(label = "Please check a desired plot option")) + geom_text(aes(x = 1, y = 1, label = label), size = 9) + theme_void()
      }
    return(list(
      plot = output_plot
      ,warning = as.character(warning)
      ,executed = 'done'
    ))
    
  })
  output$executed <- renderText({plotter_shiny()$executed})
  output$plot <- renderPlot({plotter_shiny()$plot}, bg="transparent")
  output$ui_plot <- renderUI({plotOutput("plot", width = input$plot_width, height = input$plot_height)})
  output$warning <- renderText({plotter_shiny()$warning})
  outputOptions(output, "executed", suspendWhenHidden=FALSE)
}