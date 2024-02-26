heatmap_fun <- function(DT_in, #Data to be heatmapped. Assumed in long format; will be cast
                        cast = TRUE, #FALSE if DT_in is already in wide format and doesn't need to be cast
                        dcast_RHS, #variable to use as columns
                        dcast_value.var, #variable to use as values (i.e. plot in cells of heatmap)
                        sort_by = NULL, #variable or vector of variable names in DT_in by which to sort the wide-format data. Use NULL to not do any sorting.
                        sort_direction = 1, #or -1 -- sort ascending or descending. No effect if sort_by is NULL.
                        sort_na.last = TRUE, #whether to put NAs last or first in the sorting. No effect if sort_by is NULL.
                        heatmap_title = NULL, #if different from value.var
                        label_rows_by, #a vector of variables in DT_in by which to label rows (e.g. names, IDs)
                        label_columns_by = NULL, #a variable in DT_in by which to label columns, if different from dcast_RHS (use NULL to use dcast_RHS as column labels
                        #left-hand side row annotations
                        row_anno_funs_left = NULL, #example: 
                        #list("Accuracy" = "anno_points", #named list of functions to use for each annotation. Names correspond to variables in DT_in. Set this argument to NULL to have no row annotation.
                        # "full_call" = "anno_simple",
                        # "subset_call"= "anno_simple") 
                        row_anno_args_left = NULL, #example:
                        # list("Accuracy" = NULL, #named list of lists of additional arguments for each row annotation function.
                        #      "full_call" = list("col" = c("0"="gray80", "1"="black")), 
                        #      "subset_call" = list("col" = c("0"="gray80", "1"="black"))) 
                        rowAnnotation_args_left = NULL, #example:
                        # list(annotation_name_side = list("Accuracy" = "top", #addtional args to rowAnnotation(), controlling things like whether to show each annotation name, annotation name graphic parmaeters, annotation name side for display, etc. See ?HeatmapAnnotation for options.
                        #                                  "full_call" = "top",
                        #                                  "subset_call" = "top"),
                        #      annotation_name_rot = list("Accuracy" = 90,
                        #                                 "full_call" = 90,
                        #                                 "subset_call" = 90),
                        #      annotation_width = list("Accuracy" = unit(2, "lines"),
                        #                              "full_call" = unit(1, "lines"),
                        #                              "subset_call" = unit(1, "lines")
                        #      )
                        # )
                        row_anno_legends_left = NULL,
                        #example:
                        #list("Accuracy" = NULL,
                        # "full_call" = Legend(labels=c("0", "1"),
                        #                      legend_gp = gpar(fill = c("gray80", "black"))
                        # ),
                        # "subset_call" = Legend(labels=c("0", "1"),
                        #                        legend_gp = gpar(fill = c("gray80", "black"))
                        # ))
                        #right-hand side row annotations
                        row_anno_funs_right = NULL,
                        row_anno_args_right = NULL,
                        rowAnnotation_args_right = NULL,
                        row_anno_legends_right = NULL,
                        #bottom column annotations
                        column_anno_funs_bottom = NULL, #named list of annotation functions to use for column annotations. Names correspond to variables in DT_in. Set this argument to NULL to have no column annotation.
                        #example:
                        # list("weight"="anno_text")
                        column_anno_args_bottom = NULL,
                        #example: list("weight"=list(rot = 0, #named list of lists of additional arguments for each column annotation function.
#                         location = 0.5,
#                         just = "center",
#                         height = unit(2, "lines"),
#                         gp=gpar(fontsize = 12)))
                        columnAnnotation_args_bottom = NULL,
#example:
# list(annotation_height = list("weight" = unit(2, "lines"))
# )
                        column_anno_legends_bottom = NULL, #any legends for column annotations
#example: list("weight" = NULL)
                        #top column annotations
                        column_anno_funs_top = NULL,
                        column_anno_args_top = NULL,
                        columnAnnotation_args_top = NULL,
                        column_anno_legends_top = NULL,
                        #split rows
                        split_rows_by = NULL, #variable in DT_in by which to split the heatmap rows -- set to NULL not to split rows
                        nsplitplot = NULL, #number of row splits (if !is.null(split_rows_by)) to plot
                        cluster_row_slices = TRUE, #argument to Heatmap(). Has no effect if split_rows_by is NULL. FALSE stops reordering of row slices; TRUE allows reordering. If FALSE, row slices will be in the same order as the levels of split_rows_by.
                        #split columns
                        split_columns_by = NULL, #variable in DT_in by which to split the heatmap columns -- set to NULL not to split heatmap columns
                        cluster_column_slices = TRUE, #argument to Heatmap(). Has no effect if split_rows_by is NULL. FALSE stops reordering of row slices; TRUE allows reordering. If FALSE, column slices will be in the same order as the levels of split_columns_by.
                        #control colormapping
                        colormap_type = "continuous", #type of colormap to use: "continuous", "diverging", or "qualitative"
                        color_lims = c(NA_real_,NA_real_), #limits between which to interpolate colors. A two-element vector for "continuous" or "diverging" colormaps. As many values as there are colors for a "qualitative" colormap. Use NA_real_ to signal that you want one or both limits to be computed as the limits of the data.
                        color_midpoint = 0, #applies only for "diverging" colormaps: the center color (usually white or yellow) will be fixed at this value (usually 0).
                        colormap_base = magma(11), #the set of colors to interpolate or use. E.g. magma(11) for a continuous colormap, or RColorBrewer::brewer.pal(n=9, palette="RdBu") for diverging.
                        #specify device for saving plot
                        plot_save_device = "pdf",
                        plot_save_device_args = list(height=11,
                                                     width=16),
                        plot_filename = NULL,
                        #any additional arguments to supply to Heatmap() function
                        Heatmap_args = list(show_row_names = TRUE, #show row names or not
                        show_row_dend = FALSE, #show row dendrograms or not
                        show_column_names = TRUE, #show row names or not
                        show_column_dend = FALSE), #show column dendrograms or not, 
                        #any additional arguments to supply to draw() function
                        draw_Heatmap_args = list(heatmap_legend_side="top", #controls where legends go
                          adjust_annotation_extension = FALSE), 
#e.g. can add padding as padding = unit(#add extra margins around plot (e.g. to stop row/column names from being cut off)
# c(0,1,2,1), #bottom, left, top, right space
# "lines")
                        #specify column names as a text annotation?
                        column_name_anno_text = FALSE, #if TRUE, then original column names will be replaced with a text annotation
                        column_name_anno_text_args = NULL,
                        column_name_anno_text_side = "bottom", #or "top"
                        #specify row names as a text annotation?
                        row_name_anno_text = FALSE, #if TRUE, then original row names will be replaced with a text annotation
                        row_name_anno_text_args = NULL, #parameters for row names as text annotation (arguments to anno_text())
                        row_name_anno_text_side = "right" #or "left"
                        ){ 
  DT_in <- copy(DT_in) #to treat as though passed by reference, not value
 
  if(cast==TRUE){
  #Gather all RHS variables for the dcast()
  new_LHS_vars <- unique(c(label_rows_by,
                    names(row_anno_funs_left),
                    names(row_anno_funs_right),
                    split_rows_by,
                    sort_by))
  
  #in case there is some extra data that we don't need to consider,
  #keep only the unique rows according to the variables we care about
  DT_in <- unique(DT_in[, .SD, .SDcols = c(new_LHS_vars,
                                                dcast_RHS,
                                                dcast_value.var,
                                           names(column_anno_funs_bottom),
                                           names(column_anno_funs_top),
                                           label_columns_by,
                                           split_columns_by)])
  
  #Get the column names after dcast()
  #Convert to character, in case they are numeric
  col_cast_names <- DT_in[, sapply(.SD, function(x) unique(as.character(x))), .SDcols = dcast_RHS]
  
  #construct the formula to pass to dcast()
  cast_formula <- as.formula(paste0(paste(paste0("`", #add backticks just in case these are not valid R variable names (like they have spaces)
                                                 new_LHS_vars,
                                                 "`"), 
                                          collapse=" + "),
                                    "~ ",
                                    paste0("`",
                                           dcast_RHS,
                                           "`")))
  #cast to wide format
  DTcast <- dcast(DT_in,
                     cast_formula,
                     value.var = dcast_value.var)
  
  #sort, if so specified
  if(!is.null(sort_by)){
    setorderv(DTcast,
              cols = sort_by,
              order = sort_direction,
              na.last = sort_na.last)
   
  }
  }else{
    DTcast <- copy(DT_in)
  }
  
  #split rows as requested 
  if(!is.null(split_rows_by)){
    #take the specified number of row splits for plotting
    if(!is.null(nsplitplot)){
      DTcast[, split_order:=lapply(.SD,
                                   function(x) {
                                     as.numeric(
                                       factor(x,
                                              levels = unique(x)
                                       )
                                     )
                                   }
      ),
      .SDcols = split_rows_by]
      
      DTcast <- DTcast[split_order<=nsplitplot]
    } #end if(!is.null(nsplitplot))
    #now, get the vector of values by which to split rows
    splitrows <- DTcast[[split_rows_by]]
    #if row split reordering is turned off, then get the order of rows
    if(cluster_row_slices == FALSE){
      if(is.factor(splitrows)){ #if it's already a factor, use its levels
        splitrows <- droplevels(splitrows) #in case taking the specified subset of row splits for plotting caused us to remove any levels, let's drop them
      }else{ #if it's not already a factor, set its levels in the order of their sorted occurrence
        splitrows <- factor(splitrows, levels = unique(splitrows))
      }
    } #end if(cluster_row_slices==FALSE)
  }else{ #if split_rows_by is NULL
    splitrows <- NULL
  }
  
  #fix row order
  if(!is.null(sort_by)){
  #if sorted and nothing else is in the row_order argument, then take the sorted row labels.
    if(!("row_order" %in% names(Heatmap_args))){
      Heatmap_args[["row_order"]] <- DTcast[[label_rows_by]]
    }
    
  }
  
  #create a matrix
  DTmat <- as.matrix(DTcast[,
                            .SD,
                            .SDcols = col_cast_names])
  
  #assign row names to the matrix
  rownames(DTmat) <- DTcast[[label_rows_by]]
  
  #split columns, if so requested
  if(!is.null(split_columns_by)){
    DT_colsplit <- unique(DT_in[, .SD, .SDcols = c(dcast_RHS,
                                                   split_columns_by)])
    DT_colsplit[, (dcast_RHS):=lapply(.SD, as.character), .SDcols = dcast_RHS]
    DT_colsplit <- DT_colsplit[colnames(DTmat), on=dcast_RHS]
    splitcolumns <- DT_colsplit[[split_columns_by]]
    if(cluster_column_slices == FALSE){
      if(is.factor(splitcolumns)){ #if it's already a factor, use its levels
        splitcolumns<- droplevels(splitcolumns) 
      }else{ #if it's not already a factor, set its levels in the order of their sorted occurrence
        splitcolumns <- factor(splitcolumns, levels = unique(splitcolumns))
      }
    } #end if(cluster_columns_slices==FALSE)
  }else{
    splitcolumns <- NULL
  }
  
  #create row annotations
  
  #left-side row annotations
  if(!is.null(row_anno_funs_left)){
    row_anno_vars_left <- names(row_anno_funs_left)
    row_annotation_list_left <- sapply(row_anno_vars_left,
                                  function(x){
                                    do.call(row_anno_funs_left[[x]],
                                            c(list("x" = DTcast[[x]],
                                              "which" = "row"),
                                                   row_anno_args_left[[x]])
                                            )
                                  },
                                  simplify = FALSE,
                                  USE.NAMES = TRUE)
  }else{
    row_annotation_list_left <- NULL
  }
  

  #right-side row annotations
  if(!is.null(row_anno_funs_right)){
    row_anno_vars_right <- names(row_anno_funs_right)
    row_annotation_list_right <- sapply(row_anno_vars_right,
                                       function(x){
                                         do.call(row_anno_funs_right[[x]],
                                                 c(list("x" = DTcast[[x]],
                                                   "which" = "row"),
                                                        row_anno_args_right[[x]])
                                                 )
                                       },
                                       simplify = FALSE,
                                       USE.NAMES = TRUE)
    
  }else{
    row_annotation_list_right <- NULL
  }
  
  #if row names should be provided as a text annotation
  if(row_name_anno_text==TRUE){
    if(row_name_anno_text_side=="left"){
      row_annotation_list_left <- c(row_annotation_list_left,
                                    list("rn" = do.call(anno_text,
                                                              row_name_anno_text_args)))
    }else{
      row_annotation_list_right <- c(row_annotation_list_right,
                                     list("rn" = do.call(anno_text,
                                                               row_name_anno_text_args)))
    }
  }
  
  if(!is.null(row_annotation_list_left)){
    #and finally, call rowAnnotation to finally get all the row annotations together
    row_anno_final_left <- do.call(HeatmapAnnotation,
                                   c(list(which="row"),
                                     row_annotation_list_left,
                                     rowAnnotation_args_left))
  }else{
    row_anno_final_left <- NULL
  }
  
  if(!is.null(row_annotation_list_right)){
    #and finally, call rowAnnotation to finally get all the row annotations together
    row_anno_final_right <- do.call(HeatmapAnnotation,
                                   c(list(which="row"),
                                     row_annotation_list_right,
                                     rowAnnotation_args_right))
  }else{
    row_anno_final_right <- NULL
  }
  
  #create column annotations
  #bottom column annotations
  if(!is.null(column_anno_funs_bottom)){
    column_anno_vars_bottom <- names(column_anno_funs_bottom)
    #first, we need to get the values of each variable for each column
    DT_column_anno_bottom <- unique(DT_in[, .SD, .SDcols = c(dcast_RHS, #the column names themselves
                                                      column_anno_vars_bottom)]) #the column variables
    #ensure that column names are sorted in the same order as they appear in the matrix
    DT_column_anno_bottom <- DT_column_anno_bottom[colnames(DTmat), on=dcast_RHS]
    #now, create column annotations
    #first, add the data argument to each of the items in column_anno_args
    column_anno_args_bottom <- sapply(column_anno_vars_bottom,
                               function(x) c(list("x"=DT_column_anno_bottom[[x]]),
                                             column_anno_args_bottom[[x]]),
                               simplify = FALSE,
                               USE.NAMES = TRUE)
    #now, construct the list of column annotations
    column_annotation_list_bottom <- sapply(column_anno_vars_bottom,
                                    function(x) do.call(column_anno_funs_bottom[[x]],
                                                       column_anno_args_bottom[[x]]),
                                    simplify = FALSE,
                                    USE.NAMES = TRUE)
  
  }else{
    column_annotation_list_bottom <- NULL
  }
  
  #top column annotations
  if(!is.null(column_anno_funs_top)){
    column_anno_vars_top <- names(column_anno_funs_top)
    #first, we need to get the values of each variable for each column
    DT_column_anno_top <- unique(DT_in[, .SD, .SDcols = c(dcast_RHS, #the column names themselves
                                                             column_anno_vars_top)]) #the column variables
    #ensure that column names are sorted in the same order as they appear in the matrix
    DT_column_anno_top <- DT_column_anno_top[colnames(DTmat), on=dcast_RHS]
    #now, create column annotations
    #first, add the data argument to each of the items in column_anno_args
    column_anno_args_top <- sapply(column_anno_vars_top,
                                      function(x) c(list("x"=DT_column_anno_top[[x]]),
                                                    column_anno_args_top[[x]]),
                                      simplify = FALSE,
                                      USE.NAMES = TRUE)
    #now, construct the list of column annotations
    column_annotation_list_top <- sapply(column_anno_vars_top,
                                            function(x) do.call(column_anno_funs_top[[x]],
                                                                column_anno_args_top[[x]]),
                                            simplify = FALSE,
                                            USE.NAMES = TRUE)
  
  }else{
    column_annotation_list_top <- NULL
  }
  
  #if column names should be provided as a text annotation
  if(column_name_anno_text==TRUE){
    if(column_name_anno_text_side=="bottom"){
      column_annotation_list_bottom <- c(column_annotation_list_bottom,
                                    list("columnnames" = do.call(anno_text,
                                                              column_name_anno_text_args)))
    }else{
      column_annotation_list_top <- c(column_annotation_list_top,
                                     list("columnnames" = do.call(anno_text,
                                                               column_name_anno_text_args)))
    }
  }
  
  if(!is.null(column_annotation_list_bottom)){
    #and finally, call HeatmapAnnotation to get all the column annotations together
    column_anno_final_bottom <- do.call(HeatmapAnnotation,
                                        c(column_annotation_list_bottom,
                                          columnAnnotation_args_bottom))
  }else{
    column_anno_final_bottom <- NULL
  }

  
  if(!is.null(column_annotation_list_top)){
    #and finally, call HeatmapAnnotation to get all the column annotations together
    column_anno_final_top <- do.call(HeatmapAnnotation,
                                        c(column_annotation_list_top,
                                          columnAnnotation_args_top))
  }else{
    column_anno_final_top <- NULL
  }

  #main heatmap
  
  #create color ramp function for the data itself
  #get limits from data if not user-specified
  if(is.na(color_lims[1])){
    color_lims[1] <- min(DTmat, na.rm = TRUE)
  }
  if(is.na(color_lims[2])){
    color_lims[2] <- max(DTmat, na.rm = TRUE)
  }
  
  colormap_base_n <- length(colormap_base)
  
  if(colormap_type=="continuous"){ 
    col_fun = colorRamp2(seq(color_lims[1],
                             color_lims[2],
                             length.out=colormap_base_n),
                         colormap_base)
  }else if(colormap_type=="diverging"){ #if we're plotting data that goes from negative to positive, then use a diverging colormap
    col_fun = colorRamp2(c(seq(color_lims[1],
                               color_midpoint,
                               length.out = (colormap_base_n + 1)/2),
                           seq(color_midpoint,
                               color_lims[2],
                               length.out = (colormap_base_n + 1)/2)[-1]), #this enforces that 0 is the midpoint color, even if the data range is not symmetrical (e.g. from -3 to 15, rather than from -15 to 15)
                         colormap_base)
  }else if(colormap_type=="qualitative"){
    col_fun <- colormap_base
    names(col_fun) <- color_lims
  }
  
  #get name of heatmap colorbar
  if(is.null(heatmap_title)) heatmap_title <- dcast_value.var
  
  #get rownames fontsize, if it was specified
  if("row_names_gp" %in% names(Heatmap_args)){
    if("fontsize" %in% names(Heatmap_args$row_names_gp)){
      row_names_fontsize <- Heatmap_args$row_names_gp$fontsize
    }
  }else{
    row_names_fontsize = 12
  }
  
  h_main <- do.call(Heatmap,
                    args = c(
                      list(matrix=DTmat,
                           name=heatmap_title,
                           row_split = splitrows,
                           column_split = splitcolumns,
                           col = col_fun,
                           bottom_annotation = column_anno_final_bottom,
                           top_annotation = column_anno_final_top,
                           right_annotation = row_anno_final_right,
                           left_annotation = row_anno_final_left),
                      Heatmap_args)
  )
  
  if(!is.null(plot_filename)){
    do.call(plot_save_device,
            c(list(file = plot_filename),
                 plot_save_device_args))
    do.call(draw,
            args = c(list(h_main,
                     annotation_legend_list = c(row_anno_legends_left,
                     row_anno_legends_right,
                     column_anno_legends_bottom,
                     column_anno_legends_top)),
                     draw_Heatmap_args))
    dev.off()
  }
  
  #return the heatmap object
  return(h_main)
}