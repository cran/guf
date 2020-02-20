#' The inverse of \%in\%
#'
#' \code{\%notin\%} tells you which values of vector x are not in vector y.
#'
#' @param x A vector
#' @param y A vector
#' @return A vector of of booleans with the length of vector x. \code{TRUE} indicates that the particular element of x is not in y, \code{FALSE} indicates that the element of x is in y.
#' @details Although this function can be called as \code{`\%notin\%`(x, y)}, the intended use is like the function \code{\%in\%}: similar to \code{x \%in\% y}, this function's supposed use is \code{x \%notin\% y}.
#' Vector types (more formally: modes) don't need to match (see last example).
#' @examples c(1,2,3) %notin% c(0,2,4) # returns TRUE FALSE TRUE
#' # compare to:
#' c(1,2,3) %in% c(0,2,4) # returns FALSE TRUE FALSE
#'
#' @examples c(1,2) %notin% c(0,2,4) # returns TRUE FALSE
#' # versus (vectors changed place, hence, result adapts to length of x):
#' c(0,2,4) %notin% c(1,2) # returns TRUE FALSE TRUE
#'
#' @examples c('Hello', 'world') %notin% unlist(strsplit('The world is not enough', ' ')) # TRUE FALSE
#' @examples c('Hello', 'world') %notin% c(1,2,3,4,5) # returns TRUE TRUE. Vector types don't need to match.
`%notin%` <- function (x, y) {
  match(x, y, nomatch = 0L) == 0L
}


#' Get the specified right most characters of a string
#'
#' right() counts from the right most position of a character string, for a specified number of characters, and returns the resulting substring. If the specified number of characters is more than the total length of the string, then the entire string will be returned.
#'
#' @param string A character string, or a vector of character strings. This is the (vector of) character strings(s) of which the right most characters are to be returned.
#' @param nr_of_chars An integer or a vector of integers, specifying the right most number of characters to return. If both 'string' and 'nr_or_chars' are a vector, then the vector lengths must match.
#' @return A character string, or a vector of character strings if the input is a vector.
#' @examples right("hello world", 3) # returns 'rld'
#' @examples right(c("hello", "world"), 3) # returns vector c('llo', 'rld')
#' @examples right(c("hello", "world"), c(1,3)) # returns vector c('o', 'rld')
#' @examples right("hello world", 80) # returns 'hello world'
right = function (string, nr_of_chars) {
  msg <- NULL
  if (class(string) != 'character') {
    msg = "- The variable 'string' needs to be of class character"
  }
  if (class(nr_of_chars) != 'numeric') {
    if (!is.null(msg)) {msg = paste(msg, "\n")}
    msg = paste(msg, "- The variable 'nr_of_chars' needs to be of class integer")
  }
  if ((length(nr_of_chars) > 1) & (length(string) != length(nr_of_chars))) {
    if (!is.null(msg)) {msg = paste(msg, "\n")}
    msg = paste(msg, "- The vector length of 'string' and 'nr_of_chars' need to be the same if the vector length of 'nr_of_chars' is > 1")
  }
  if (!is.null(msg)) {
    stop(msg)
  }
  # actual function:
  substr(string, nchar(string)-(nr_of_chars-1), nchar(string))
}




#' Get the specified left most characters of a string
#'
#' left() counts from the start of a character string, for a specified number of characters, and returns the resulting substring. If the specified number of characters is more than the total length of the string, then the entire string will be returned.
#'
#' @param string A character string, or a vector of character strings. This is the (vector of) character strings(s) of which the starting characters are to be returned.
#' @param nr_of_chars An integer or a vector of integers, specifying the number of characters from the start of the string to return. If both 'string' and 'nr_or_chars' are a vector, then the vector lengths must match.
#' @return A character string, or a vector of character strings if the input is a vector.
#' @examples left("hello world", 2) # returns 'he'
#' @examples left(c("hello", "world"), 2) # returns vector c('he', 'wo')
#' @examples left(c("hello", "world"), c(1,3)) # returns vector c('h', 'wor')
#' @examples left("hello world", 80) # returns 'hello world'
left = function (string, nr_of_chars){
  msg <- NULL
  if (class(string) != 'character') {
    msg = "- The variable 'string' needs to be of class character"
  }
  if (class(nr_of_chars) != 'numeric') {
    if (!is.null(msg)) {msg = paste(msg, "\n")}
    msg = paste(msg, "- The variable 'nr_of_chars' needs to be of class integer")
  }
  if ((length(nr_of_chars) > 1) & (length(string) != length(nr_of_chars))) {
    if (!is.null(msg)) {msg = paste(msg, "\n")}
    msg = paste(msg, "- The vector length of 'string' and 'nr_of_chars' need to be the same if the vector length of 'nr_of_chars' is > 1")
  }
  if (!is.null(msg)) {
    stop(msg)
  }
  # actual function:
  substr(string, 1, nr_of_chars)
}


#' Standard Error
#'
#' The Standard Error (`se`) of a numerical vector x.
#'
#' @usage se(x, na.rm = TRUE)
#'
#' @param x A vector of numerical values, be it integers and/or real numbers.
#' @param na.rm A Boolean, indicating whether NAs need to be removed from x before calculating the Standard Error; defaults to TRUE
#' @return One real number, being the Standard Error.
#' @examples my_vector = c(1, 3, 4, 7.5, 8)
#' var(my_vector); sd(my_vector); se(my_vector)
#' @examples my_vector = c(NA, 3, 4, 7.5, 8)
#' var(my_vector); sd(my_vector); se(my_vector)
#' # note that se() by default leaves out NAs. But compare:
#' var(my_vector); sd(my_vector); se(my_vector, na.rm=FALSE)
se <- function(x, na.rm=TRUE) {
  msg <- NULL
  if (class(x) != 'numeric') {
    msg = "- The variable 'x' needs to be of class numeric"
  }
  if (length(na.omit(x))==0) {
    if (!is.null(msg)) {msg = paste(msg, "\n")}
    if (length(x) > 0) {
      msg = paste(msg, "- Only NAs in 'x', but at least one number required")
    } else {
      msg = paste(msg, "- No values in 'x'; 'x' needs to contain at least one number")
    }
  }
  if (class(na.rm) != 'logical') {
    if (!is.null(msg)) {msg = paste(msg, "\n")}
    msg = paste(msg, "- The variable 'na.rm' needs to be of class logical")
  } else {
    if (is.na(na.rm)) {
      if (!is.null(msg)) {msg = paste(msg, "\n")}
      msg = paste(msg, "- The variable 'na.rm' needs to be either TRUE or FALSE; NA detected")
    }
  }
  if (!is.null(msg)) {
    stop(msg)
  }
  # actual function:
  sqrt(var(x,na.rm=na.rm)/length(na.omit(x))) # i.e. sd(x) / sqrt(length(x))
}


#' The function round_something() does numerical rounding with a specified number of decimals. The 'something' will typically be a data.frame with mixed column types, of which the numerical columns must be rounded. This function makes that easier: all non-numerical columns are left in place, the numerical columns are rounded. Columns that are character but can be interpreted as numerical are first converted to numerical, and then rounded.
#' @param something The object of which the numbers need to be rounded. Can be a numerical or character vector, a matrix, a data.frame or a list containing these elements.
#' @param decimals An integer specifying how many decimal points the result must contain.
#' @return The same structure as used for the input of 'something', but then with rounded numbers (where applicable).
#' @examples my_DF = data.frame(A=c(1.111,2.22,3.3,4), B=c("A", "Bb", "Ccc", "Dddd"), row.names=c(1,2,3,4))
#' round_something(my_DF)
#' round_something(my_DF, 2)
#' @examples my_list = list(A=c(1.111,2.22,3.3,4), B=c("A", "Bb", "Ccc", "Dddd"))
#' round_something(my_list)
#' round_something(my_list, 2)
round_something <- function(something, decimals=0) {


  sub_round  <- function(input, digits) {
    result <- round(input, digits)
    return(result)
  }

  round_df_or_matrix_or_vector <- function(input, digits) {
    if (class(input) == "numeric") {
        result <- sub_round(input, digits)
    } else if (class(input) == "character") {
        result <- suppressWarnings(sapply(input, function(x) if(!is.na(as.numeric(x))){sub_round(as.numeric(x), digits)} else {x}))
    } else if (class(input) == "matrix") {
      result <- suppressWarnings(apply(input, 1:2, function(x) if(!is.na(as.numeric(x))){sub_round(as.numeric(x), digits)} else {x}))
    } else if (class(input) == "data.frame") {
      nums <- sapply(input, is.numeric)
      input[,nums] <- sub_round(input[,nums], digits = digits)
      result <- input
    }
    return(result)
  }

  round_list <- function(the_list, decimals) {
    result <- list()
    listnames <- names(the_list)
    for (i in 1:length(the_list)) {
      if (class(the_list[[i]]) == "list") {
        result[[listnames[i]]] <- round_list(the_list[[i]], decimals)
      } else {
        result[[listnames[i]]] <- round_df_or_matrix_or_vector(the_list[[i]], decimals)
      }
    }
    return(result)
  }

  if ((class(something) == "numeric") | (class(something) == "character") | (class(something) == "matrix") | (class(something) == "data.frame")) {
    result <- round_df_or_matrix_or_vector(something, decimals)
  } else if (class(something) == "list") {
    result <- round_list(something, decimals)
  }
  return(result)
}


# internal function
count_vector <- function(the_vector, vector_name='element') {
  if (sum(is.factor(the_vector), (typeof(the_vector)=='integer'))==2) {
    vector_type = 'factor'
    the_levels = levels(the_vector)
    the_vector <- as.vector(as.character(the_vector))
  } else if (is.numeric(the_vector)) {
      vector_type = 'numeric'
  } else if (is.character(the_vector)) {
    vector_type = 'character'
  }
  result <- as.matrix(table(the_vector))
  result <- cbind(rownames(result), result)
  colnames(result) <- c(vector_name, "count")
  result <- as.data.frame(result)
  row.names(result) <- 1:nrow(result)
  result$count <- as.numeric(as.character(result$count))
  result[[vector_name]] <- as.character(result[[vector_name]])
  if (vector_type == 'factor') {
    result[[vector_name]] <- factor(result[[vector_name]], levels=the_levels)
  } else if (vector_type == 'numeric') {
    result[[vector_name]] <- as.numeric(result[[vector_name]])
  }
  return(result)
}

order2 <- function(DF, varnames, i=NULL) {
  if (is.null(i)) {
    i = length(varnames)
  }
  DF <- DF[order(DF[[varnames[i]]]),]
  if (i==1) {
    return(DF)
  } else {
    return(order2(DF, varnames, i-1))
  }
}

# internal function
FUN_DF <- function(the_DF, group.by, FUN=NULL, num.column=NULL) {
  find_sep_character <- function(group.by, sep_char="_") {
    if (length(grep(sep_char, group.by))>0) {
      sep_char = paste(sep_char, "_", sep="")
      find_sep_character(group.by, sep_char)
    } else {
      return(sep_char)
    }
  }
  sep_char = find_sep_character(colnames(the_DF))
  NA_replacement <- NULL
  for (varname in group.by) {
    if (sum(is.na(the_DF[[varname]]))>0) {
      temp <- make_NA_replacement(the_DF[[varname]])
      NA_replacement <- c(NA_replacement, temp)
      names(NA_replacement)[length(NA_replacement)] = varname
      if (is.factor(the_DF[[varname]])) {
        levels(the_DF[[varname]]) <- c(levels(the_DF[[varname]]), temp)
      }
      if (is.numeric(the_DF[[varname]])) {
        the_DF[[varname]][which(is.na(the_DF[[varname]]))] <- as.numeric(temp)
      } else {
        the_DF[[varname]][which(is.na(the_DF[[varname]]))] <- temp
      }
    }
  }
  helpervar_name = paste("helper", "column", sep=sep_char)
  for (i in 1:length(group.by)) {
    if (i == 1) {
      the_DF[[helpervar_name]] = the_DF[[group.by[i]]]
    } else {
      the_DF[[helpervar_name]] = paste(the_DF[[helpervar_name]], the_DF[[group.by[i]]], sep=sep_char)
    }
  }

  if (is.null(FUN)) {
    result = as.data.frame(table(the_DF[[helpervar_name]]))
    result[] = lapply(result, as.character)
    colnames(result)[1] <- helpervar_name
    for(i in 1:length(group.by)) {
      result[[group.by[i]]] = sapply(strsplit(result[[helpervar_name]], sep_char), function(x) x[i])
    }
    result[[helpervar_name]] <- NULL
    if ("count" %in% group.by) {
      count_col_name = paste("count", sep_char, sep="")
    } else {
      count_col_name = "count"
    }
    colnames(result)[1] = count_col_name
    result = result[,c(group.by, count_col_name)]
    result = merge(unique(the_DF[,group.by]), result, sort=FALSE) # basically to sort it in the original order
    result[[count_col_name]] <- as.integer(result[[count_col_name]])
  } else {
    result = aggregate(formula=as.formula(paste(num.column, "~", helpervar_name, sep='')), data=the_DF, FUN=FUN)
    # aggregate does not include rows of helpervar_name when the calculated content (outcome of the FUN) is NA. Hence, include such rows via the following three lines:
    helpvar_content <- as.data.frame(unique(the_DF[[helpervar_name]]))
    colnames(helpvar_content) <- helpervar_name
    result = merge(helpvar_content, result, all.x=TRUE)
    # then re-create the grouping variables by splitting the previously combined values:
    for(i in 1:length(group.by)) {
      result[[group.by[i]]] = sapply(strsplit(as.character(result[[helpervar_name]]), sep_char), function(x) x[i])
    }
    result$grouping_var <- NULL
    result = result[,c(group.by, num.column)] # simple re-ordering of columns
  }
  for (grouping_var in group.by) {
    if (is.factor(the_DF[[grouping_var]])) {
      result[[grouping_var]] <- factor(result[[grouping_var]], levels=levels(the_DF[[grouping_var]]))
    }
  }
  result <- order2(result, group.by)
  row.names(result) <- 1:nrow(result)
  #  now convert NA dummies back to NA:
  if (!is.null(NA_replacement)) {
    for (varname in group.by) {
      if (varname %in% names(NA_replacement)) {
        result[[varname]][which(result[[varname]] == NA_replacement[varname])] <- NA
      }
    }
  }
  return(result)
}


# internal function
make_unique_varname <- function(varname, the_vector, separator_char='_') {
  if (varname %in% the_vector) {
    return(make_unique_varname(paste(varname, separator_char, sep=''), the_vector))
  } else {
    return(varname)
  }
}


# internal function
make_NA_replacement <- function(the_vector) {
  if (is.numeric(the_vector)) {
    return(max(the_vector, na.rm=TRUE)+1)
  } else {
    if (is.factor(the_vector)) {
      the_vector <- levels(the_vector)
    }
    dummy = the_vector[order(the_vector)]
    dummy <- dummy[!is.na(dummy)]
    if (length(dummy)>0) {
      return(paste(dummy[length(dummy)], 'z', sep=''))
    } else {
      return('z')
    }
  }
}



# internal function
FUN_per_group_per_split <- function(DF, group.by, split.by, row_total=FALSE, FUN=NULL, num.column=NULL) {
  # helper functions
  make_grouping_variable_name <- function(varname) {
    if (varname %in% colnames(DF)) {
      return(make_grouping_variable_name(paste(varname, '_', sep='')))
    } else {
      return(varname)
    }
  }
  remove_empty_list_elements <- function(the_list) {
    result <- lapply(the_list, function(x) if (!is.null(x)) {if (nrow(x)>0) {x} else {NULL}})
    result <- result[lengths(result) != 0]
    return(result)
  }

  # x_values, i.e. the pivoted 'split.by' column
  if (is.numeric(DF[[split.by]])) {
    x_values <- unique(DF[,split.by]) # keep it numeric to get proper numeric ordering, in a few lines under here... Avoid c(1, 3, 11) being ordered as c('1', '11', '3')
  } else {
    x_values <- as.character(unique(DF[,split.by]))
  }
  if (is.factor(DF[[split.by]])) {
    x_values <- levels(DF[[split.by]]) # this basically overrules the previous statements to determine x_values
    if (sum(is.na(unique(DF[,split.by])))>0) {
      x_values <- c(x_values, NA)
    }
  } else {
    x_values <- x_values[order(x_values)] # if NA, then that will [also] be the last value in this vector
  }
  x_values <- as.character(x_values) # at last, finally, everything: characters
  NA_replacement <- NULL
  if (sum(is.na(x_values))>0) {
    temp <- make_NA_replacement(x_values)
#    temp <- make_NA_replacement('-99999', x_values, '9') # Low probability '-99999' will already be a value in x_values, but if so, then a '9' will be added until it's really unique.
    NA_replacement <- c(NA_replacement, temp)
    names(NA_replacement)[length(NA_replacement)] = 'x_values'
    x_values[which(is.na(x_values))] <- temp
    if (is.factor(DF[[split.by]])) {
      levels(DF[[split.by]]) <- c(levels(DF[[split.by]]), temp)
    }
    DF[[split.by]][which(is.na(DF[[split.by]]))] <- temp # here, temp is not (only) a column name, but actually based on the values in DF[[split.by]]
  }

  # y-values, i.e. unique values from the 'group.by' column(s) arranged vertially, i.e. rows
  for (varname in group.by) {
    if (sum(is.na(DF[[varname]]))>0) {
      temp <- make_NA_replacement(DF[[varname]])
      NA_replacement <- c(NA_replacement, temp)
      names(NA_replacement)[length(NA_replacement)] = varname
      if (is.factor(DF[[varname]])) {
        levels(DF[[varname]]) <- c(levels(DF[[varname]]), temp)
      }
      if (is.numeric(DF[[varname]])) {
        DF[[varname]][which(is.na(DF[[varname]]))] <- as.numeric(temp)
      } else {
        DF[[varname]][which(is.na(DF[[varname]]))] <- temp
      }
    }
  }
  grouping_variable_name <- make_grouping_variable_name("grouping_var_name") # this is an instrumental column, to be deleted at the end
  if (length(group.by)==1) {
    DF[[grouping_variable_name]] <- as.character(DF[[group.by]])
  } else {
    DF[[grouping_variable_name]] <- apply(DF[,group.by], 1, paste, collapse='__')
  }
  y_values <- unique(DF[,c(grouping_variable_name, group.by)])
  if (is.vector(y_values)) {
    repetitions = length(y_values)
  } else if (sum(is.matrix(y_values), is.data.frame(y_values)) == 1) {
    repetitions = nrow(y_values)
  } else if (sum(is.factor(y_values), (typeof(y_values)=='integer'))==2) {
    repetitions = length(y_values)
  }
  if (is.null(FUN)) {
    result <- cbind(y_values, as.data.frame(matrix(0, ncol=length(x_values), nrow=repetitions)))
  } else {
    result <- cbind(y_values, as.data.frame(matrix(NA, ncol=length(x_values), nrow=repetitions)))
  }
  colnames(result) <- c(grouping_variable_name, group.by, x_values)
  result[,c(grouping_variable_name, group.by)] <- y_values
  DF_split <- split(DF, DF[,grouping_variable_name])
  # some list elements of DF_split may have 0 rows. Remove these:
  DF_split <- remove_empty_list_elements(DF_split)
  # do the actual count:
  if (is.null(FUN)) {
    DF_count <- lapply(DF_split, function(x) cbind(unique(x[,c(grouping_variable_name, group.by)]), t(as.matrix(table(x[,split.by])))))
  } else {
    suppressWarnings(
      DF_FUN <- lapply(DF_split, function(x) cbind(unique(x[,c(grouping_variable_name, group.by)]), aggregate(formula=as.formula(paste(num.column, "~", group.by, "+", split.by, sep='')), data=x, FUN=FUN)))
    )
  }

  # transfer to result:
  if (is.null(FUN)) {
    for (rownr in 1:length(DF_count)) {
      DF_count_sub <- DF_count[[rownr]]
      for (colname in colnames(DF_count_sub)[(2+length(group.by)):ncol(DF_count_sub)]) {
        result[result[[grouping_variable_name]]==DF_count_sub[[grouping_variable_name]], colname] = DF_count_sub[[colname]]
      }
    }
  } else {
    for (DF_FUN_name in names(DF_FUN)) {
      DF_FUN_sub <- DF_FUN[[DF_FUN_name]]
      for (rownr in 1:nrow(DF_FUN_sub)) {
        result[result[[grouping_variable_name]]==DF_FUN_sub[[grouping_variable_name]][rownr], as.character(DF_FUN_sub[rownr, split.by])] = DF_FUN_sub[rownr, num.column]
      }
    }
  }
  result[[grouping_variable_name]] <- NULL
  result <- order2(result, group.by)
  row.names(result) <- 1:nrow(result)
#  now convert NA dummies back to NA:
  if (!is.null(NA_replacement)) {
    for (varname in group.by) {
      if (varname %in% names(NA_replacement)) {
        result[[varname]][which(result[[varname]] == NA_replacement[varname])] <- NA
      }
    }
    if ('x_values' %in% names(NA_replacement)) {
      colnames(result)[which(colnames(result)==NA_replacement['x_values'])] <- make_unique_varname('NA', NA_replacement['x_values'], '_')
#      colnames(result)[which(colnames(result)==NA_replacement['x_values'])] <- make_NA_varname('NA', NA_replacement['x_values'], '_')
    }
  }
  if (row_total==TRUE) {
    result <- as.data.frame(result)
    total_colname <- make_grouping_variable_name('row.total')
    result[[total_colname]] <- apply(result[,(length(group.by)+1):ncol(result)], 1, sum)
  }
  return(result)
}



#' Count the number of occurrences of every unique element in a data object
#'
#' The function count() does roughly the same as the function table(). However, the differences are that (a) count() may make the result more directly accessible, because it returns the result in the form of a data.frame; and (b) that it is polymorphic, that is, three levels of complexity can be specified. Perhaps count() may align with a certain psychological expectation that when base R provides functions like mean(some_numerical_vector) and sum(some_numerical_vector), then count(some_numerical_vector) would seem to complement these. However, functions like mean() and sum() only work on numerical vectors, and return one numerical value only (and hence can be used more easily as sub-functions in e.g. apply()), whereas count can deal with various data types and returns a data.frame.
#'
#' @param x The object to count elements from. Can be a vector, a matrix or a data.frame.
#' @param group.by An optional vector of column names in x. It denotes groups, sub-groups, sub-sub-groups (etc., depending on the number of columns specified) by which counts need to be grouped. See examples.
#' @param split.by An optional column name in x, by which to split the counts 'horizontally'. That is, whereas 'group.by' is returned as rows, 'split.by' is returned as columns, whereby every value in 'split.by' will become a column. In that sense, it acts as a pivot specifier.
#' @param row.total Boolean, specifying whether row totals need to be included as the right-most column. This is only relevant IFF split.by is provided. The column will be called 'row.total'; if that column name already exists in x, then 'row.total' will be trailed by underscores until a unique column name is generated.
#'
#' @return A data.frame. If 'x' is a vector, the data.frame has as the first column 'element' which are the elements in x (the vector) that have been counted, and a second column 'count' which represent the counts; if 'x' is a data.frame, then the first column(s) is/are the 'group.by' column(s), and a colum 'count' contains the actual counts of the number of rows for the unique number of rows for 'group.by'. If 'split.by' is also specified, then the return data.frame consists of the columns specified by 'group.by' and the unique values in 'split.by'. See examples.
#'
#' @examples my_num_vector = c(1,1,1,4,5,5)
#' mean(my_num_vector)
#' sum(my_num_vector)
#' table(my_num_vector)
#' count(my_num_vector)
#'
#' @examples my_str_vector = c('R', 'R', 'R', 'S', 'T', 'T')
#' table(my_str_vector)
#' count(my_str_vector)
#'
#' @examples my_DF <- data.frame(var1=rep(c('A','B','C'), 2), var2=c(1,1, 2,2, 3,3),
#' var3=rep(c('bbb','aaa','bbb'), 2))
#' count(my_DF, c('var1', 'var2'))
#' count(my_DF, c('var1', 'var3'))
#' count(my_DF, c('var2', 'var3'))
#' count(my_DF, 'var3')
#' #and compare with:
#' count(my_DF$var3)
#'
#' @examples my_DF = data.frame(var1=factor(c(rep('low', 4),rep('medium', 4),rep('high', 4)),
#' levels=c('low', 'medium', 'high')), var2=c(1, 2,2, 3,3,3, 4,4,4,4, 3, 2),
#' var3=rep(c('bbb','aaa','bbb'), 4), stringsAsFactors=FALSE)
#' count(my_DF, c('var3', 'var2'), 'var1')
#' # The counts are grouped by unique combinations of 'var3' and 'var2', ...
#' # ...and split out by the unique content of 'var1'.
#' # Note that if levels are given (as in this case), then the columns for 'split.by'...
#' # ...are ordered according to the sequence of the levels; otherwise in alphanumerical order.
#'
#' @examples # Also non-factors can be used for 'split.by':
#' count(my_DF, c('var1', 'var3'), 'var2')
#'
#' @examples # For the 'group.by' variable, NAs are treated as 'factor'.
#' # When there are NAs in the 'split.by' column, then an extra NA column is returned, ...
#' # ...specifying the counts of the NAs:
#' my_DF_w_NA = my_DF # same as above, but now...
#' my_DF_w_NA$var1[1] <- NA
#' my_DF_w_NA$var2[c(6,10)] <- NA
#' my_DF_w_NA$var3[10] <- NA
#' count(my_DF_w_NA, c('var1', 'var3'), 'var2')
#'
#' @examples # To show the idea of row totals:
#' count(my_DF_w_NA, c('var2', 'var3'), 'var1', row.total=TRUE)
count <- function(x, group.by=NULL, split.by=NULL, row.total=FALSE) {
  msg <- NULL
  if (is.null(x)) {
    msg = "- The parameter 'x' should be a vector, matrix or data.frame"
  } else {
    if (sum(is.vector(x), is.matrix(x), is.data.frame(x)) > 0) {
      # all good
    } else {
      if (sum(is.factor(x), (typeof(x)=='integer'))==2) {
        # it's a vector consisting of factors, which is allowable
      } else {
        msg = "- The parameter 'x' should be a vector, matrix or data.frame"
      }
    }
  }

  test_var_parameter <- function(x, parameter_name, parameter_string) {
    msg <- NULL
    if (sum((is.na(parameter_name)), (is.nan(parameter_name))) > 0) {
      if (!is.null(msg)) {msg = paste(msg, "\n")}
      msg = paste(msg, "- Parameter '", parameter_string, "' should be either NULL or a vector of one or more character strings that denote column names in 'x'", sep='')
    } else {
      if (!is.null(parameter_name)) {
        if (sum(is.vector(parameter_name), is.character(parameter_name)) == 2) {
          if (sum(is.matrix(x), is.data.frame(x)) == 0) {
            if (!is.null(msg)) {msg = paste(msg, "\n")}
            msg = paste(msg, "- Parameter '", parameter_string, "' should be either NULL or a vector of one or more character strings that denote column names in 'x'", sep='')
          } else { # OK, it apparently is a matrix or data.frame
            wrong_col_names <- which(parameter_name %notin% colnames(x))
            if (length(wrong_col_names) > 0) {
              if (!is.null(msg)) {msg = paste(msg, "\n")}
              msg = paste(msg, "- Parameter '", parameter_string, "' specifies column ", sep='')
              if (length(wrong_col_names) == 1) {
                msg = paste(msg, "name that is not in 'x': ", paste(parameter_name[wrong_col_names], collapse=', '), sep='')
              } else {
                msg = paste(msg, "names that are not in 'x': ", paste(parameter_name[wrong_col_names], collapse=', '), sep='')
              }
            }
          }
        } else {
          msg = paste(msg, "- Parameter '", parameter_string, "' should be either NULL or a vector of one or more character strings that denote column names in 'x'", sep='')
        }
      }
    }
    return(msg)
  }
  parameter_test <- test_var_parameter(x, group.by, 'group.by')
  if (!is.null(parameter_test)) {
    if (!is.null(msg)) {msg = paste(msg, "\n")}
    msg = paste(msg, parameter_test, sep='')
  }
  parameter_test <- test_var_parameter(x, split.by, 'split.by')
  if (!is.null(parameter_test)) {
    if (!is.null(msg)) {msg = paste(msg, "\n")}
    msg = paste(msg, parameter_test, sep='')
  }
  if (!is.null(split.by) & !is.null(group.by)) {
    if (split.by %in% group.by) {
      if (!is.null(msg)) {msg = paste(msg, "\n")}
      msg = paste(msg, "- Variables specified in 'group.by' cannot be used in 'split.by'", sep='')
    }
  }

  if ((is.matrix(x)) | (is.data.frame(x))) {
    if ((is.null(group.by)) & (is.null(split.by))) {
      if (!is.null(msg)) {msg = paste(msg, "\n")}
      msg = paste(msg, "- If a data.frame or matrix is provided for 'x', then at least parameter 'group.by' must be provided as well", sep='')
    }
  }
  if (!is.logical(row.total)) {
    if (!is.null(msg)) {msg = paste(msg, "\n")}
    msg = paste(msg, "- Parameter 'row.total' needs to be a Boolean", sep='')
  }
  if (!is.null(msg)) {
    stop(msg)
  }

  # actual function

  if (is.vector(x)) {
    count_vector(x)
  } else if (sum(is.factor(x), (typeof(x)=='integer'))==2) {
    count_vector(x)
  } else { # matrix or data.frame
    if (!is.null(group.by)) {
      if (length(group.by)==1) {
        if (is.null(split.by)) {
          result <- count_vector(x[[group.by]], group.by)
          if (is.factor(x[[group.by]])) {
            return(result[match(levels(x[[group.by]]), result[[group.by]]),])
          } else {
            return(result)
          }
        } else {
          FUN_per_group_per_split(x, group.by, split.by, row.total)
        }
      } else {
        if (is.null(split.by)) {
          FUN_DF(x, group.by)
        } else {
          FUN_per_group_per_split(x, group.by, split.by, row.total)
        }
      }
    }
  }
}


#' Aggregates a numerical column
#'
#' The function aggro() does roughly the same as the function aggregate(). However, the differences are that (a) aggro() returns the results ordered by all grouping columns; (b) aggro() is polymorphic, that is, two levels of complexity can be specified; and (c) aggro also displays the results per NA if NAs are values in the grouping column(s) or split.by column.
#'
#' @param x A data.frame with at least one column that can be treated as 'factor', and one numerical column.
#' @param group.by A vector of column names in x. It denotes groups, sub-groups, sub-sub-groups (etc., depending on the number of columns specified) by which counts need to be grouped. See examples.
#' @param split.by An optional column name in x, by which to split the counts 'horizontally'. That is, whereas 'group.by' is returned as rows, 'split.by' is returned as columns, whereby every value in 'split.by' will become a column. In that sense, it acts as a pivot specifier. This is one of the main differentiators with aggregate().
#' @param num.column A string, denoting the name of the numerical column in x on which to apply function FUN.
#' @param FUN A function that returns a single numerical value when applied to a numerical vector; examples are \code{sum}, \code{mean}, \code{sd}, \code{se}, \code{var}, etc.
#'
#' @return A data.frame. The first column(s) is/are the 'group.by' column(s). If 'split.by' is not provided, then the final column name is the column name of the 'num.column'; if 'split.by' is provided, then the return data.frame consists of the columns specified by 'group.by' and the unique values in 'split.by'. See examples.
#'
#' @examples my_DF = data.frame(var1=factor(c(rep('low', 4),rep('medium', 4),rep('high', 4)),
#' levels=c('low', 'medium', 'high')), var2=c(1, 2,2, 3,3,3, 4,4,4,4, 3, 2),
#' var3=rep(c('bbb','aaa','bbb'), 4), stringsAsFactors=FALSE)
#' aggro(my_DF, c('var3', 'var1'), 'var2', sum)
#' # Just like with the function count(), the results are grouped by unique combinations of ...
#' # ...'var3' and 'var1'. Note the following:
#' # * Column names are given in parenthesis (either single, ', or double, ")
#' # * Functions are not specified with parenthesis
#' # * The output is ordered; in principle according to alphanumerical order, except..
#' #   ... when a 'group.by' column is an ordered factor, the factor order is followed.
#' # However, that said, up to this point, the results are the same as with aggregate():
#' aggregate(var2~var3+var1, my_DF, sum)
#' # Yet, it is getting more interesting/useful when either the results are split, ...
#' # ... or when there are NAs envolved; in both cases, aggro() digresses from aggegate();
#' # see the following:
#'
#' @examples # With split.by. Also non-factors can be used for 'split.by':
#' aggro(my_DF, group.by='var1', num.column='var2', FUN=sum, split.by='var3')
#'
#' @examples # With NAs. For the 'group.by' variable, NAs are treated as 'factor'.
#' # When there are NAs in the 'split.by' column, then an extra NA column is returned, ...
#' # ...specifying the counts of the NAs:
#' my_DF_w_NA = my_DF # same as above, but now...
#' my_DF_w_NA$var1[1] <- NA
#' my_DF_w_NA$var2[c(6,10)] <- NA
#' my_DF_w_NA$var3[10] <- NA
#' aggro(my_DF_w_NA, c('var1', 'var3'), 'var2', sum)
#' # Compare with:
#' aggregate(var2~var1+var3, my_DF_w_NA, sum)
#'
#' # And indeed, with a split.by:
#' my_DF_w_NA$var3[8] <- NA
#' aggro(my_DF_w_NA, group.by='var1', num.column='var2', FUN=sum, split.by='var3')
#'
aggro <- function(x, group.by, num.column, FUN, split.by=NULL) {
  msg <- NULL
  if (is.null(x)) {
    msg = "- The parameter 'x' should be a data.frame"
  } else if (!is.data.frame(x)) {
    msg = "- The parameter 'x' should be a data.frame"
  }
  if (!is.null(msg)) {stop(msg)} # stop directly here, because for subsequent checks, it is assumed / required that x is a data.frame

  check_var <- function(varname, x) {
    msg <- NULL
    if (!is.null(varname)) {
      if (is.vector(varname) && is.character(varname)) {
        if (sum(varname %in% colnames(x)) == length(varname)) {
          # all good
        } else {
          if (!is.null(msg)) {msg = paste(msg, "\n")}
          if (sum(varname %notin% colnames(x)) > 1) {
            faulty_columns = paste("c('", paste(varname[which(varname %notin% colnames(x))], collapse = "', '"), "')", sep='')
            msg = paste(msg, "- Column names ", faulty_columns, " in '", varname, "' are not in 'x'", sep='')
          } else {
            faulty_columns = varname[which(varname %notin% colnames(x))]
            msg = paste(msg, "- Column name ", faulty_columns, " in '", varname, "' is not in 'x'", sep='')
          }
        }
      } else {
        if (!is.null(msg)) {msg = paste(msg, "\n")}
        msg = paste(msg, "- Parameter 'varname' should be (a vector of) one or more character strings denoting column names in 'x'", sep='')
      }
    }
    return(msg)
  }

  msg2 <- check_var(group.by, x)
  if (!is.null(msg2)) {
    if (!is.null(msg)) {msg = paste(msg, "\n")}
    msg = paste(msg, msg2, sep='')
  }
  msg2 <- check_var(split.by, x)
  if (!is.null(msg2)) {
    if (!is.null(msg)) {msg = paste(msg, "\n")}
    msg = paste(msg, msg2, sep='')
  }
  msg2 <- check_var(num.column, x)
  if (!is.null(msg2)) {
    if (!is.null(msg)) {msg = paste(msg, "\n")}
    msg = paste(msg, msg2, sep='')
  }

  if (sum(group.by %in% split.by) > 0) {
    if (!is.null(msg)) {msg = paste(msg, "\n")}
    msg = paste(msg, "- There cannot be an overlap between column names in 'group.by' and 'split.by'", sep='')
  }
  if (sum(group.by %in% num.column) > 0) {
    if (!is.null(msg)) {msg = paste(msg, "\n")}
    msg = paste(msg, "- There cannot be an overlap between column names in 'group.by' and 'num.column'", sep='')
  }
  if (sum(num.column %in% split.by) > 0) {
    if (!is.null(msg)) {msg = paste(msg, "\n")}
    msg = paste(msg, "- There cannot be an overlap between column names in 'split.by' and 'num.column'", sep='')
  }

  if (is.null(FUN)) {
    if (!is.null(msg)) {msg = paste(msg, "\n")}
    msg = paste(msg, "- A function name for a function with a single numerical outcome must be provided for FUN", sep='')
  } else {
    function_name <- deparse(substitute(FUN))
    if (is.function(FUN)) {
      res <- NULL
      try(
        res <- FUN(c(1,2,3)),
        silent = TRUE
      )
      if (is.null(res)) {
        if (!is.null(msg)) {msg = paste(msg, "\n")}
        if (exists(function_name)) {
          msg = paste(msg, "- Function '", function_name, "()' does not provide a single number when applied to a numerical vector", sep='')
        } else {
          msg = paste(msg, "- Function '", function_name, "()' not accessible", sep='')
        }
      } else {
        if ((is.numeric(res)) && (length(res)==1)) {
          if (is.null(num.column)) {
            if (!is.null(msg)) {msg = paste(msg, "\n")}
            msg = paste(msg, "- A numerical column 'num.column' is required", sep='')
          } else {
            if (!is.numeric(x[[num.column]])) {
              if (!is.null(msg)) {msg = paste(msg, "\n")}
              msg = paste(msg, "- Column 'num.column' should have numerical content", sep='')
            }
          }
        } else {
          if (!is.null(msg)) {msg = paste(msg, "\n")}
          msg = paste(msg, "- Function FUN should return a single numerical value when applied to a numerical vector", sep='')
        }
      }
    } else {
      if (!is.null(msg)) {msg = paste(msg, "\n")}
      msg = paste(msg, "- Parameter 'FUN' not recognized as function", sep='')
    }
  }
  if (!is.null(msg)) {
    stop(msg)
  }

  # actual function call here
  if (is.null(split.by)) {
    FUN_DF(the_DF=x, group.by, FUN, num.column)
  } else {
    FUN_per_group_per_split(DF=x, group.by, split.by, row_total=FALSE, FUN=FUN, num.column=num.column)
  }
}
