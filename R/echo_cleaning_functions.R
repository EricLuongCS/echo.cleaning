# For ECHO Data Cleaning
# This has been catered to a specific dataset

# (1)
# Get TOP and BOTTOM n observations for a specific variable

#' Minimum and Maximum Values
#'
#' @description A function that gets the top and bottom n values for each
#'   specified variable.
#' @param dataframe The dataframe to perform the function on (make sure desired
#'   filters are applied) and that variables you want top/bottom values for are
#'   numeric variables
#' @param base_variables The variables that identify which row each top/bottom
#'   value comes from. These variables will not have top/bottom values
#' @param temp_addtl_variables These are the variables to find top/bottom values
#'   for. If "all" is provided, the function will attempt to get all variables
#'   that are not base_variables.
#' @param numberTopBottom This is the number of top/bottom values to get. That
#'   is, if you want the top 10/ bottom 10 values, then numberTopBottom = 10
#' @details This should return a nested dataframe with a column of dataframes of
#'   top and bottom values for each variable.
#' @return A nested dataframe

getMinMax <- function(dataframe, base_variables = c("PatientID", "StudyDate", "StudyNotes", "InterpreterID"),
                      temp_addtl_variables = "all", numberTopBottom)
{
  addtl_variables <- c()

  if(is_scalar_character(temp_addtl_variables) & temp_addtl_variables[1] == "all")
  {

    addtl_variables <- colnames(dataframe)[(length(base_variables)+1):ncol(dataframe)]

  } else if(is_scalar_character(temp_addtl_variables) | is.vector(temp_addtl_variables))
  {

    all_var_names <- colnames(dataframe)

    # Get variables that are in the dataset and set those to the addtl_variables to look at
    vars_in_dataframe <- temp_addtl_variables[which(temp_addtl_variables %in% all_var_names)]
    addtl_variables <- vars_in_dataframe

    # Look for variables that are not in the dataset...
    vars_NOT_in_dataframe <- temp_addtl_variables[-which(temp_addtl_variables %in% all_var_names)]

    if(length(vars_NOT_in_dataframe) > 0) # If there are variables that are not in the dataframe, then spit out a warning
    {
      cat(red("WARNING: Check spelling of specified variables. The following variable(s) were dropped because they do not exist in specified dataset:"))
      cat("\n")
      cat(paste(1:length(vars_NOT_in_dataframe), ". ", vars_NOT_in_dataframe, sep = "", collapse = "\n"))
      cat("\n")
    }

  }

  if(length(addtl_variables) > 0)
  {

    base_addtl_vars <- c(base_variables, addtl_variables)

    # Group the dataframe by the addtl_variables
    grouped_dataframe <- dataframe %>%
      select(!! base_addtl_vars) %>% # NOTE: !! unquotes the input, so that it's evaluated without quotes
      gather(addtl_variables, key = variable, value = reading) %>%
      group_by(variable)

    # Get the bottom (lowest values)
    bottom_dataframe <- grouped_dataframe %>%
      top_n(-(numberTopBottom))

    # Get the top (highest values)
    top_dataframe <- grouped_dataframe %>%
      top_n(numberTopBottom)

    # Calculate the mean and sd for the variables
    mean_sd <- grouped_dataframe %>%
      summarise(mean_reading = mean(reading, na.rm = TRUE), std_reading = sd(reading, na.rm = TRUE)) %>%
      mutate(mean_sd = str_c(round(mean_reading, 3), round(std_reading, 3), sep = " +/- ")) %>%
      select(-c(mean_reading, std_reading))

    # Combine the top and bottom values into a single dataframe
    bottom_top_dataframe <- bind_rows(bottom_dataframe, top_dataframe) %>%
      group_by(variable) %>%
      arrange(reading) %>%
      distinct() %>%
      nest()
    # Merge the mean and sd to the dataframe above
    bottom_top_mean_sd <- bottom_top_dataframe %>%
      left_join(mean_sd, by = "variable")

    # Return variables in the same order as entered
    bottom_top_mean_sd$variable <- factor(bottom_top_mean_sd$variable, levels = addtl_variables)
    bottom_top_mean_sd <- bottom_top_mean_sd[order(bottom_top_mean_sd$variable), ]
    bottom_top_mean_sd$variable <- as.character(bottom_top_mean_sd$variable)

    return(bottom_top_mean_sd)
  } else
  {
    warning("There are no variables specified.")
  }
}
# ---



# ~~~
# (2)
# Flag Duplicates Function

#' NumPlicate Flagger
#'
#' @description A function that looks at the multiple measurements for each
#'   variable (var_1, var_2,...) and calculates the coefficient of variation
#'   (CV) from between the measurements. It then looks at the CV distribution of
#'   CV's for all rows for those variables. Outliers will be measurements that
#'   vary too much based on Z-score.
#' @param data The dataframe to perform the function on (make sure desired
#'   filters are applied) and that variables you want to check are numeric
#'   variables
#' @param base_variables The variables that identify which row each outlier
#'   value comes from.
#' @param variables These are the variables to find outlier values based on
#'   precision of measurements. If "all" is provided, the function will attempt
#'   to get all variables that are not base_variables.
#' @details This should return a nested dataframe with a column of dataframes of
#'   outlier values for each variable
#' @return A nested dataframe


NumPlicate_Flagger <- function(data, base_variables = c("PatientID", "StudyDate", "StudyNotes", "InterpreterID"), variables = "all")
{

  temp_variables <- colnames(data)[5:ncol(data)]
  temp_variables <- str_replace(temp_variables, "(?<=_)([[:digit:]]*|avg)$", "chihuahua")  # Replace digits or "avg" at the end of each variable name
  temp_variables <- str_replace(temp_variables, "(?<=_)(avg2)$", "poodle")  # Replace "avg2" at the end of each variable name
  temp_variables <- str_replace(temp_variables, "_poodle", "_2")
  temp_variables <- str_replace(temp_variables, "_chihuahua", "")

  # Count how many n_plicates there are for each variable
  # Variables with only one measurement (no _plicates), then they will be disregarded
  num_plicates <- table(temp_variables) - 1   # Subtract one to ignore the _avg variable as well as those with only one measurement
  num_plicates <- num_plicates[num_plicates > 1]  # Variables must have at least _2 plicates to be considered (cannot calculate an sd with only 1...)

  # Get variables that have n_plicates
  runnable_variables <- names(num_plicates)

  if(is_scalar_character(variables) & variables[1] == "all")
  {

    desired_variables <- runnable_variables

  } else if((is_scalar_character(variables) | is.vector(variables)) & variables[1] == "all")
  {

    desired_variables <- runnable_variables[which(runnable_variables %in% variables)]
    dropped_variables <- variables[-which(variables %in% runnable_variables)]

    if(length(dropped_variables) > 0)
    {

      cat(red("WARNING: Check spelling of specified variables. The following variable(s) were dropped because they do not exist in specified dataset:"))
      cat("\n")
      cat(paste(1:length(dropped_variables), ". ", dropped_variables, sep = "", collapse = "\n"))
      cat("\n")

    }

  }

  # Create a dataframe that has the number of _plicates for each variable
  num_plicates <- as.data.frame(num_plicates) %>%
    mutate(temp_variables = as.character(temp_variables))
  desired_numPlicates <- as.data.frame(desired_variables) %>%
    mutate(desired_variables = as.character(desired_variables)) %>%
    left_join(num_plicates, by = c("desired_variables" = "temp_variables"))

  # Create a list to hold the num_plicate data
  list_of_data <- list()
  data_storage <- tibble(variables = desired_variables)

  # If there are desired variables at all
  if(length(desired_variables) > 0)
  {
    for(i in 1:nrow(desired_numPlicates))
    {

      # Create a new vector of desired variables adding in the _numPlicates
      temp_desired <- desired_numPlicates[i, "desired_variables"]
      # Recall: Only variables with at least _2 plicates are included
      temp_desired2 <- c(temp_desired, paste(temp_desired, 2:desired_numPlicates[i, "Freq"], sep = "_"))

      # Calculate the rowCV = rowMean/ rowSD
      data2 <- data %>%
        select(base_variables, temp_desired2) %>%
        mutate(!!(paste(temp_desired, "CV", sep = "_")) := (apply(.[temp_desired2], MARGIN = 1, FUN = function(x) sd(x, na.rm = TRUE)) / rowMeans(.[temp_desired2], na.rm = TRUE)))

      # Calculate the mean and sd of the coefficient of variation (CV)
      temp_mean <- colMeans(data2[, (paste(temp_desired, "CV", sep = "_"))], na.rm = TRUE)
      temp_sd <- apply(data2[, (paste(temp_desired, "CV", sep = "_"))], MARGIN = 2, FUN = function(x) sd(x, na.rm = TRUE))

      # Use the mean and sd to standardize the CV ~ (CV - meanCV)/ sdCV
      data2[, paste(temp_desired, "ZScore", sep = "_")] <- ((data2[, paste(temp_desired, "CV", sep = "_")] - temp_mean)/temp_sd)

      # If the CV > 0.1 or standardized CV > 3.5, then flag it
      data2$flagged <- 0
      data2$flagged[which(data2[, paste(temp_desired, "CV", sep = "_")] > 0.1 | data2[, paste(temp_desired, "ZScore", sep = "_")] > 3.5)] <- 1

      # Only include the flagged rows
      data2 <- data2 %>%
        filter(flagged == 1)

      if(nrow(data2) > 0)
      {
        list_of_data[[i]] <- data2
      } else
      {
        list_of_data[[i]] <- data2
        cat(i, ". ", temp_desired, " does not have any flagged num_plicates. \n", sep = "")
      }
    }
    # Store everything into one tibble
    data_storage <- add_column(data_storage, flagged_data = list_of_data)
    return(data_storage)

  } else
  {
    warning("There are no variables specified.",)
  }

}
