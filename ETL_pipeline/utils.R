# Load Libraries
library(here)
library(tidyverse)
library(nycflights13)


#------------------------------------------------------------------------------#
# Transform Functions #
#------------------------------------------------------------------------------#

transform.setDatatype <- function(df,
                                  input_datatype){
  # Function for parsing transform_information object
  #
  # Args:
  #  df: data.frame
  #  input_datatype: string. Options: "numeric", "character", "time"
  #
  # Returns:
  #  Vector with old_name that fulfills datatype criterion
  
  df %>% filter(Datatype == input_datatype) %>% 
    select(Old_name) %>% pull %>% as.character
  
}

# # Example
# 
# # transform_object <- tibble(
# #   Old_name = c("a", "b"),
# #   Datatype = c("numeric", "character")
# # )
# 
# transform.setDatatype(transform_object, "numeric")

#------------------------------------------------------------------------------#


transform.AssignDatatype <- function(df,
                                     transform_object,
                                     data_type){
  # The function renames and sets data types for columns based on information
  # stored in the dataframe transform_object
  #
  # Args:
  #  df: dataframe to be transformed
  #  transform_object: dataframe, 
  #  column_name: column to be transformed
  #  data_type: string. Options: "character", "numberic", "Date"
  #
  # Returns:
  #  data frame
  
  transform_information <- transform_object
  
  if (length(transform.setDatatype(transform_information, "numeric")) > 0){
    df <- df %>% 
      mutate_at(transform.setDatatype(transform_information, "numeric"),
                funs(as.numeric))
  }
  if (length(transform.setDatatype(transform_information, "character")) > 0){
    df <- df %>% 
      mutate_at(transform.setDatatype(transform_information, "character"),
                funs(as.character))
  }
  if (length(transform.setDatatype(transform_information, "Date")) > 0){
    df <- df %>% 
      mutate_at(transform.setDatatype(transform_information, "Date"),
                funs(as.Date))
  }
  return(df)
}

# Example

# transform_object <- tibble(
#   Old_name = c("a", "b"),
#   Datatype = c("numeric", "character")
# # )
# 
# test_df <- tibble(
#   a = c("1","2","3"),
#   b = c("foo", "bar", "test")
# )
# 
# transform.AssignDatatype(test_df, transform_object = transform_object)
#------------------------------------------------------------------------------#

transform.renameObject <- function(df,
                                   transform_object,
                                   col_old_name,
                                   col_new_name){
  # Function to rename column names of a dataframe based on Object transform_object
  #
  # Args:
  #  df: dataframe on which to rename the columns
  #  col_old_name: colun name in transform_object whith old_names
  #  col_new_name: column name in transform_object with new_names
  #
  # Returns:
  #  dataframe with renamed columns
  
  col_old_name <- enquo(col_old_name)
  col_new_name <- enquo(col_new_name)
  
  new <- transform_object %>% 
    select(!!col_new_name) %>% pull
  
  old <- transform_object %>% 
    select(!!col_old_name) %>% pull
  
  rename_vector <- setNames(as.character(old),
                            as.character(new))
  
  df %>% select(!!!rename_vector)
}

# Example

# transform_object <- tibble(
#   Old_name = c("a", "b"),
#   New_name = c("new1","new2")
# )
# 
# test_df <- tibble(
#   a = c("1","2","3"),
#   b = c("foo", "bar", "test")
# )
# 
# transform.renameObject(test_df, transform_object, Old_name, New_name)


