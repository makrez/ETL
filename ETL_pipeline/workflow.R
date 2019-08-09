# Example Workflow with flights dataset
library(here)
source(here::here("utils.R"))
source(here::here("wrapper.R"))

df <- transform.table(flights)

