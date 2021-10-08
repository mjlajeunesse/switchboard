# Environment that holds various global variables and settings for switchboards
switchboard.env <- new.env(parent = emptyenv())

set_switchboardVariable <- function(switchboardVariable, variableValue) 
									assign(switchboardVariable, variableValue, envir = switchboard.env)
get_switchboardVariable <- function(switchboardVariable) 
									unlist(mget(switchboardVariable, envir = switchboard.env, ifnotfound = NA))
