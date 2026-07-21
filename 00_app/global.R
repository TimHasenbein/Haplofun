# file to store user names
CREDENTIALS_FILE <- "credentials.rds"

# function to load user data
load_users <- function() {
  if (file.exists(CREDENTIALS_FILE)) {
    users <- readRDS(CREDENTIALS_FILE)
    if (is.null(users)) {
      return(initialize_default_users())
    }
    return(users)
  } else {
    return(initialize_default_users())
  }
}

# initialize default users
initialize_default_users <- function() {
  tibble::tibble(
    user = "admin",
    password_hash = sodium::password_store("testpass") 
  )
}

# save user data
save_users <- function(data) {
  tryCatch({
    saveRDS(data, CREDENTIALS_FILE)
  }, error = function(e) {
    message("Error during saving: ", e$message)
  })
}

# --- Initialize data bank ---
users_data <- load_users() 

# check credentials
check_credentials <- function(user, password) {
  user_row <- users_data[users_data$user == user, ]
  if (nrow(user_row) == 1) {
    return(sodium::password_verify(user_row$password_hash, password))
  } else {
    return(FALSE)
  }
}

# register new users
register_user <- function(user, password) {
  if (user %in% users_data$user) {
    return("existing") 
  }
  new_hash <- sodium::password_store(password)
  users_data <<- users_data %>%
    tibble::add_row(user = user, password_hash = new_hash)
  save_users(users_data)
  return("success")
}