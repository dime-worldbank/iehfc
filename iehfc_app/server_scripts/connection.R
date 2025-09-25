# Retrieve environment variables
server_hostname <- Sys.getenv("DATABRICKS_SERVER_HOSTNAME")
http_path <- Sys.getenv("DATABRICKS_HTTP_PATH")
client_id <- Sys.getenv("DATABRICKS_CLIENT_ID")
client_secret <- Sys.getenv("DATABRICKS_CLIENT_SECRET")

# Pass environment variables to Python
reticulate::py_run_string(
  paste0(
    "server_hostname = '", server_hostname, "'\n",
    "http_path = '", http_path, "'\n",
    "client_id = '", client_id, "'\n",
    "client_secret = '", client_secret, "'\n"
  )
)

# Run the consolidated Python file to define the credential_provider and fetch_dataset functions
reticulate::py_run_file("server_scripts/databricks_utils.py")

# Import the Python functions into R
fetch_dataset <- reticulate::py_eval("fetch_dataset", convert = FALSE)



databricks_connect_and_read <- function(catalog, schema, table) {
  if (!requireNamespace("reticulate", quietly = TRUE)) stop("reticulate package is required.")

  tryCatch({
    data <- fetch_dataset(
      catalog = catalog,
      schema = schema,
      table = table,
      server_hostname = server_hostname,
      http_path = http_path,
      client_id = client_id,
      client_secret = client_secret
    )

    # Convert the Python pandas DataFrame to an R data frame
    r_dataframe <- reticulate::py_to_r(data)

    # Return the converted data
    return(r_dataframe)
  }, error = function(e) {
    print(paste("Error connecting to Databricks via fetch_dataset:", e$message))
  })
}
