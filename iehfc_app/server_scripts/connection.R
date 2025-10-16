
# Run the consolidated Python file to define the credential_provider and fetch_dataset functions
reticulate::py_run_file("server_scripts/databricks_utils.py")

# Import the Python functions into R
fetch_dataset <- reticulate::py_eval("fetch_dataset", convert = FALSE)



databricks_connect_and_read <- function(catalog, schema, table, server_hostname, http_path) {
  if (!requireNamespace("reticulate", quietly = TRUE)) stop("reticulate package is required.")

  data <- fetch_dataset(
    catalog = catalog,
    schema = schema,
    table = table,
    server_hostname = server_hostname,
    http_path = http_path
  )

  # Convert the Python pandas DataFrame to an R data frame
  r_dataframe <- reticulate::py_to_r(data)

  # Return the converted data
  return(r_dataframe)
}
