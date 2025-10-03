from databricks.sdk.core import Config, oauth_service_principal
from databricks import sql
import pandas as pd


def fetch_dataset(catalog, schema, table, server_hostname, http_path):
    # Replace hardcoded values with function arguments
    conn = sql.connect(
        server_hostname=server_hostname,
        http_path=http_path,
        auth_type="databricks-oauth"
    )

    query = f"SELECT * FROM {catalog}.{schema}.{table}"
    cursor = conn.cursor()
    cursor.execute(query)
    data = cursor.fetchall()
    columns = [desc[0] for desc in cursor.description]  # Get column names from cursor
    data = pd.DataFrame(data, columns=columns)  # Convert to pandas DataFrame
    cursor.close()
    conn.close()

    # Ensure data is always a pandas DataFrame
    if data.empty:
        print("Query returned no results. Returning an empty DataFrame.")
        data = pd.DataFrame(columns=columns)  # Return an empty DataFrame with column names

    return data
