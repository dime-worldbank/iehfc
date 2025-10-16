from databricks.sdk.core import Config, oauth_service_principal
from databricks import sql
import pandas as pd

MAX_ROWS = 500000
def check_table_size(catalog, schema, table, conn):
    cursor = conn.cursor()
    size_query = f"SELECT COUNT(*) as row_count FROM {catalog}.{schema}.{table}"
    cursor.execute(size_query)
    row_count = cursor.fetchone()[0]
    cursor.close()
    return row_count

def fetch_dataset(catalog, schema, table, server_hostname, http_path):
    conn = None
    cursor = None
    
    try:
        conn = sql.connect(
            server_hostname=server_hostname,
            http_path=http_path,
            auth_type="databricks-oauth",
            _connect_timeout=60,  # Connection timeout in seconds
            _request_timeout=120   # Request timeout in seconds
        )
        
        # Check table size first
        row_count = check_table_size(catalog, schema, table, conn)
        
        if row_count > MAX_ROWS:
            raise ValueError(f"Table too large to fetch completely. Table has {row_count:,} rows, maximum allowed is {MAX_ROWS:,}")
        
        # Fetch all data from the table
        query = f"SELECT * FROM {catalog}.{schema}.{table}"
        
        cursor = conn.cursor()
        cursor.execute(query)
        
        # Fetch data using standard method
        data = cursor.fetchall()
        columns = [desc[0] for desc in cursor.description]
        data = pd.DataFrame(data, columns=columns)
        
        # Return data or empty DataFrame if no results
        if data.empty:
            return pd.DataFrame()
        
        return data
        
    finally:
        # Always clean up resources
        if cursor:
            cursor.close()
        if conn:
            conn.close()
