from databricks.sdk.core import Config, oauth_service_principal
from databricks import sql
import pandas as pd
import os
from dotenv import load_dotenv

# Load environment variables from .env file
load_dotenv()

HOST = os.getenv("HOST")
CLIENT_ID = os.getenv("CLIENT_ID")
CLIENT_SECRET = os.getenv("CLIENT_SECRET")
HTTP_PATH = os.getenv("HTTP_PATH")


def fetch_dataset(catalog, schema, table):
    def credential_provider():

        # Check for missing environment variables
        if not all([HOST, HTTP_PATH, CLIENT_ID, CLIENT_SECRET]):
            raise RuntimeError("One or more required environment variables are not set.")

        config = Config(
            host=HOST,
            client_id=CLIENT_ID,
            client_secret=CLIENT_SECRET,
        )
        return oauth_service_principal(config)

    try:

        # Replace hardcoded values with function arguments
        conn = sql.connect(
            server_hostname=os.getenv("HOST"),  # Use environment variable for server_hostname
            http_path=os.getenv("HTTP_PATH"),  # Use environment variable for http_path
            credential_provider=credential_provider
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
    except Exception as e:
        raise RuntimeError(f"Error fetching dataset: {str(e)}")
