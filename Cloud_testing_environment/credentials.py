from storage.azure_provider import AzureStorageProvider
from storage.s3_provider import S3StorageProvider

# Creates the credentials for the blob and S3 bucket used throughout the project
CREDENTIALS = {
    "azure-dev": 
        {
        "provider": "azure", "account_url": "https://tyfirstblob.blob.core.windows.net"
        }, 
    "aws-prod": 
        {
            "provider": "aws", "bucket": "tc-first-bucket",
            #can be replaced. These are my personal keys but I made the bucket for this project.
            "access_key": "DUMMYKEYFORTESTINGONLY",
            "secret_key": "DummySecretKey"
        }
    }

# Checks to see if the credentials requested are part of the set dictionary
def get_storage_provider(connection_name: str):
    creds = CREDENTIALS[connection_name]

    if not creds:
        raise ValueError(f"No credentials found for connection name: {connection_name}")
    
    if creds["provider"] == 'azure':
        return AzureStorageProvider(creds)
    elif creds["provider"] == 'aws':
        return S3StorageProvider(creds)
    # Used if the provider is somehow an unrecognized value
    else:
        raise ValueError(f"unsopported provider: {creds['provider']}")