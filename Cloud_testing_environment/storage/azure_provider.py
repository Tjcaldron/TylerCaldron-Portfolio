import azure.storage.blob as azure_blob
from azure.identity import DefaultAzureCredential
from storage.base import StorageProvider

'''
Sets up the Azure envirnonment used for the API and for testing. Think of it as half of the brain for accessing the cloud provider.
It could be revamped to work with another storage provider if so wished, it would just take some minor modifications.
'''

# Initiates a class that interacts with the Azure blob storage throughout the project.
class AzureStorageProvider(StorageProvider):
    # Initializes the class and the credentials used throughout the class
    def __init__(self, creds: dict):
        self.account_url = creds["account_url"]
        self.blob_service_client = azure_blob.BlobServiceClient(account_url=self.account_url, credential=DefaultAzureCredential())

    # Helper function to extract container and blob path from the directory.
    def _get_container_and_blob(self, path: str, file_name: str):
        # Sets up the path so it looks something like this: "/folder/subfolder/"
        path = path.strip('/')
        parts = path.split('/')
        container = parts[0] if parts else 'default-container'
        blob_path = '/'.join(parts[1:] +[file_name]) if len(parts) > 1 else file_name
        # Returns the value as a tuple
        return container, blob_path
    
    # Creates or overwrites a file in the Azure blob storage
    def create_file(self, path: str, file_name: str, content: bytes) -> bool: 
        container, blob_path = self._get_container_and_blob(path, file_name)
        # This print statement is so the user has some sort of notification that things are happening
        # Can be commented out to improve run time.
        print(f"[Azure] Creating file in container {container}, blob: {blob_path}")
        # Checks to see if the file was created to the right container/blob and returns True if so
        try:
            container_client =self.blob_service_client.get_container_client(container)
            if not container_client.exists():
                container_client.create_container()
            blob_client = container_client.get_blob_client(blob_path)
            blob_client.upload_blob(content, overwrite=True)
            print("[Azure] File created successfully.")
            return True   
        except Exception as e:
            print(f"[Azure] Error creating file: {e}")
            return False
        
    # Creates a copy of an existing file in the blob    
    def download_file(self, path: str, file_name: str) -> bytes:
        container, blob_path = self._get_container_and_blob(path, file_name)
        # Gets the data from the new copy and compares it to the original, if it is the same
        # then it returns True, otherwise it returns False
        try:
            blob_client = self.blob_service_client.get_blob_client(container=container, blob=blob_path)
            stream = blob_client.download_blob()
            return stream.readall()
        except Exception as e:
            print(f"[Azure] Error downloading file: {e}")
            raise FileNotFoundError("File not found in Azure storage.")

    # Deletes a specified file from the blob    
    def delete_file(self, path:str, file_name:str) -> bool:
        container, blob_path = self._get_container_and_blob(path, file_name)
        # Another message for the user to show that it is performing an action
        print(f"[Azure] Deleting file from container {container}, blob: {blob_path}")
        # Checks to see if the file was correctly deleted and returns True if it was, otherwise
        # it returns false.
        try:
            blob_client = self.blob_service_client.get_blob_client(container=container, blob=blob_path)
            if not blob_client.exists():
                print(f"[Azure] Blob does not exist: {blob_path}")
                return False
            blob_client.delete_blob()
            return True
        except Exception as e:
            print(f"[Azure] Error deleting file: {e}")
            return False
        