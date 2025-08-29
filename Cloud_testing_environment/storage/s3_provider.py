import boto3
from storage.base import StorageProvider
from botocore.exceptions import ClientError

'''
Sets up the AWS S3 envirnonment used for the API and for testing. Think of it as half of the brain for accessing the cloud provider.
It could be revamped to work with another storage provider if so wished, it would just take some minor modifications.
'''

# Initiates a class that interacts with the S3 Bucket storage throughout the project.
class S3StorageProvider(StorageProvider):
    # Initializes the class and the credentials used throughout the class.
    def __init__(self, creds: dict):
        self.bucket = creds["bucket"]
        self.s3_client = boto3.client(
            "s3", 
            aws_access_key_id=creds["access_key"],
            aws_secret_access_key=creds["secret_key"]
        )
    # A helper function for setting the bucket path from the directory.
    def _build_key(self, path: str, file_name: str)-> str:
        path = path.strip("/")
        # Sets up the path so it looks something like this: "/folder/subfolder/"
        return f"{path}/{file_name}" if path else file_name
    
     # Creates or overwrites a file in the S3 Bucket storage.
    def create_file(self, path: str, file_name: str, content:bytes) -> bool:
        key = self._build_key(path, file_name)
        # This print statement is so the user has a notification it is running.
        print(f"[S3] Creating a file.")
        # Checks to see if the file was created to the right bucket and returns True if so.
        try:
            self.s3_client.put_object(Bucket=self.bucket, Key=key, Body=content)
            return True
        except ClientError as e:
            print(f"[S3] Error creating file:{e}")
            return False
    
    # Creates a copy of an existing file in the bucket.
    def download_file(self, path: str, file_name: str) -> bytes:
        key = self._build_key(path, file_name)
        # Gets the data from the new copy and compares it to the original, if it is the same
        # then it returns True, otherwise it returns False.
        try:
            response = self.s3_client.get_object(Bucket=self.bucket, Key=key)
            return response["Body"].read()
        except ClientError as e:
            raise FileNotFoundError("File not found in s3 storage.")

    # Deletes a file from AWS S3 Bucket storage.
    def delete_file(self, path: str, file_name: str) -> bool:
        key = self._build_key(path, file_name)
        # Notifies the user that deletion has begun
        print(f"[S3] Attempting to delete file at: {key}")
        # If deletion is successful it returns True, otherwise it returns False.
        try: 
            response = self.s3_client.delete_object(Bucket=self.bucket, Key=key)
            print("[S3] File deletion successful (or file did not exist).")
            return True
        except ClientError as e:
            print(f"[S3] Error deleting file: {e}")
            return False
        