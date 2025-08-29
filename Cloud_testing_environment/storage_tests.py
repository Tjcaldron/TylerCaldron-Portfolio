import unittest
from storage.azure_provider import AzureStorageProvider
from storage.s3_provider import S3StorageProvider
'''
Unit tests that can be run to ensure that the Azure and S3 providers are functioning properly. 
Were primarily used in the testing phase but are still fully operational if you would like to test them out. 
'''

# Tests azure_provider to ensure that it properly creates, downloads, and deletes files.
class TestStorage(unittest.TestCase):
    @classmethod
    # Sets up the tests with information to compare against in credentials.py
    def setUp(cls):
        cls.provider = AzureStorageProvider({
            "provider": "azure",
            "account_url": "https://tyfirstblob.blob.core.windows.net"
        })
        # fills the basic information for the new "test file"
        cls.test_path = "test-container"
        cls.test_file_name = "unit_test.txt"
        cls.test_content = b"This is an Azure unit test!"

    # Creates the test file here using information from setUp()
    def test_create(self):
        success = self.provider.create_file(self.test_path, file_name=self.test_file_name, content=self.test_content)
        self.assertTrue(success, "File failed to create in Azure storage :C")

    # Creates a copy of the file and compares it to the original to ensure fidelity
    def test_download(self):
        self.provider.create_file(self.test_path, file_name=self.test_file_name, content=self.test_content)
        downloaded = self.provider.download_file(self.test_path, file_name=self.test_file_name)
        self.assertIsNotNone(downloaded, "Downloaded content is None.")
        self.assertEqual(downloaded, self.test_content, "Downloaded content does not match original.")

    # Deletes a specified file within the blob
    def test_delete(self):
        delete_file = "test_delete.txt"
        # Creates the file first with a new name
        self.provider.create_file(self.test_path, file_name=delete_file, content=self.test_content)
        result = self.provider.delete_file(self.test_path, file_name=delete_file)
        self.assertTrue(result, 'File failed to delete from storage')
        # Makes sure that it was the new file that was deleted and not another one
        with self.assertRaises(Exception):
            downloaded = self.provider.download_file(self.test_path, file_name=delete_file)
            self.assertIsNone(downloaded)
    
    # Cleans up the files made throughout the tests
    def tearDown(self):
        container_name = self.test_path.strip('/').replace('/','-').lower() or 'default'
        blob_client = self.provider.blob_service_client.get_blob_client(container=container_name, blob=self.test_file_name)
        # This is a safety measure to ensure that only files created during the tests are deleted
        if blob_client.exists():
            try: 
                self.provider.delete_file(path=self.test_path, file_name=self.test_file_name)
            except Exception as e:
                print(f"[Cleanup Warning] Failed to delete test file: {e}")

# Unit tests designed to test s3_provider's capacity like previously done with azure_provider
class TestS3Storage(unittest.TestCase):
    @classmethod
    # Sets up the values to be used with the tests in the bucket
    def setUp(cls):
        cls.provider = S3StorageProvider({
            "provider": "aws",
            "bucket": "tc-first-bucket",
            "access_key": "Dummy-Key", 
            "secret_key": "Dummy-secret"
        })
        # Fills the basic information for the new "test file" and sets the testing path
        cls.test_path = "test-folder"
        cls.test_file_name = "unit_test_s3.txt"
        cls.test_content = b"This is an S3 unit test!"

    # Creates the test file here using information from setUp()
    def test_S3_create(self):
        success = self.provider.create_file(self.test_path, file_name=self.test_file_name, content=self.test_content)
        self.assertTrue(success, "File failed to create in S3 storage.")

    # Creates a copy of the file and compares it to the original to ensure fidelity
    def test_S3_download(self):
        self.provider.create_file(self.test_path, file_name=self.test_file_name, content=self.test_content)
        downloaded = self.provider.download_file(self.test_path, file_name=self.test_file_name)
        self.assertIsNotNone(downloaded, "Downloaded content is None.")
        self.assertEqual(downloaded, self.test_content, "Downloaded content does not match the original.")
    
    # Deletes a specified file within the bucket
    def test_S3_delete(self):
        delete_file = "s3_test_delete.txt"
        self.provider.create_file(self.test_path, file_name=delete_file, content=self.test_content)
        result = self.provider.delete_file(self.test_path, file_name=delete_file)
        self.assertTrue(result, 'File failed to delete from S3 storage.')
         # Makes sure that it was the new file that was deleted and not another one
        with self.assertRaises(FileNotFoundError):
            self.provider.download_file(self.test_path, file_name=delete_file)
    
    # Cleans up the files made throughout the tests
    def tearDown(self):
        try:
            self.provider.delete_file(self.test_path, file_name=self.test_file_name)
        except Exception as e:
            print(f"cleanup warning: failed to delete test file: {e}")

if __name__ == "__main__":
    unittest.main()