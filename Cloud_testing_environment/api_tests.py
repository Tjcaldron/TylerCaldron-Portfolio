import unittest
from app import app
'''
This is the testing file for the API and its functions. It is designed to run both Azure and AWS 
to prevent having to change values around. 
'''

class APITestCase(unittest.TestCase):
    def setUp(self):
        # Sets the credential values to be used in the testing process.
        self.client = app.test_client()
        # Sets the providers to a small list so both tests can run.
        self.providers = ["aws-prod", "azure-dev"]
        self.test_path = "test-folder"
        self.test_file_name = "api_test.txt"
        self.test_content = "This API test content."

    # Calls the create_file API to create or overwrite a new file.
    def test_create_file(self):
        # Loops through the providers list to get both AWS and Azure values.
        for provider in self.providers:
            with self.subTest(provider=provider):
                response = self.client.post('/api/files/create-file', json={
            "connection_name": provider,
            "file_path": self.test_path,
            "file_name": self.test_file_name,
            "file_contents": self.test_content
            })
            # Checks that the API responded correctly. Can be commented out to improve run time.
            data = response.get_json()
            print(f"[{provider}] Create response: {data}")
            self.assertEqual(response.status_code, 200)
            self.assertTrue(data["successful"])

    # Calls the download_file API to make a copy of the specified file for testing.
    def test_download_file(self):
        # Loops through the providers list to get both AWS and Azure values.
        for provider in self.providers:
            # Sets the values for the file to copy.
            with self.subTest(provider=provider):
                response = self.client.post('/api/files/create-file', json={
            "connection_name": provider,
            "file_path": self.test_path,
            "file_name": self.test_file_name,
            "file_contents": self.test_content
            })
            # Gets the value of the copied file.
            response = self.client.get('/api/files/download-file', json={
                "connection_name": provider,
                "file_path": self.test_path,
                "file_name": self.test_file_name
            })
            # Checks that the API responded correctly. Can be commented out to improve run time.
            print(f"[{provider}] Download status: {response.status_code}")
            self.assertEqual(response.status_code, 200)
            self.assertEqual(response.data, self.test_content.encode())

    def test_delete_file(self):
        # Loops through the providers list to get both AWS and Azure values.
        for provider in self.providers:
            # Gets the values for the selected file to delete later on.
            with self.subTest(provider=provider):
                response = self.client.post('/api/files/create-file', json={
            "connection_name": provider,
            "file_path": self.test_path,
            "file_name": self.test_file_name,
            "file_contents": self.test_content
            })
            # Deletes the file previously selected.
            response = self.client.delete('/api/files/delete-file', json={
                "connection_name": provider,
                "file_path": self.test_path,
                "file_name": self.test_file_name
            })
            # Checks that the API responded correctly. Can be commented out to improve run time.
            data = response.get_json()
            print(f"[{provider}] Delete response {data}")
            self.assertEqual(response.status_code, 200)
            self.assertTrue(data["successful"])

if __name__ == '__main__':
    unittest.main()