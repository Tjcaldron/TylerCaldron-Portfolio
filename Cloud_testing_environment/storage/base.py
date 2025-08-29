from abc import ABC, abstractmethod
'''
Base.py serves as a blueprint/interface for the project, it dictates what has to exist to run.
'''

class StorageProvider(ABC):
    @abstractmethod
    # Creates a file for the specified cloud provider.
    def create_file(self, path: str, file_name: str, content: bytes)-> bool:
        # Returns True if the file was created correctly and False otherwise.
        raise NotImplementedError

    @abstractmethod
    # Downloads a file from the specified cloud provider
    def download_file(self, path: str, file_name: str)-> bytes:
        # Returns the file contents as bytes so they can be compared against 
        # the original file.
        raise NotImplementedError

    @abstractmethod
    # Deletes a specified file from the cloud provider
    def delete_file(self, path: str, file_name: str)-> bool:
        # Returns True if the file was deleted successfully and False otherwise/
        raise NotImplementedError