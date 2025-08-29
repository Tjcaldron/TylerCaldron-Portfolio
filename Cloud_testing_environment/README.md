# Cloudmersive Software engineering exercise

By: Tyler Caldron
A Python-based web API that abstracts the cloud storage operations of Azure blob storage and AWS S3 using a unified interface.
It supports the creation, download, and deletion of files via API endpoints.

## Features

- Upload, download, and delete files using cloud storage providers.
- Support for Azure blobs and AWS S3 Buckets.
- Easy to manipulate to add additional providers.
- Includes testing for the API and additionally for the providers as well.

## Tech Stack

- Python 3.11+
- Flask
- Azure SDK (`azure-storage-blob`, `azure-identity`)
- Boto3 (AWS S3 SDK)
- unittest (Python built-in testing framework)


