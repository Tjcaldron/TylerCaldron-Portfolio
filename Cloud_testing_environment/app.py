from flask import Flask, request, jsonify, send_file
from credentials import get_storage_provider
from io import BytesIO

# Initializes the Flask app
app = Flask(__name__)

'''
This file sets up the main Flask aspplication to handle all of the file operations
across multiple cloud providers using one unified API.

Routes:
    POST   /api/files/create-file   - Creates a file in the selected cloud storage
    GET    /api/files/download-file - Downloads a file from cloud storage
    DELETE /api/files/delete-file   - Deletes a file from cloud storage
'''

@app.route('/api/files/create-file', methods=['POST'])
# Endpoint to create/overwrite a file in cloud storage.
def create_file():
    data = request.json
    if data is None:
        return jsonify(successful=False, error_message="Missing or invalid JSON body."), 400
    try:
        provider = get_storage_provider(data['connection_name'])
        success = provider.create_file(data["file_path"], data["file_name"], data["file_contents"].encode('utf-8'))
        # Returns whether the upload was successful or not.
        return jsonify(successful=success, error_message="" if success else "Upload failed.")
    except Exception as e:
        return jsonify(successful=False, error_message=str(e))

@app.route('/api/files/download-file', methods=['GET'])
# Endpoint to download a file from cloud storage.
def download_file():
    data = request.json
    if data is None:
        return jsonify(successful=False, error_message="Missing or invalid JSON body."), 400
    try: 
        provider = get_storage_provider(data['connection_name'])
        content = provider.download_file(data["file_path"], data["file_name"])
        # Returns whether the copy was successful or not.
        return send_file(BytesIO(content), mimetype='application/oclet-stream', download_name=data["file_name"])
    except Exception as e:
        return jsonify(successful=False, error_message=str(e))

@app.route('/api/files/delete-file', methods=['DELETE'])
# Endpoint to delete a file in cloud storage.
def delete_file():
    data = request.json
    if data is None:
        return jsonify(successful=False, error_message="Missing or invalid JSON body."), 400
    try: 
        provider = get_storage_provider(data['connection_name'])
        success = provider.delete_file(data["file_path"], data["file_name"])
        # Returns whether the delete was successful or not.
        return jsonify(successful=success, error_message="" if success else "Delete failed.")
    except Exception as e:
        return jsonify(successful=False, error_message=str(e))

# Starts the application for local development.
if __name__ == "__main__":
    app.run(debug=True)