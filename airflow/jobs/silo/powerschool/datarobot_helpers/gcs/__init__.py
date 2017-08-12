from datarobot_helpers.config import GCS_CONFIG
from oauth2client.service_account import ServiceAccountCredentials
from gcloud import storage
from retrying import retry

GCLOUD_KEYFILE = GCS_CONFIG['gcloud_keyfile']
GCLOUD_PROJECT_NAME = GCS_CONFIG['gcloud_project_name']
GCS_BUCKET_NAME = GCS_CONFIG['gcs_bucket_name']
GCLOUD_CREDENTIALS = ServiceAccountCredentials.from_json_keyfile_name(GCLOUD_KEYFILE)

@retry(wait_exponential_multiplier=1000, wait_exponential_max=64000, stop_max_attempt_number=10)
def upload_to_gcs(bucket_folder, generic_filename, filedir, filename, project_name=GCLOUD_PROJECT_NAME, credentials=GCLOUD_CREDENTIALS, bucket_name=GCS_BUCKET_NAME):
    """
    upload file to a Google Cloud Storage blob
        - bucket_folder
        - generic_name
        - filedir
        - filename
    """
    ## instantiate GCS client
    gcs_client = storage.Client(project_name, credentials)
    gcs_bucket = gcs_client.get_bucket(bucket_name)

    ## get GCS blob
    gcs_path = '{0}/{1}/{2}'.format(bucket_folder, generic_filename, filename)
    gcs_blob = gcs_bucket.blob(gcs_path)
    print('\tUploading to Google Cloud Storage... {}'.format(gcs_blob))

    ## upload file
    if filedir.endswith('/'):
        filedir = filedir[:-1]
    file_path = '{0}/{1}'.format(filedir, filename)
    gcs_blob.upload_from_filename(file_path)