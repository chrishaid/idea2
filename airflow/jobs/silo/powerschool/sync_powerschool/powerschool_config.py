import base64
import os
import json
import requests
import configparser
from datetime import datetime


"""
    - save_path /path/to/data/files
    - creds_file /path/to/sync_psconfig.py file with PS credentials (UID and PWD)
"""
save_path = 'data/'
creds_file = '/config/sync_psconfig.py'

"""
PowerSchool config variables
    - base_url          full URL of your PS instance 'https://...'
    - maxpagesize       configured on your plugin screen, cannot be greater
    - client_id         found on plugin screen (!!! KEEP PRIVATE !!!)
    - client_secret     found on plugin screen (!!! KEEP PRIVATE !!!)
    - state_filepath    /path/to/state.json (will be created if it doesn't exist initially)
"""
BASE_URL = 'https://kippchicago.powerschool.com'
maxpagesize = 1000

config = configparser.ConfigParser()
config.read(creds_file)
client_id = config.get('sync_ps', 'PS_CLIENT_ID')
client_secret = config.get('sync_ps', 'PS_CLIENT_PWD')

state_filepath = 'state.json'
credentials_concat = '{0}:{1}'.format(client_id, client_secret)
CREDENTIALS_ENCODED = base64.b64encode(credentials_concat.encode('utf-8'))

"""
Access Token generation and validation
    - get_access_token()
    - build_auth_headers()
"""
def get_access_token(base_url=BASE_URL, credentials_encoded=CREDENTIALS_ENCODED):
    """
    retrieve a new access_token from PowerSchool
        - base_url                  full URL of PS instance (default from CONFIG)
        - credentials_encoded       API credentials (default from CONFIG)
    """
    print('Retrieving new access token')
    access_token_timestamp = datetime.now()
    access_headers = {
            'Authorization': b'Basic ' + credentials_encoded,
            'Content-Type': 'application/x-www-form-urlencoded;charset=UTF-8'
        }
    access_payload = {'grant_type':'client_credentials'}
    r_access = requests.post('{0}/oauth/access_token/'.format(base_url), headers=access_headers, params=access_payload)

    access_json = r_access.json()
    access_json['timestamp'] = str(access_token_timestamp)
    return access_json

def build_auth_headers(state, credentials_encoded):
    """
    check if `access_token` is still valid (obtain a new one if not) then build auth_headers for further API calls and return
        - state                     dict containing saved-state info
        - credentials_encoded       API credentials (default from CONFIG)
    """
    ## parse access_token variables
    access_token_saved = state['access_token']
    access_token_timestamp = datetime.strptime(state['timestamp'], '%Y-%m-%d %H:%M:%S.%f') ## TODO: make this better
    access_token_expires = int(state['expires_in'])

    current_timestamp = datetime.now()
    timestamp_diff = current_timestamp - access_token_timestamp
    if timestamp_diff.total_seconds() > access_token_expires:
        print('Access token has expired, refreshing...')
        access_json = get_access_token()
        access_token_new = access_json['access_token']

        with open(state_filepath, 'w') as file:
            json.dump(access_json, file)

        access_token = access_token_new
    else:
        print('Access token still valid\n')
        access_token = access_token_saved

    auth_headers = {'Authorization': 'Bearer {0}'.format(access_token),}
    return auth_headers

## check if saved-state file exists and load
## if not, get new access_token and save response to `state.json`
if os.path.isfile(state_filepath):
    with open(state_filepath) as file:
        state = json.load(file)
else:
    state = get_access_token()
    with open(state_filepath, 'w+') as file:
        json.dump(state, file)

## build authorization headers, and check access_token for validity
auth_headers = build_auth_headers(state, CREDENTIALS_ENCODED)

"""
all together now
"""
CONFIG = {
        'credentials_encoded': CREDENTIALS_ENCODED,
        'auth_headers': auth_headers,
        'base_url': BASE_URL,
        'maxpagesize': maxpagesize,
        'save_path': save_path
    }
