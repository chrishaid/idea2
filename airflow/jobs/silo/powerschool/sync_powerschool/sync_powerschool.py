#!/user/bin/python3.5

from powerschool_config import CONFIG
from datarobot_helpers import email, gcs
import argparse
import os
from datetime import datetime
from dateutil.relativedelta import relativedelta
import requests
import re
import json
import math
import zipfile
import sys

CREDENTIALS_ENCODED = CONFIG['credentials_encoded']
AUTH_HEADERS = CONFIG['auth_headers']
BASE_URL = CONFIG['base_url']
MAXPAGESIZE = CONFIG['maxpagesize']
SAVE_PATH = CONFIG['save_path']

def try_print(stmnt):
    """
    try print and if it failes rais an exception
    """
    try:
        print(stmnt)
    except ValueError:
        print("Could not convert data to an integer in print statement.")

def get_table_count(table_name, query, base_url=BASE_URL, headers=AUTH_HEADERS, maxpagesize=MAXPAGESIZE):
    """
    get row and page count from endpoint
        - table_name
        - query
        - headers
        - maxpagesize
    """
    print("Running get_table_count() . . . ")
    r_count = requests.get('{0}/ws/schema/table/{1}/count?{2}'.format(base_url, table_name, query), headers=headers)
    r_status = r_count.status_code
    if r_status != 200:
         try_print('Response NOT successful. I got code {} '.format(r_status))
         raise ValueError('Response NOT successful. I got code {} '.format(r_status))
    else:
         try_print('Response  successful! I got code {} '.format(r_status))
    count_json = r_count.json()
    row_count = count_json['count']


    pages = math.ceil(row_count / maxpagesize)

    return row_count, pages

def find_previous_partitions(parameter, stopping_criteria, decrement, identifier, table_name, table_columns):
    """
    find all valid historic partitions on PS using count endpoint
    """
    print("Running find_previous_partitions() . . . ")
    historic_queries = []
    while parameter >= stopping_criteria:
        historic_query = {}
        parameter_new = parameter - decrement
        if parameter < 0:
            probing_query = '{0}=gt={1};{0}=le={2}'.format(identifier, parameter_new, parameter)
        else:
            probing_query = '{0}=ge={1};{0}=lt={2}'.format(identifier, parameter_new, parameter)
        probing_query_formatted = 'q={}&'.format(probing_query)

        row_count, pages = get_table_count(table_name, probing_query_formatted)
        if row_count > 0:
            historic_query = {
                    'table_name': table_name,
                    'query_expression': probing_query,
                    'projection': table_columns
                }
            historic_queries.append(historic_query)
        parameter = parameter_new

    return historic_queries

def get_table_data(table_name, query, pages, table_columns, base_url=BASE_URL, maxpagesize=MAXPAGESIZE, headers=AUTH_HEADERS):
    """
    get data at specified endpoint
        - table_name
        - query
        - pages
        - table_columns
        - headers
        - base_url
        - maxpagesize
    """
    print("Running get_table_data() . . . ")
    table_data = []
    for p in range(pages):
        page_number = p + 1
        # print('\tGetting page number... {}'.format(page_number), end='\r', flush=True)
        endpoint = '{0}/ws/schema/table/{1}?{2}page={3}&pagesize={4}&projection={5}'.format(base_url, table_name, query, page_number, maxpagesize, table_columns)
        r_data = requests.get(endpoint, headers=headers)

        if r_data.ok:
            data_json = r_data.json()
            records = data_json['record']
            for r in records:
                table_data.append(r['tables'][table_name])
        else:
            print(r_data.text)
            raise Exception(r_data.text)

    return table_data

def save_file(save_dir, filename, data):
    """
    check if save folder exists (create if not) and save data to specified filepath
        - filepath
        - data
    """
    print('\tSaving to... {}'.format(save_dir))
    if not os.path.isdir(save_dir):
        os.mkdir(save_dir)

    filepath = '{0}/{1}'.format(save_dir, filename)
    with open(filepath, 'w+') as outfile:
        json.dump(data, outfile)

    zipfilepath = filepath.replace('.json','.zip')
    with zipfile.ZipFile(zipfilepath, 'w', zipfile.ZIP_DEFLATED) as zf:
        zf.write(filepath)

    os.remove(filepath)

def main():
    print("Running main() . . . ")
    if not os.path.isdir(SAVE_PATH):
        os.mkdir(SAVE_PATH)

    ## parse endpoint JSON file
    print("Parse endpoints . . . ")
    parser = argparse.ArgumentParser()
    parser.add_argument('-e', '--endpoints', help='Endpoints file', required=False)
    args = parser.parse_args()
    if args.endpoints:
        with open(args.endpoints) as file:
            endpoints_json = json.load(file)
    else:
        exception_text = '\n'.join(('Missing argument: --endpoints /path/to/endpoints.json\n',
                                    'Return all populated fields from all records in the table, which satisfy the specified criteria',
                                    'ex: {"endpoints":[{"table_name":"","projection":"","query_expression":""},...]}:',
                                    '- table_name\tTable may be any extended schema table extension (1-1 or 1-many), independent table, or a supported core table',
                                    '- projection\tA comma-delimited list indicating the desired columns to be included in the API call',
                                    '- query_expression (optional)\trefer to https://support.powerschool.com/developer/#/page/searching -- will partition data into separate files\n'))
        raise ValueError(exception_text)

    ## for each endpoint...
    print("Processing endpoints . . . ")
    ENDPOINTS = endpoints_json['endpoints']
    print("Loop through endpoints . . . ")
    for i, e in enumerate(ENDPOINTS):
        table_start = datetime.now()

        ## parse variables
        table_name = e['table_name']
        table_columns = e['projection']

        save_dir = '{0}{1}'.format(SAVE_PATH, table_name)

        filename = '{0}.json'.format(table_name)
        query = ''

        ## if there's a query included...
        if 'query_expression' in e.keys():
            ## format query expression and rename file to include query string
            query = 'q={}&'.format(e['query_expression'])
            query_filename = ''.join(e for e in query if (e.isalnum() or e == '-'))
            filename = '{0}_{1}.json'.format(table_name, query_filename)

            ## check if there's already a directory of historical data, and if not...
            if not os.path.isdir(save_dir):
                ## create the directory
                os.mkdir(save_dir)

                ## extract identifiers, operators, and parameters from query (assumes only one identifier used)
                print('{}:  No data found on disk, searching for historical data to backfill...'.format(table_name))
                pattern = r'([\w_]*)(==|=gt=|=ge=|=lt=|=le=)([\d]{4}-[\d]{2}-[\d]{2}|[\d]+);?'
                match = re.search(pattern, query)
                identifier, operator, parameter = match.group(1), match.group(2), match.group(3)

                ## build list of queries that return valid historical data
                ## TODO: there's got to be a better way to do this
                if identifier == 'termid':
                    parameter = int(parameter)
                    stopping_criteria = (parameter * -1)    ## termids can be negative
                    decrement = 100
                if identifier == 'yearid':
                    parameter = int(parameter)
                    stopping_criteria = 0
                    decrement = 1
                if identifier == 'assignmentcategoryassocid':
                    parameter = int(parameter)
                    stopping_criteria = 0
                    decrement = 100000
                if 'date' in identifier:
                    parameter = datetime.strptime(parameter, '%Y-%m-%d').date()
                    stopping_criteria = datetime.strptime('2000-07-01', '%Y-%m-%d').date()
                    decrement = relativedelta(years=1)
                historic_queries = find_previous_partitions(parameter, stopping_criteria, decrement, identifier, table_name, table_columns)

                ## extend ENPOINTS list to include `historic_queries`
                ENDPOINTS[i+1:i+1] = historic_queries

        ## get a row count and number of pages
        print("Getting row count. . . ")
        row_count, pages = get_table_count(table_name, query)
        print('GET {0}:  {1} rows, {2} pages {3}'.format(table_name, row_count, pages, query))

        ## download data
        print("download data. . . ")
        table_data = get_table_data(table_name, query, pages, table_columns)

        ## save data as JSON file
        print("Saving data. . . ")
        save_file(save_dir, filename, table_data)
        filename = filename.replace('.json','.zip')

        ## push JSON file to GCS
        print("Pushing to GCS. . . ")
        gcs.upload_to_gcs('powerschool', table_name, save_dir, filename)

        table_end = datetime.now()
        table_elapsed = table_end - table_start
        print('\t{0} sync complete!\tElapsed time = {1}'.format(table_name, str(table_elapsed)))

if __name__ == '__main__':
    try:
        main()
    except Exception as e:
        print('PowerSchool sync error', e)
        raise
