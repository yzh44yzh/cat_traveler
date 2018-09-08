#!/usr/bin/env python3

import requests
import json

base_url = 'http://localhost:8080'
headers = {
    'Content-Type': 'application/json'
}


def enter(cat, town):
    data = {
        'cat': cat,
        'town': town
    }
    print_query('enter', data)
    print_res(requests.put(base_url + '/enter', headers=headers, data=json.dumps(data)))


def leave(cat, town):
    data = {
        'cat': cat,
        'town': town
    }
    print_query('leave', data)
    print_res(requests.put(base_url + '/leave', headers=headers, data=json.dumps(data)))


def dwell(cat, town):
    data = {
        'cat': cat,
        'town': town
    }
    print_query('dwell', data)
    print_res(requests.get(base_url + '/dwell?cat=' + cat + '&town=' + town, headers=headers))


def where_is_cat(cat):
    data = {
        'cat': cat
    }
    print_query('where_is_cat', data)
    print_res(requests.get(base_url + '/where_is_cat?cat=' + cat, headers=headers))


def who_is_in_town(town):
    data = {
        'town': town
    }
    print_query('who_is_in_town', data)
    print_res(requests.get(base_url + '/who_is_in_town?town=' + town, headers=headers))


def ping():
    print_query('ping', None)
    print_res(requests.get(base_url + '/ping', headers=headers))


def print_query(query, data):
    print('>', query)
    if data:
        print(json.dumps(data, indent=2))


def print_res(res):
    j = json.loads(res.content)
    print('<', res.status_code)
    print(json.dumps(j, indent=2), '\n')


ping()
enter('Tihon', 'Minsk')
leave('Tihon', 'Minsk')
# dwell('Tihon', 'Minsk')
# where_is_cat('Tihon')
# who_is_in_town('Minsk')

print('DONE')