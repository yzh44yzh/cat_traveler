#!/usr/bin/env python3

import requests
import json

base_url = 'http://localhost:8080/'
headers = {
    'Content-Type': 'application/json'
}


def enter(cat, town):
    data = json.dumps({
        'cat': cat,
        'town': town
    })
    print('> enter', data)
    res = requests.put(base_url + 'enter', headers=headers, data=data)
    print('<', res.status_code, res.content)


def leave(cat, town):
    data = json.dumps({
        'cat': cat,
        'town': town
    })
    print('> leave', data)
    res = requests.put(base_url + 'leave', headers=headers, data=data)
    print('<', res.status_code, res.content)


def dwell(cat, town):
    print('> dwell', cat, town)
    res = requests.get(base_url + 'dwell?cat=' + cat + '&town=' + town, headers=headers)
    print('<', res.status_code, res.content)


def where_is_cat(cat):
    print('> where_is_cat', cat)
    res = requests.get(base_url + 'where_is_cat?cat=' + cat, headers=headers)
    print('<', res.status_code, res.content)


def who_is_in_town(town):
    print('> who_is_in_town', town)
    res = requests.get(base_url + 'who_is_in_town?town=' + town, headers=headers)
    print('<', res.status_code, res.content)


def ping():
    print('> ping')
    res = requests.put(base_url + 'ping', headers=headers)
    print('<', res.status_code, res.content)


enter('Tihon', 'Minsk')
leave('Tihon', 'Minsk')
dwell('Tihon', 'Minsk')
where_is_cat('Tihon')
who_is_in_town('Minsk')
ping()

print('DONE')