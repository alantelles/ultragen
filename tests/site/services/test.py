import requests

url = "https://genius.p.rapidapi.com/search"

headers = {
    'x-rapidapi-key': "99a003d31cmshe41c63c82496b77p131580jsn4a6eb96ebc67",
    'x-rapidapi-host': "genius.p.rapidapi.com"
    }

response = requests.request("GET", url, params={'q': 'Dabeull'} ,headers=headers)

with open('dabeull.json', 'wt') as resp:
    resp.writelines(response.text)