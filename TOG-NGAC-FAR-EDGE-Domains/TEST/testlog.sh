mytoken=eyJ0eXAiOiJKV1QiLCJhbGciOiJIUzI1NiJ9.eyJzdWIiOiJyLmRlbG9uZ0BvcGVuZ3JvdXAub3JnIiwiaWF0IjoxNTU2NTgyOTYyLCJleHAiOjE1NTkxNzQ5NjJ9.tAQmP5791FP3jSdlYIoUvWbG48VLxpVGBoQq5GbPHvI
curl -H "Authorization: OAuth ${mytoken}" -H "Content-Type: text/plain" -XPOST http://141.58.0.8:2777/new_log?code=200\&ip="10.11.12.13"\&message="Test NGAC log"\&user="r.delong@opengroup.org"
