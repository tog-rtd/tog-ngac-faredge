server="141.58.0.8"
execmanager_port="2777"
token=eyJ0eXAiOiJKV1QiLCJhbGciOiJIUzI1NiJ9.eyJzdWIiOiJyLmRlbG9uZ0BvcGVuZ3JvdXAub3JnIiwiaWF0IjoxNTU2ODI5MTQ1LCJleHAiOjE1NTk0MjExNDV9.o4tiBF1iIULT86aPiH9gs_WyD1ntg1gpB5Bs_I2fHPk
echo token is ${mytoken}
# #### register a new log
curl -H "Authorization: OAuth ${mytoken}" -H "Content-Type: text/plain"   -XPOST http://${server}:${execmanager_port}/new_log?code=111\&ip="10.11.12.13"\&message="Test log"\&user="jajab@abc.com"
