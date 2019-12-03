#!/bin/bash
	server="141.58.0.8";
	execmanager_port="2777";
	mytoken="eyJ0eXAiOiJKV1QiLCJhbGciOiJIUzI1NiJ9.eyJzdWIiOiJyLmRlbG9uZ0BvcGVuZ3JvdXAub3JnIiwiaWF0IjoxNTU2ODMwMTY3LCJleHAiOjE1NTk0MjIxNjd9.BAshfdS2fdvqN-gkr2FP0PoTVPvML614UtORAYf6A5Y";
	echo "token is ${mytoken}";

curl -v -H "Authorization: OAuth ${mytoken}" -H "Content-Type: text/plain"   -XPOST http://httpbin.org/post?code=111\&ip="1.1.1.1"\&message="Testagain"\&user="rjd@tog.org";
