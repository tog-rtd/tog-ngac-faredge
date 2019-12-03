#!/bin/bash
	server="141.58.0.8";     <-- for other partners tools running out of the server
	execmanager_port="2777"; <-- for other partners tools running out of the server
# 	server="localhost";      #<-- Rance, the sec server runs on the same node, then you must use this.
# 	execmanager_port="8000"; #<-- Rance, the sec server runs on the same node, then you must use this.
#I mean, partners access to the server at the hlrs server at the address 141.58.0.8:2777 but internally on the same machine is localhost:8000
#then the NGAC server running in the server at HLRS see the Repository server at localhost:8000

#If you run the NGAC on your laptop, and the Repository-server is runing at HRLS, then you has to use the address 141.58.0.8:2777

#  ################## GET A NEW TOKEN FOR A REGISTERED USER ###################################
	echo "obtaining token ....";
	read -p $'Press [Enter] key to get an authentication \033[1;37mNEW TOKEN\033[1;34m for the example user';
	echo "token is ${mytoken}";
	mytoken=`curl -s -H "Content-Type: text/plain" -XGET http://${server}:${execmanager_port}/login?email="r.delong@opengroup.org"\&pw="0220057633"`;
	echo "token is ${mytoken}";

# #### register a new log
curl -H "Authorization: OAuth ${mytoken}" -H "Content-Type: text/plain"   -XPOST http://${server}:${execmanager_port}/new_log?code=111\&ip="10.11.12.13"\&message="Aa aa"\&user="jaja@abc.com";
