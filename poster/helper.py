#!/usr/bin/python

import httplib,sys

payload = "accountType=GOOGLE&Email=imapple1@gmail.com&Passwd=picopeta&service=cp&source=whydoyoucare" 
headers = {"Content-type":"application/x-www-form-urlencoded"}

con = httplib.HTTPSConnection("google.com")
con.request("POST","/accounts/ClientLogin",payload,headers)
response = con.getresponse()

print response.getheaders()

print response.read()
