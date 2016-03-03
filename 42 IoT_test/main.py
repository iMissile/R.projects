#!/usr/bin/python
# -*- coding: cp1251 -*-

from influxdb import InfluxDBClient
from influxdb import SeriesHelper



def main():
    # InfluxDB connections settings
    host = 'cloud.iot-playground.com'
    port = 8086
    #user = 'pkochnov'
    #password = 'Qwerty123'
    user = 'root'
    password = 'root'
    dbname = 'eiotclouddb'
    
    query = "select time, Value from hist where Id = 'hBuOYDpUnewPKMG6' and time > now() - 1d & db=eiotclouddb & epoch=ms &_=1456906918710"
    
    myclient = InfluxDBClient(host, port, user, password, dbname)
    
    print("Queying data: " + query)
    result = myclient.query(query)
    
if __name__ == '__main__':
    main()
