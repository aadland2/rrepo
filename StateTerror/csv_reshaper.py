# -*- coding: utf-8 -*-
"""
Created on Fri May 20 19:19:38 2016

@author: aadlandma
"""
import csv 
path = "C:/Users/aadlandma/Desktop/migrationViz/static/csv"
rights = path + "/predictedRights3.csv"
with open(rights, 'rb') as f:
    reader = csv.reader(f)
    for row in reader:
        print row