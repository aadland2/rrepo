# -*- coding: utf-8 -*-
"""
Created on Fri May 20 19:19:38 2016

@author: aadlandma
"""
import csv 
path = "C:/Users/aadlandma/Desktop/migrationViz/static/csv"
rights = path + "/predictedRights3.csv"
out_path = path + "/out.csv"
rows = []
with open(rights, 'rb') as f:
    reader = csv.reader(f)
    for row in reader:
        rows.append(row)


ids = [row[9] for row in rows]
years = [row[2] for row in rows]

ids_i = list(set(ids))
years_i = list(set(years)).sort()
ids_i.sort()
all_rows = []
for i in ids_i:
    print i
    row_e = []
    row_e.append(i)
    for row in rows:
        if row[9] == i:
            row_e.extend([row[3]])
    all_rows.append(row_e)



b = open(out_path, 'w')
a = csv.writer(b)
a.writerows(all_rows)
b.close()