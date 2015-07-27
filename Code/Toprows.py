# -*- coding: utf-8 -*-
"""
Created on Wed Jul 22 22:21:59 2015

@author: dmilnes
"""
import os
import sys
import csv
os.chdir('C:\\Users\\dmilnes\\Documents\\Personal\\Kaggle\\SFO2HND\\Data')

filename = 'coupon_visit_train.csv'

def sample(number):
    samps = []
    with open(filename) as f:
        content = csv.reader(f)
        content.next()
        for x in range(number):
            samps.append(content.next())
    return samps

def gethead():
    with open(filename) as myfile:
        rows = csv.reader(myfile)        
        head = next(rows)
    return head

samplesize = sample(5000)
header = gethead()

with open('coupon_visit_sample.csv', 'wb') as fp:
    a = csv.writer(fp)
    a.writerow(header)    
    a.writerows(samplesize)
    

