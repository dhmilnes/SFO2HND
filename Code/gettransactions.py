# -*- coding: utf-8 -*-
"""
Created on Wed Jul 22 15:35:51 2015

@author: dmilnes
"""
import os
import sys
import csv
from datetime import datetime
os.chdir('C:\\Users\\dmilnes\\Documents\\Personal\\Kaggle\\SFO2HND\\Data')

filename = 'coupon_visit_sample.csv'

def matchval(colno,value):
    purchases = []
    with open(filename) as f:
        content = csv.DictReader(f)
        for line in content:
            if line["PURCHASE_FLG"]==value:
                purchases.append(line)
    return purchases

def gethead():
    with open(filename) as myfile:
        rows = csv.reader(myfile)        
        head = next(rows)
    return head

def purchased_coupon_views_in_session(transactions): 
    session_stats = []    
    for trans in transactions:
        stats = {'tcp':0,'purchcp':0, 'uniqcp':0, 
                 'SESSION_ID_hash':trans['SESSION_ID_hash']}
        couponhashes = {}
        #count the number of views in the file
        with open(filename) as f:
            content = csv.DictReader(f)
            for line in content:
                line['I_DATE']
                if (line['SESSION_ID_hash']==trans['SESSION_ID_hash'] and
                    line['I_DATE']<trans['I_DATE']):    
                        stats['tcp']+=1
                        if line['VIEW_COUPON_ID_hash']==trans['VIEW_COUPON_ID_hash']:
                            stats['purchcp']+=1
                        if not line['VIEW_COUPON_ID_hash'] in couponhashes:
                            stats['uniqcp']+=1
                            couponhashes.update({line['VIEW_COUPON_ID_hash']:0})
                        couponhashes[line['VIEW_COUPON_ID_hash']]+=1
        session_stats.append(stats)                                     
    return session_stats
    
    
def doclen():
    with open(filename) as myfile:
        rows = csv.reader(myfile)    
        doclen = 0
        for line in rows:
            doclen += 1
    return doclen-1
 
purchases = matchval(0,'1') #look in col zero for the purchase flag
header = gethead()
#sstats = purchased_coupon_views_in_session(purchases)





""" 
Goals here are to parse internet logs for data

1) For coupons purchased in a session how many coupons purchased?
2) For coupons purchased how many prior views 
    
"""
    