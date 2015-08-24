# -*- coding: utf-8 -*-
"""
Created on Fri Jul 24 13:25:29 2015
@author: dmilnes
Goals:  Very Simple Weekly Visits
"""

import os
import sys
import csv
from datetime import datetime
os.chdir('C:\\Users\\dmilnes\\Documents\\Personal\\Kaggle\\SFO2HND\\Data')

visits = 'coupon_visit_train.csv'
writefile = 'user_coupon_impressions_train.csv'

def get_weeks(filename):
    weeks = {}
    with open(filename) as f:
       content = csv.reader(f)
       content.next()
       for line in content:
           day = datetime.strptime(line[0],'%m/%d/%Y').date()
           week = datetime.strptime(line[1],'%m/%d/%Y').date()
           weeks.update({day:week})
    return weeks

def get_genres(filename):
    genres = {}    
    with open(filename) as f:
       content = csv.reader(f)
       content.next()
       for line in content:       
           genres.update({line[1]:line[2]})
    return genres
   
def gethead(filename):
    with open(filename) as myfile:
        rows = csv.reader(myfile)        
        head = next(rows)
    return head       
    
  
def user_coupon(filename):
    usercouponhash = {}  
    with open(filename) as f:
        content = csv.DictReader(f)
        for line in content:
            user = line['USER_ID_hash']
            coupon = line['VIEW_COUPON_ID_hash']
            if line['PURCHASEID_hash']=='': 
                purchase = 0 
            else: 
                purchase = 1
            couponentry = {coupon:[1,purchase]}            
            userentry = {user:couponentry}
            if not user in usercouponhash:
                usercouponhash.update(userentry)
            elif not coupon in usercouponhash[user]:
                usercouponhash[user].update(couponentry)
            else:
                usercouponhash[user][coupon][0]+=1
                usercouponhash[user][coupon][1]+=purchase
    return usercouponhash

def flatten_user_coupon(couponhash):
    flattened = []
    for user, userinfo in couponhash.items():
        for coupon, value in userinfo.items():
            flattened.append([user,coupon,value[0],value[1]])
    return flattened

header = gethead(visits)     
couponhash = user_coupon(visits)
flat = flatten_user_coupon(couponhash)

with open(writefile, 'wb') as csvout:
    writer = csv.writer(csvout)
    writer.writerow(['USER_ID_hash','COUPON_ID_hash','impressions','purchases'])
    writer.writerows(flat)
