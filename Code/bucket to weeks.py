# -*- coding: utf-8 -*-
"""
Created on Fri Jul 24 13:25:29 2015

@author: dmilnes

Goals:  Break everything into a week
"""

import os
import sys
import csv
from datetime import datetime
os.chdir('C:\\Users\\dmilnes\\Documents\\Personal\\Kaggle\\SFO2HND\\Data')

visits = 'coupon_visit_train.csv'
writefile = 'weekly_userdata_train.csv'
weekfile = 'Weekdates.csv'


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

       
def gethead(filename):
    with open(filename) as myfile:
        rows = csv.reader(myfile)        
        head = next(rows)
    return head       
    
    
def userweekbuckets(filename):
    userhash = {}
    with open(filename) as f:
        content = csv.DictReader(f)
        for line in content:
            try:
                dayt = datetime.strptime(line['I_DATE'],'%m/%d/%Y %H:%M')
            except ValueError:
                dayt = datetime.strptime(line['I_DATE'],'%Y-%m-%d %H:%M:%S')
            
            day = dayt.date()
            week = datedict[day]
            user = line['USER_ID_hash']
            purch = int(line['PURCHASE_FLG'])
            coupon = line['VIEW_COUPON_ID_hash']            
            ## week, coupon, views, purchases         
            weekinfo = {coupon:[1,purch]}
            userinfo = {week: weekinfo}
            if not user in userhash:
                userhash.update({user:userinfo})
            else: 
                weekhash = userhash[user]             
                if not week in weekhash:
                    weekhash.update({week:weekinfo})
                elif coupon not in weekhash[week]:
                    weekhash[week].update(weekinfo)
                else:
                    weekhash[week][coupon][0] += 1
                    weekhash[week][coupon][1] += purch
                userhash[user] = weekhash    
    return userhash

def flatten_user_weeks(userweek):
    flattened = []
    for user, userinfo in userweek.items():
        for week, weekinfo in userinfo.items():
            for coupon, couponinfo in weekinfo.items():
                views, purchs = couponinfo
                flattened.append([user,week,coupon,views,purchs])
    return flattened


       
datedict = get_weeks(weekfile)
userweek = userweekbuckets(visits)
flat = flatten_user_weeks(userweek)

with open(writefile, 'wb') as csvout:
    writer = csv.writer(csvout)
    writer.writerow(['user_hash','week','coupon_hash','views','purchases'])
    writer.writerows(flat)
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    

