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
writefile = 'weekly_visit_train.csv'
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
    
    
def user_visits_week(filename):
    uservisithash = {}
    usersessionhash = {}
    with open(filename) as f:
        content = csv.DictReader(f)
        for line in content:
            try:
                dayt = datetime.strptime(line['I_DATE'],'%m/%d/%Y %H:%M')
            except ValueError:
                dayt = datetime.strptime(line['I_DATE'],'%Y-%m-%d %H:%M:%S')
            session = line['SESSION_ID_hash']
            day = dayt.date()
            week = datedict[day]
            user = line['USER_ID_hash']
            purch = int(line['PURCHASE_FLG'])
            userinfo = {week:[1,0]}
            if not user in uservisithash:
                usersessionhash.update({user:[session]})
                uservisithash.update({user:userinfo})
            elif not session in usersessionhash[user]:
                usersessionhash[user].append(session)
                if not week in uservisithash[user]:
                    uservisithash[user].update(userinfo)
                else:
                    uservisithash[user][week][0]+=1
            elif not week in uservisithash[user]:
                uservisithash[user].update(userinfo)
            uservisithash[user][week][1]+=purch
    return uservisithash

def flatten_user_weeks(user_visits):
    flattened = []
    for user, userinfo in user_visits.items():
        for week, weekinfo in userinfo.items():
            sessions, purchases = weekinfo
            flattened.append([user,week,sessions, purchases])
    return flattened
       
datedict = get_weeks(weekfile)
visitsweeks = user_visits_week(visits)
flat = flatten_user_weeks(visitsweeks)

with open(writefile, 'wb') as csvout:
    writer = csv.writer(csvout)
    writer.writerow(['user_hash','week','visits','purchases'])
    writer.writerows(flat)
