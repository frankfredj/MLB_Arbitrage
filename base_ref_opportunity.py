#Pipeline pour aller chercher les donnees afin de placer un pari


from selenium.webdriver import Chrome
from selenium import webdriver 
from selenium.webdriver.chrome.options import Options
from selenium.webdriver.common.keys import Keys
from selenium.webdriver.common.desired_capabilities import DesiredCapabilities

import numpy as np
import pandas as pd

import re
from dateutil.parser import parse
from datetime import datetime
from datetime import timedelta

from selenium.webdriver.common.keys import Keys
from tqdm import tqdm

import requests
import urllib

import os.path
from os import path

import scipy.stats as stats
import time



chrome_options = Options()
chrome_options.add_argument("--headless")

driver = webdriver.Chrome(r"C:\bin\chromedriver_win32\chromedriver.exe", options=chrome_options)
driver.create_options()



def opportunities(driver):

	dictio = pd.read_csv("D:/MLB/abreviations_dictionary.csv")

	dates = datetime.now()

	y = str(dates.year)
	m = str(dates.month)
	if len(m) == 1:
		m = "0" + m
	d = str(dates.day)
	if len(d) == 1:
		d = "0" + d

	date = y + "-" + m + "-" + d

	print("Extracting betting opportunities for: " + date + "... ")

	#Offered returns:

	url = "https://www.sportsbookreview.com/betting-odds/mlb-baseball/money-line/?date=" + str(date.replace("-","")) 
	driver.get(url)

	team_boxes = driver.find_elements_by_class_name("_3qi53")
	time.sleep(1)
	odds_boxes = driver.find_elements_by_class_name("_1QEDd")

	teams = []
	odds = []


	n_teams = int(len(team_boxes) / 2)
	n_cols = int(len(odds_boxes) / n_teams)


	for box in team_boxes:

		teams.append(str(box.get_attribute("text").split("-")[0]).replace(u"\xa0", ""))

	keep = []	

	for i in range(0, len(teams)):

		a = np.where(dictio["mlb_abr"] == teams[i])[0]

		if len(a) != 0:
			keep.append(i)
			teams[i] = str(dictio["City"][a[0]])		

		else:
		
			continue


	teams = np.array(teams)[keep]
	keep = []	

	money_lines = []
	ml_per_team = []

	for box in odds_boxes:

		money_lines.append(str(box.get_attribute("innerHTML")))

	cut = []
	for i in range(0,len(money_lines)):
		if "opener" in money_lines[i]:
			cut.append(i)

	cut = np.array(cut)

	temp = np.where(np.diff(cut) > 1)[0]

	frm = cut[temp] + 1
	to = cut[temp + 1]

	frm = list(frm) 
	frm.append(cut[-1] + 1)

	to = list(to)
	to.append(len(money_lines))

	for i in range(0,len(money_lines)):
		money_lines[i] = money_lines[i].split(">")[1].split("<")[0]	


	r = list()

	for i in range(0, len(frm)):

		temp = money_lines[frm[i]:to[i]]

		pos = [x for x in temp if not "-" in x]
		neg = [x for x in temp if "-" in x and x != "-"]

		if len(pos) == 0 or len(neg) == 0:
			continue

		pos = np.array([float(x) for x in pos])
		neg = np.array([float(x) for x in neg])

		pos = (pos + 100) / 100
		neg = (100 - neg) / -neg

		k = 0 
		while(temp[k] == "-" and k < len(temp)):
			k += 1


		if k == len(temp) - 1:
			continue

		if "-" in temp[k]:

			r.append([neg.max(), pos.max()])

		else:

			r.append([pos.max(), neg.max()])


		keep.append(2*i)
		keep.append(2*i+1)


	keep = list(set(keep))
	keep = [x for x in keep if x < len(teams)]

	teams = teams[keep]

	team_pairs = []

	for i in range(0, int(len(teams) / 2)):
		team_pairs.append([teams[2*i + 1], teams[2*i]])


	r = pd.DataFrame(r, columns = ["R1", "R2"])
	t = pd.DataFrame(team_pairs, columns = ["Team_1", "Team_2"])

	returns = pd.concat([r,t], axis = 1)
	returns["Date"] = date

	returns.to_csv("D:/MLB/offered_returns_" + date + ".csv", index = False)

	print("Offered returns for " + date + ":")
	print(returns)


	#lineups

	print("Extracting lineups...")

	def translate(word, to):

		k = np.where(dictio == word)[0][0]

		return dictio[to][k]


	url = "https://www.rotowire.com/baseball/daily-lineups.php"
	driver.get(url)

	boxes = driver.find_elements_by_class_name("lineup__box")

	keep = []
	for i in range(0, len(boxes)):

		tms = boxes[i].find_elements_by_class_name("lineup__abbr")

		if len(tms) == 0:
			continue

		a = tms[0].get_attribute("innerHTML")
		a = translate(a, "City")

		b = tms[1].get_attribute("innerHTML")
		b = translate(b,"City")

		if a in teams and b in teams:

			keep.append(i)

	

	def process_box(box):

		p = box.find_elements_by_class_name("lineup__player-highlight-name")

		pitchers = []

		for x in p:

			pitchers.append(str(x.get_attribute("innerHTML")).split("</a>\n")[0].split(">")[-1])

		position_away = []
		position_home = []

		names_away = []
		names_home = []

		home_team = []
		away_team = []


		lineups_bat = box.find_elements_by_class_name("lineup__list")

		th = translate(box.find_elements_by_class_name("lineup__abbr")[1].text, "City")
		ta = translate(box.find_elements_by_class_name("lineup__abbr")[0].text, "City")

		away = lineups_bat[0]
		home = lineups_bat[1]

		for x in home.find_elements_by_class_name("lineup__player"):

			position_home.append(x.find_element_by_class_name("lineup__pos").text)
			names_home.append(str(x.get_attribute("innerHTML")).split("title=")[1].split(" href")[0].replace('"', ''))
			home_team.append(th)

		position_home.append("P")
		names_home.append(pitchers[1])
		home_team.append(th)

		for x in away.find_elements_by_class_name("lineup__player"):

			position_away.append(x.find_element_by_class_name("lineup__pos").text)
			names_away.append(str(x.get_attribute("innerHTML")).split("title=")[1].split(" href")[0].replace('"', ''))
			away_team.append(ta)

		position_away.append("P")
		names_away.append(pitchers[0])
		away_team.append(ta)


		position = position_home + position_away
		names = names_home + names_away
		team = home_team + away_team

		return pd.DataFrame({"Positions" : position, "Name" : names, "Team" : team})

	l_up = []

	for i in tqdm(keep):

		if i == keep[0]:

			l_up = process_box(boxes[i])

		else:

			l_up = l_up.append(process_box(boxes[i]))


	l_up.to_csv("D:/MLB/lineups_" + date + ".csv", index = False)

	print("Files saved.")

