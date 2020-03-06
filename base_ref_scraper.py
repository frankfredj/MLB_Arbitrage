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




def parse_tbl(table):

	date_index = []

	for i in range(0,np.shape(table)[0]):
		if len(str(table.iloc[i,0]).split(" ")) > 1:
			date_index.append(i)

	for i in range(0, (len(date_index)-1)):
		temp = str(table.iloc[date_index[i],0]).split(" ")
		temp = [x for x in temp if x != "" and x != " "]
		date = temp[0]
		del temp[0]
		
		for j in range(0, 2):
			date = date + " " + temp[j]

		date = datetime.strptime(date, "%d %b %Y")
		index = list(range(date_index[i], date_index[i+1]))
		table.iloc[index,0] = date

	i = date_index[-1]

	temp = str(table.iloc[i,0]).split(" ")
	temp = [x for x in temp if x != "" and x != " "]
	date = temp[0]
	del temp[0]

	for j in range(0, 2):
		date = date + " " + temp[j]
	if "." in date:
		date = date.split(".")[0]

	date = datetime.strptime(date, "%d %b %Y")
	index = list(range(i, np.shape(table)[0]))
	table.iloc[index,0] = date

	table = table.drop(date_index)

	rmv = np.where(table.isnull().any(axis=1))[0]
	if len(rmv) > 0:
		table = table.drop(table.index[rmv])

	date = table.columns[1][1].split(" ")
	date = [x for x in date if x != "" and x != " "]

	temp = date[0]
	for i in range(1,3):
	    temp = temp + " " + date[i]
	if "." in temp:
		temp = temp.split(".")[0]

	n = 0
	while str(type(table.iloc[n,0])) != "<class 'pandas._libs.tslibs.timestamps.Timestamp'>":
		table.iloc[n,0] = datetime.strptime(temp, "%d %b %Y")
		n += 1

	table = table.iloc[:,[0,1,2,4,5,6]]
	table.columns = ["Date", "Team_1", "Team_2", "R1", "R2", "N"]

	rmv = []

	for i in range(0,np.shape(table)[0]):

		try:

			temp = table.iloc[i,1].split("-")

			for j in range(0,2):
				temp_2 = [x for x in temp[j].split(" ") if x != "" and x != " "]
				date = temp_2[0]

				for k in range(1,len(temp_2)):
					date = date + " " + temp_2[k]

				temp[j] = date

			for j in range(0,2):
					table.iloc[i,j+1] = temp[j]	

		except:
			rmv.append(i)
			continue

	if len(rmv) > 0:
		rmv = table.index[rmv]
		table.drop(rmv)

	rmv = []
	for i in range(0,np.shape(table)[0]):
		for j in range(1,3):

			m = np.where(dictio["Full"] == table.iloc[i,j])[0]

			if len(m) > 0:
				k = m[0]
				table.iloc[i,j] = dictio["City"][k]
			else:
				rmv.append(i)
	if len(rmv) > 0:
		rmv = list(set(rmv))
		rmv = table.index[rmv]
		table = table.drop(rmv)

	return table
	




def scrape_scores(frm, to, driver, check):

	colnames = ["Date", "Team_1", "Score_1", "Team_2", "Score_2", "Link"]
	dictio = pd.read_csv("D:/MLB/abreviations_dictionary.csv") 

	dont_scrape = []

	#check = path.exists("D:/MLB/scores.csv")

	if check:

		dont_do = list(set(list(pd.read_csv("D:/MLB/scores.csv")["Date"])))

		if path.exists("D:/MLB/dont_do.csv"):
			add = list(set(list(pd.read_csv("D:/MLB/dont_do.csv")["Date"])))
			dont_do = list(set(list(dont_do + add)))

		for i in range(0,len(dont_do)):
			dont_do[i] = datetime.strptime(dont_do[i], "%Y-%m-%d").date()

	errors = []

	while(frm <= to):

		if check:
			if frm in dont_do:
				frm = frm + timedelta(days=1) 
				continue


		try:
			rows = []

			d = str(frm.day)
			m = str(frm.month)
			y = str(frm.year)

			url = "https://www.baseball-reference.com/boxes/?month=" + m + "&day=" + d + "&year=" + y 
			
			try:
				driver.get(url)
			except:
				if len(d) == 1:
					d = str(0) + d
				if len(m) == 1:
					m = str(0) + m
				url = "https://www.baseball-reference.com/boxes/?date=" + y + "-" + m + "-" + d
				driver.get(url)

			date = str(frm)

			try:

				boxes = driver.find_elements_by_class_name("teams")

				if len(boxes) == 0:

					dont_scrape.append(date)

					frm = frm + timedelta(days=1) 
					continue	

			except:				

				dont_scrape.append(date)

				frm = frm + timedelta(days=1) 
				continue

			for box in tqdm(boxes):

				try:

					text = box.text.split("\n")
					if box.text.split("\n")[0].split(" ")[0] == "Game":
						del text[0]
					if "League" in text[0] or "League" in text[1]:
						continue
					for i in range(0,len(text)):
						if "Final" in text[i]:
							text[i] = text[i].split("Final")[0]
						if "(" in text[i]:
							text[i] = text[i].split("(")[0]

					row = []
					row.append(date)

					for i in range(0, len(text)):
						temp = [x for x in text[i].split(" ") if (x != " " and x != "")]

						score = int(temp[-1])
						team = temp[0]
						for j in range(1,(len(temp)-1)):
							team = team + " " + temp[j]

						team = dictio["City"][np.where(dictio["Full"] == team)[0][0]]

						row.append(team)
						row.append(score)

					link = str(box.find_element_by_class_name("gamelink").find_element_by_xpath('.//a').get_attribute('href'))
					row.append(link)
					row = np.array(row)

					home = link.split("boxes/")[-1].split("/")[0]

					if row[1] != home:
						row = row[[0,3,4,1,2,5]]

					rows.append(row)

				except:

					errors.append(frm)
					print("Error scrapping: " + str(frm) + ".")
					continue
					#bug = box
					#break


			if not path.exists("D:/MLB/scores.csv"):
				pd.DataFrame(rows, columns = colnames).to_csv("D:/MLB/scores.csv", index = False)
			else:
				pd.read_csv("D:/MLB/scores.csv").append(pd.DataFrame(rows, columns = colnames)).to_csv("D:/MLB/scores.csv", index = False)

			print("Scrapped: " + str(frm) + " succesfully.")
			frm = frm + timedelta(days=1)

		except:

			frm = frm + timedelta(days=1)
			print("No matches found for: " + str(frm) + ".")

	if not path.exists("D:/MLB/dont_scrape.csv"):
		pd.DataFrame(dont_scrape, columns = ["Date"]).to_csv("D:/MLB/dont_scrape.csv", index = False)
	else:
		pd.read_csv("D:/MLB/dont_scrape.csv").append(pd.DataFrame(dont_scrape, columns = ["Date"])).to_csv("D:/MLB/dont_scrape.csv", index = False)

	if not path.exists("D:/MLB/errors.csv"):
		pd.DataFrame(errors, columns = ["Date"]).to_csv("D:/MLB/errors.csv", index = False)
	else:
		pd.read_csv("D:/MLB/errors.csv").append(pd.DataFrame(errors, columns = ["Date"])).to_csv("D:/MLB/errors.csv", index = False)


	pd.read_csv("D:/MLB/scores.csv").drop_duplicates().to_csv("D:/MLB/scores.csv", index = False)
	#pd.read_csv("D:/MLB/bat.csv").drop_duplicates().to_csv("D:/MLB/bat.csv", index = False)
	#pd.read_csv("D:/MLB/pitch.csv").drop_duplicates().to_csv("D:/MLB/pitch.csv", index = False)
	pd.read_csv("D:/MLB/errors.csv").drop_duplicates().to_csv("D:/MLB/errors.csv", index = False)
	pd.read_csv("D:/MLB/dont_scrape.csv").drop_duplicates().to_csv("D:/MLB/dont_scrape.csv", index = False)








def sync_player_stats_and_scores(driver):

	temp = list(set(list(pd.read_csv("D:/MLB/scores.csv")["Date"])))
	temp = np.array(temp)

	frm = np.min(temp)
	to = np.max(temp)

	get_player_stats(frm, to, driver, check = True)






def get_player_stats(frm, to, driver, check):

	scores = pd.read_csv("D:/MLB/scores.csv")
	scores = scores.drop_duplicates()
	scores.to_csv("D:/MLB/scores.csv", index = False)

	if check:

		dont_do = list(set(list(pd.read_csv("D:/MLB/bat.csv")["Date"])))
		dont_do = dont_do + list(set(list(pd.read_csv("D:/MLB/pitch.csv")["Date"])))
		dont_do = list(set(dont_do))

		if path.exists("D:/MLB/dont_do.csv"):
			add = list(set(list(pd.read_csv("D:/MLB/dont_do.csv")["Date"])))
			dont_do = list(set(list(dont_do + add)))

		#for i in range(0,len(dont_do)):
			#dont_do[i] = datetime.strptime(dont_do[i], "%Y-%m-%d").date()	

		rmv = []
		for i in range(0, len(scores)):
			if scores.at[i, "Date"] in dont_do:
				rmv.append(i)

		if len(rmv) > 0:
			rmv = scores.index[rmv]
			scores = scores.drop(rmv)

	keep = []

	for i in range(0,np.shape(scores)[0]):

		date = datetime.strptime(scores.iloc[i,0], "%Y-%m-%d").date()

		if frm <= date <= to:
			keep.append(i)

	if len(keep) > 0:
		scores = scores.iloc[keep,:]
	else:
		print("Invalid date range.")
		
	if len(keep) > 0:

		to_do = list(scores["Link"])
		error = []

		#error_bat = []
		#error_pitch = []
		#error = []

		bat_total = []
		pitch_total = []
		for i in tqdm(range(len(to_do))):

			url = str(scores.iloc[i,5])

			try:

				driver.get(url)
				boxes = driver.find_elements_by_class_name("table_outer_container")

				try:
					bat_1 = pd.read_html(boxes[0].get_attribute("outerHTML"))[1]
				except:
					bat_1 = pd.read_html(boxes[0].get_attribute("outerHTML"))[0]
				try:
					bat_2 = pd.read_html(boxes[1].get_attribute("outerHTML"))[1]
				except:
					bat_2 = pd.read_html(boxes[1].get_attribute("outerHTML"))[0]

				bat_1["Team"] = scores.iloc[i,3]
				bat_2["Team"] = scores.iloc[i,1]

				bat = bat_1.append(bat_2)
				bat["Date"] = scores.iloc[i,0]

				#clean data

				rmv = np.where(bat["Batting"].isnull())[0]
				if len(rmv) > 0:
					bat = bat.drop(bat.index[rmv])

				rmv = np.where(bat["Batting"] == "Team Totals")[0]
				bat = bat.drop(bat.index[rmv])

				position = []
				j = np.where(bat.columns == "Batting")[0][0]
				for k in range(0,np.shape(bat)[0]):
					temp = bat.iloc[k,j].split(" ")
					temp = [x for x in temp if x != "" and x != " "]

					position.append(temp[-1])
					del temp[-1]

					nm = temp[0]
					for m in range(1,len(temp)):
						nm = nm + " " + temp[m]

					bat.iloc[k,j] = nm

				bat["Position"] = position

				bat = bat.fillna(0)


				try:
					pitch_1 = pd.read_html(boxes[2].get_attribute("outerHTML"))[1]
				except:
					pitch_1 = pd.read_html(boxes[2].get_attribute("outerHTML"))[0]
				try:
					pitch_2 = pd.read_html(boxes[3].get_attribute("outerHTML"))[1]
				except:
					pitch_2 = pd.read_html(boxes[3].get_attribute("outerHTML"))[0]

				pitch_1["Team"] = scores.iloc[i,3]
				pitch_2["Team"] = scores.iloc[i,1]

				pitch = pitch_1.append(pitch_2)
				pitch["Date"] = scores.iloc[i,0]

				rmv = np.where(pitch["Pitching"].isnull())[0]
				if len(rmv) > 0:
					pitch = pitch.drop(pitch.index[rmv])

				rmv = np.where(pitch["Pitching"] == "Team Totals")[0]
				pitch = pitch.drop(pitch.index[rmv])

				details = []
				j = np.where(pitch.columns == "Pitching")[0][0]
				for k in range(0,np.shape(pitch)[0]):
					temp = pitch.iloc[k,j].split(",")
					if len(temp) == 1:
						details.append(0)
						continue
					else:
						temp = [x for x in temp if x != "" and x != " "]
						pitch.iloc[k,j] = temp[0]
						details.append(temp[-1].replace(" ", ""))

				pitch["Details"] = details
				pitch = pitch.fillna(0)

				#save

				if len(bat_total) == 0:
					bat_total = bat
				else:
					bat_total = bat_total.append(bat, sort = True)


				if len(pitch_total) == 0:
					pitch_total = pitch
				else:
					pitch_total = pitch_total.append(pitch, sort = True)			


				if i % 50 == 49 or i == (len(to_do) - 1):

					if len(bat_total) > 0:

						if not path.exists("D:/MLB/bat.csv"):
							bat_total.to_csv("D:/MLB/bat.csv", index = False)
						else:
							pd.read_csv("D:/MLB/bat.csv").append(bat_total).to_csv("D:/MLB/bat.csv", index = False)

						print("Saved bat.csv.")

					bat_total = []

					if len(pitch_total) > 0:
				
						if not path.exists("D:/MLB/pitch.csv"):
							pitch_total.to_csv("D:/MLB/pitch.csv", index = False)
						else:
							pd.read_csv("D:/MLB/pitch.csv").append(pitch_total).to_csv("D:/MLB/pitch.csv", index = False)

						print("Saved pitch.csv")

					pitch_total = []


			except:
				error.append(to_do[i])
				continue


	if not path.exists("D:/MLB/error.csv"):
		pd.DataFrame(error, columns = ["Date"]).to_csv("D:/MLB/error.csv", index = False)
	else:
		pd.read_csv("D:/MLB/error.csv").append(pd.DataFrame(error, columns = ["Date"])).to_csv("D:/MLB/error.csv", index = False)

	pd.read_csv("D:/MLB/bat.csv").drop_duplicates().to_csv("D:/MLB/bat.csv", index = False)
	pd.read_csv("D:/MLB/pitch.csv").drop_duplicates().to_csv("D:/MLB/pitch.csv", index = False)
	pd.read_csv("D:/MLB/error.csv").drop_duplicates().to_csv("D:/MLB/errors.csv", index = False)














def scrape_wagers(driver):


	scores = pd.read_csv("D:/MLB/scores_reg.csv")
	dictio = pd.read_csv("D:/MLB/abreviations_dictionary.csv")

	dates = list(set(list(scores["Date"])))


	#Code si le fichier existe

	if path.exists("D:/MLB/scores_BT.csv"):

		have = list(pd.read_csv("D:/MLB/scores_BT.csv")["Date"])

		dates = [x for x in dates if x not in have]


	#Code si le fichier existe


	scores["P_1"] = 0.0
	scores["P_2"] = 0.0	

	scores["R_1"] = 0.0
	scores["R_2"] = 0.0

	scores["R_1_best"] = 0.0
	scores["R_2_best"] = 0.0	

	scores["R_1_sd"] = 0.0
	scores["R_2_sd"] = 0.0	


	for date in tqdm(dates):

		#time.sleep(5)

		#Reach the webpage

		url = "https://www.sportsbookreview.com/betting-odds/mlb-baseball/money-line/?date=" + str(date.replace("-","")) 
		driver.get(url)

		#scrape raw data

		team_boxes = driver.find_elements_by_class_name("_3qi53")
		time.sleep(1)
		odds_boxes = driver.find_elements_by_class_name("_1QEDd")

		if len(team_boxes) == 0:
			print("Skipping " + str(date) +" ...")
			continue

		if len(odds_boxes) == 0:
			print("Skipping " + str(date) +" ...")
			continue

		teams = []

		best = []
		mu = []
		sd = []

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
				p1 = neg
				p2 = pos

			else:
				p2 = neg
				p1 = pos

			mu.append(p1.mean())
			mu.append(p2.mean())

			if len(p1) > 1:
				sd.append(p1.std())
			else:
				sd.append(0)

			if len(p2) > 1:
				sd.append(p2.std())
			else:
				sd.append(0)


			c = 1 / p1.mean() + 1 / p2.mean()

			odds.append((1/c)*(1/p1.mean()))
			odds.append((1/c)*(1/p2.mean()))

			best.append(p1.max())
			best.append(p2.max())		

			keep.append(2*i)
			keep.append(2*i+1)



		keep = list(set(keep))
		keep = [x for x in keep if x < len(teams)]

		teams = teams[keep]



		if len(teams) != len(odds):
			continue

		else:

			#Fill scores frame

			index = np.where(scores["Date"] == date)[0]
			temp = scores.iloc[index,:]


			for i in range(0, len(temp)):

				i_1 = np.where(teams == temp.at[index[i],"Team_1"])[0]

				if len(i_1) == 0:
					continue

				else:

					i_1 = i_1[0]

					if i_1 % 2 == 1:

						i_2 = i_1 - 1

					else:

						i_2 = i_1 + 1

					scores.at[index[i], "P_1"] = odds[i_1]
					scores.at[index[i], "P_2"] = odds[i_2]	

					scores.at[index[i], "R_1_best"] = best[i_1]
					scores.at[index[i], "R_2_best"] = best[i_2]	

					scores.at[index[i], "R_1"] = mu[i_1]
					scores.at[index[i], "R_2"] = mu[i_2]	

					scores.at[index[i], "R_1_sd"] = sd[i_1]
					scores.at[index[i], "R_2_sd"] = sd[i_2]	


	keep = np.where(scores["P_1"] != 0)[0]
	scores = scores.iloc[keep,:]

	print("Saving scraped data...")

	if path.exists("D:/MLB/scores_BT.csv"):

		pd.read_csv("D:/MLB/scores_BT.csv").append(scores).drop_duplicates().to_csv("D:/MLB/scores_BT.csv", index = False)

	else:

		scores.to_csv("D:/MLB/scores_BT.csv", index = False)
	







def fix_player_stats(frm, to, driver, check):

	scores = pd.read_csv("D:/MLB/to_scrape.csv")
	scores = scores.drop_duplicates()

	to_do = list(scores["Link"])
	error = []

	#error_bat = []
	#error_pitch = []
	#error = []

	bat_total = []
	pitch_total = []
	for i in tqdm(range(len(to_do))):

		url = str(scores.iloc[i,5])

		try:

			driver.get(url)
			boxes = driver.find_elements_by_class_name("table_outer_container")

			try:
				bat_1 = pd.read_html(boxes[0].get_attribute("outerHTML"))[1]
			except:
				bat_1 = pd.read_html(boxes[0].get_attribute("outerHTML"))[0]
			try:
				bat_2 = pd.read_html(boxes[1].get_attribute("outerHTML"))[1]
			except:
				bat_2 = pd.read_html(boxes[1].get_attribute("outerHTML"))[0]

			bat_1["Team"] = scores.iloc[i,3]
			bat_2["Team"] = scores.iloc[i,1]

			bat = bat_1.append(bat_2)
			bat["Date"] = scores.at[i,"Date"]

			#clean data

			rmv = np.where(bat["Batting"].isnull())[0]
			if len(rmv) > 0:
				bat = bat.drop(bat.index[rmv])

			rmv = np.where(bat["Batting"] == "Team Totals")[0]
			bat = bat.drop(bat.index[rmv])

			position = []
			j = np.where(bat.columns == "Batting")[0][0]
			for k in range(0,np.shape(bat)[0]):
				temp = bat.iloc[k,j].split(" ")
				temp = [x for x in temp if x != "" and x != " "]

				position.append(temp[-1])
				del temp[-1]

				nm = temp[0]
				for m in range(1,len(temp)):
					nm = nm + " " + temp[m]

				bat.iloc[k,j] = nm

			bat["Position"] = position

			bat = bat.fillna(0)


			try:
				pitch_1 = pd.read_html(boxes[2].get_attribute("outerHTML"))[1]
			except:
				pitch_1 = pd.read_html(boxes[2].get_attribute("outerHTML"))[0]
			try:
				pitch_2 = pd.read_html(boxes[3].get_attribute("outerHTML"))[1]
			except:
				pitch_2 = pd.read_html(boxes[3].get_attribute("outerHTML"))[0]

			pitch_1["Team"] = scores.iloc[i,3]
			pitch_2["Team"] = scores.iloc[i,1]

			pitch = pitch_1.append(pitch_2)
			pitch["Date"] = scores.at[i,"Date"]

			rmv = np.where(pitch["Pitching"].isnull())[0]
			if len(rmv) > 0:
				pitch = pitch.drop(pitch.index[rmv])

			rmv = np.where(pitch["Pitching"] == "Team Totals")[0]
			pitch = pitch.drop(pitch.index[rmv])

			details = []
			j = np.where(pitch.columns == "Pitching")[0][0]
			for k in range(0,np.shape(pitch)[0]):
				temp = pitch.iloc[k,j].split(",")
				if len(temp) == 1:
					details.append(0)
					continue
				else:
					temp = [x for x in temp if x != "" and x != " "]
					pitch.iloc[k,j] = temp[0]
					details.append(temp[-1].replace(" ", ""))

			pitch["Details"] = details
			pitch = pitch.fillna(0)

			#save

			if len(bat_total) == 0:
				bat_total = bat
			else:
				bat_total = bat_total.append(bat, sort = True)


			if len(pitch_total) == 0:
				pitch_total = pitch
			else:
				pitch_total = pitch_total.append(pitch, sort = True)			


			if i % 50 == 49 or i == (len(to_do) - 1):

				if len(bat_total) > 0:

					if not path.exists("D:/MLB/bat.csv"):
						bat_total.to_csv("D:/MLB/bat.csv", index = False)
					else:
						pd.read_csv("D:/MLB/bat.csv").append(bat_total).to_csv("D:/MLB/bat.csv", index = False)

					print("Saved bat.csv.")

				bat_total = []

				if len(pitch_total) > 0:
			
					if not path.exists("D:/MLB/pitch.csv"):
						pitch_total.to_csv("D:/MLB/pitch.csv", index = False)
					else:
						pd.read_csv("D:/MLB/pitch.csv").append(pitch_total).to_csv("D:/MLB/pitch.csv", index = False)

					print("Saved pitch.csv")

				pitch_total = []


		except:
			error.append(to_do[i])
			continue


	if not path.exists("D:/MLB/error.csv"):
		pd.DataFrame(error, columns = ["Date"]).to_csv("D:/MLB/error.csv", index = False)
	else:
		pd.read_csv("D:/MLB/error.csv").append(pd.DataFrame(error, columns = ["Date"])).to_csv("D:/MLB/error.csv", index = False)

	pd.read_csv("D:/MLB/bat.csv").drop_duplicates().to_csv("D:/MLB/bat.csv", index = False)
	pd.read_csv("D:/MLB/pitch.csv").drop_duplicates().to_csv("D:/MLB/pitch.csv", index = False)
	pd.read_csv("D:/MLB/error.csv").drop_duplicates().to_csv("D:/MLB/errors.csv", index = False)












