import numpy as np
import pandas as pd
from datetime import datetime
from tqdm import tqdm


def clean_data():

	bat = pd.read_csv("D:/MLB/bat_w_ID.csv").drop_duplicates()
	pitch = pd.read_csv("D:/MLB/pitch_w_ID.csv").drop_duplicates()
	scores = pd.read_csv("D:/MLB/scores_w_ID.csv").drop_duplicates()
	

	for i in tqdm(range(0, len(pitch))):

		if "-" not in pitch.at[i, "Date"]:

			temp = datetime.fromtimestamp(int(pitch.at[i,"Date"]))

			y = str(temp.year)
			m = str(temp.month)
			d = str(temp.day)

			if len(m) == 1:
				m = "0" + m
			if len(d) == 1:
				d = "0" + d

			pitch.at[i, "Date"] = y + "-" + m + "-" + d


	#rmv = np.where(bat["ID"] == 0)[0]
	#if len(rmv) > 0:
		#bat = bat.drop(bat.index[rmv])
		#bat = bat.reset_index(drop=True)


	#rmv = np.where(pitch["ID"] == 0)[0]
	#if len(rmv) > 0:
		#pitch = pitch.drop(pitch.index[rmv])
		#pitch = pitch.reset_index(drop=True)


	scores.to_csv("D:/MLB/scores_reg.csv", index = False)





	print("Processing bat and pitch tables...")

	rmv = np.where(bat["Position"].isnull())[0]
	if len(rmv) > 0:
		bat = bat.drop(bat.index[rmv])


	positions = [x for x in list(set(list(bat["Position"]))) if len(x.split("-")) == 1]

	actual_pos = ["1B", "2B", "SS", "3B", "C", "RF", "CF", "LF"]
	extras = [x for x in positions if x not in actual_pos]


	bat = pd.concat([bat, pd.DataFrame(columns = positions)])
	bat[positions] = 0

	bat = bat.reset_index(drop=True)



	for i in range(0,len(bat)):
		temp = [x for x in bat.at[i, "Position"].split("-") if x != "" and x != " "]
		temp = list(set(list(temp)))

		for j in temp:
			bat.at[i, j] = 1

	del bat["Position"]


	rmv = np.where(bat["P"] == 1)[0]
	if len(rmv) > 0:
		bat = bat.drop(bat.index[rmv])
		bat = bat.reset_index(drop=True)

	del bat["P"]

	details = list(set(list(bat["Details"])))
	det_all = []
	for elem in details:
		temp = elem.split(",")
		for x in temp:
			if "·" in x:
				x = x.split("·")[1]
			det_all.append(x)
	details = list(set(list(det_all)))

	for i in range(0, len(details)):
		details[i] = details[i] + "_det"

	bat = pd.concat([bat, pd.DataFrame(columns = details)])
	bat[details] = 0

	for i in range(0,len(bat)):
		temp = [x for x in bat.at[i, "Details"].split(",") if x != "" and x != " "]
		
		for j in temp:
			k = 1
			if "·" in j:
				a = j.split("·")
				k = int(a[0])
				j = a[1]
			bat.at[i, j + "_det"] = 1

	del bat["Details"]



	details = list(set(list(pitch["Details"])))
	det_all = []
	for elem in details:
		det_all.append(elem.split("(")[0])
	details = list(set(list(det_all)))
	details = [x for x in details if x != "0"]

	lengths = []
	temp = list(set(list(pitch["Details"])))

	for i in details:
		temp_2 = [x for x in temp if i in x]
		lengths.append(len(temp_2[0].split("-")))

	lengths = np.array(lengths)
	temp = np.where(lengths == 2)[0]
	details_2 = []
	for i in temp:
		details_2.append(details[i] + "2")


	pitch = pd.concat([pitch, pd.DataFrame(columns = details)])
	pitch[details] = 0

	pitch = pd.concat([pitch, pd.DataFrame(columns = details_2)])
	pitch[details_2] = 0

	for i in range(0,len(pitch)):
		temp = pitch.at[i, "Details"].split("(")
		cat = temp[0]
		if cat == "0":
			continue

		else:
			cat2 = temp[0] + "2"

			x = temp[1].split("-")
			if len(x) == 1:
				pitch.at[i,cat] = str(x[0].split(")")[0])
			else:
				x[1] = x[1].split(")")[0]
				pitch.at[i,cat] = int(x[0])
				pitch.at[i,cat2] = int(x[1])


	del pitch["Details"]


	#Give proper names

	bat_p = np.array(list(bat["Batting"]))
	bat_d = np.array(list(bat["Date"]))
	bat_t = np.array(list(bat["Team"]))

	temp = []
	for i in range(0, len(bat_p)):
		temp.append(bat_p[i] + bat_t[i])

	bat["Batting"] = temp


	pitch_p = np.array(list(pitch["Pitching"]))
	pitch_d = np.array(list(pitch["Date"]))
	pitch_t = np.array(list(pitch["Team"]))

	temp = []
	for i in range(0, len(pitch_p)):
		temp.append(pitch_p[i] + pitch_t[i])

	pitch["Pitching"] = temp

	#rmv = np.where(bat["ID"] == 0)[0]
	#if len(rmv) > 0:
		#bat = bat.drop(bat.index[rmv])

	#rmv = np.where(pitch["ID"] == 0)[0]
	#if len(rmv) > 0:
		#pitch = pitch.drop(pitch.index[rmv])		


	bat.drop_duplicates().to_csv("D:/MLB/bat_clean.csv", index = False)
	pitch.drop_duplicates().to_csv("D:/MLB/pitch_clean.csv", index = False)


















