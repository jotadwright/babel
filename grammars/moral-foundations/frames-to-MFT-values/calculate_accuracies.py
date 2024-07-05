import csv 
from argparse import ArgumentParser
from collections import Counter

parser = ArgumentParser()
parser.add_argument('--input_file', default="/Users/lara/Projects/babel/grammars/moral-foundations/frames-to-MFT-values/out/annotated_tweets_values_ewt_ontonotes_20_march_filtered_all.csv") 
args = parser.parse_args()
input_file = args.input_file

with open(input_file) as csvfile:
	lines = []
	content = csv.reader(csvfile) 

	total_non_strict_acc = 0
	total_weighted_acc = 0
	total_acc = 0

	total_relevant_retrieved_instances = 0
	total_all_retrieved_instances = 0
	total_all_relevant_instances = 0
	relevant_retrieved_instances = 0
	all_retrieved_instances = 0
	all_relevant_instances = 0

	intersection = 0
	union = 0

	length_smaller = 0
	total_length = 0

	all_values = ['authority', 'betrayal', 'care', 'cheating', 'degradation', 'fairness', 'harm', 'loyalty', 'non_moral' , 'purity' , 'subversion', 'oppression', 'liberty']
	
	# Start value dict that keeps track of which frames are mentioned with a certain value
	value_dict = {v: [] for v in all_values}

	for row_number, row in enumerate(content): 
		print("----------------------")

		# gold standard values
		values_values =  row[26:-2]

		# Get the predicted frames & values
		predicted_frames = row[-2]
		predicted_values = row[-1]

		# Calculate predictions of the gold values 
		# Table looks like: 	
		# [val1, val2, ..., valn]
		# [0, 1.6, ..., 0]
		# Gold values = every time a certain value has a value higher than 0
		# Highest gold values = values that have the highest number

		gold_values = []
		highest_gold_values = []
		highest = max(values_values)

		for i,val in enumerate(values_values): 
			if val != '0': 
				gold_values.append(all_values[i])
			if val == highest: 
				highest_gold_values.append(all_values[i])
		print("gold values")
		print(values_values)
		#print(highest_gold_values)

		# Clean the predicted frames & values, they are python lists as strings, so remove [], ' and spaces
		cleaned_predicted_frames = predicted_frames.replace("[", "")
		cleaned_predicted_frames = cleaned_predicted_frames.replace("]", "")
		cleaned_predicted_frames = cleaned_predicted_frames.replace(" ", "")
		cleaned_predicted_frames = cleaned_predicted_frames.replace("'", "")
		list_predicted_frames = cleaned_predicted_frames.split(',')

		cleaned_predicted_values = predicted_values.replace("[", "")
		cleaned_predicted_values = cleaned_predicted_values.replace("]", "")
		cleaned_predicted_values = cleaned_predicted_values.replace(" ", "")
		cleaned_predicted_values = cleaned_predicted_values.replace("'", "")
		list_predicted_values = cleaned_predicted_values.split(',')


		#print("Gold values: %s" %gold_values)

		# No frame is predicted if only '/' in list_predicted_values or if '' --> then there are no predicted values
		no_value_predicted = 1 if list_predicted_values.count('/') == len(list_predicted_values) or list_predicted_values.count('') == 1 else 0

		# Predict accuracies: 
		# Three types, non strict, weighted and normal
		# Non strict: one of the predicted values is in the list of the values that have a prediction higher than 0 (gold_values)
		# Normal: one of the predicted values is in the list of the values that have the highest prediction (highest_values)
		# Weighted: the predicted values that have highest weight (based on how many times a value is predicted in the sentence) are in the list of values that have the highest prediction 
		# e.g. 2x care and 1x harm -> care the highest) is in the list of highest gold values
		# Think about non-moral values -> TODO check this!!

		non_strict_acc = 0
		weighted_acc = 0
		acc = 0
		print("predicted values")
		print(list_predicted_values)
		cleaned = ['non_moral' if x=='/' else x for x in list_predicted_values] 
		intersec = set(gold_values).intersection(set(cleaned))
		print("intersection")
		print(intersec)
		un = set(list(set(cleaned)) + list(set(gold_values)))
		print("union")
		print(un)
		intersection = intersection + len(intersec)
		union = union + len(un)

		if len(set(cleaned)) <= len(set(gold_values)): 
			length_smaller += 1
		total_length += 1

		## Non strict accuracy
		for v in list_predicted_values: 
			if v in gold_values:
				print("We have success " + v)
				non_strict_acc = 1
				#relevant_retrieved_instances += 1
			elif no_value_predicted and 'non_moral' in gold_values: 
				non_strict_acc = 1
				#relevant_retrieved_instances += 1
			#all_retrieved_instances += 1


		## Non strict accuracy
		for v in list(set(list_predicted_values)): 
			if v in gold_values:
				print("We have success " + v)
				non_strict_acc = 1
				relevant_retrieved_instances += 1
			elif no_value_predicted and 'non_moral' in gold_values: 
				non_strict_acc = 1
				relevant_retrieved_instances += 1
			all_retrieved_instances += 1
		
		for v in gold_values: 
			all_relevant_instances += 1
		
		## Normal accuracy
		for v in list_predicted_values: 
			if v in highest_gold_values:
				#print("We have success " + v)
				acc = 1
			elif no_value_predicted and 'non_moral' in highest_gold_values: 
				acc = 1

		## Weighted accuracy
		# count values
		counted_values = Counter(list_predicted_values)
		print(counted_values)
		# sort counted value dict based on frequency
		{k: v for k, v in sorted(counted_values.items(), key=lambda item: item[1])} 
		#print(counted_values)

		print("highest weight:")
		if no_value_predicted: 
			highest_weight = ['no_value']
			print(highest_weight)
		else: 
			# make copy, remove '/' and '' and get the one with the highest value
			copy_counted_values = counted_values.copy()
			if '/' in copy_counted_values.keys(): 
				del copy_counted_values['/']
			if '' in copy_counted_values.keys(): 
				del copy_counted_values['']
			vals = list(copy_counted_values.values())
			vals.sort()
			highest_val = vals[0]
			highest_weight = [k for k, v in copy_counted_values.items() if v == highest_val]
			print(highest_weight)

		#print("This is the value with the highest weight! " + str(highest_weight))

		for v in highest_weight: 
			if v in highest_gold_values:
				print("We have success " + v)
				weighted_acc = 1
			elif no_value_predicted and 'non_moral' in highest_gold_values: 
				print("We have success because no value")
				weighted_acc = 1
		

		## Totals 
		total_non_strict_acc = total_non_strict_acc + non_strict_acc
		total_weighted_acc = total_weighted_acc + weighted_acc
		total_acc = total_acc + acc

		non_strict_acc = 0
		weighted_acc = 0
		acc = 0

		## Collect a frame dictionary
		for f,v in zip(list_predicted_frames, list_predicted_values): 
			if v != '/' and v != '': 
				value_dict[v].append(f)

	print("Total non strict: " + str(total_non_strict_acc))
	print("Total weighted: " + str(total_weighted_acc))
	print("Total strict: " + str(total_acc))
	
	print("Total: " + str(row_number))

	print(relevant_retrieved_instances)
	print(all_retrieved_instances)
	print(all_relevant_instances)

	print("Precision: " + str(relevant_retrieved_instances / all_retrieved_instances))
	precision = relevant_retrieved_instances / all_retrieved_instances
	print("Recall: " + str(relevant_retrieved_instances / all_relevant_instances))
	recall = relevant_retrieved_instances / all_relevant_instances
	print("F1: " + str(2*((precision*recall/(precision+recall)))))

	print(intersection/union)
	print(value_dict)

	print("length measure: ")
	print(length_smaller/total_length)
