from SPARQLWrapper import SPARQLWrapper, JSON 
from argparse import ArgumentParser
from tqdm import tqdm

import json
import requests
import csv
import time
import os

start = time.time()

parser = ArgumentParser()
#parser.add_argument('--input_file', default="/Users/lara/Projects/datasets/MFT_twitter/MFT_twitter_corpus_sample.csv")          
parser.add_argument('--input_file', default="/Users/lara/Projects/datasets/MFT_twitter/MFTC_tweets.csv")          
parser.add_argument('--output_file', default="/Users/lara/Projects/babel/grammars/moral-foundations/frames-to-MFT-values/out/annotated_tweets_values_ewt_ontonotes_30_april.csv")
parser.add_argument('--output_file_json', default="/Users/lara/Projects/babel/grammars/moral-foundations/frames-to-MFT-values/out/annotated_tweets_values_ewt_ontonotes_30_april.json") 

args = parser.parse_args()
input_file = args.input_file
output_file = args.output_file
output_file_json = args.output_file_json


def get_frames(utterance): 
    pf_list = []
    roles_list = []

    headers = {
        'Content-Type': 'application/json',
    }

    json_data = {
        'utterance': utterance,
        'package': 'propbank-grammar',
        'grammar': '*restored-grammar*',
        'timeout': 100,
    }

    response = requests.post('http://127.0.0.1:2560/extract-frames', headers=headers, json=json_data)

    response_json = response.json() 
    statusCode = response_json['statusCode']
    if statusCode == 200: 
        propbank_frames = response_json['frameSet']
        if propbank_frames: 
            for propbank_frame in propbank_frames: 
                pf = propbank_frame['frameName']
                pf_list.append(pf)
                roles = propbank_frame['roles']
                roles_list.append(roles)
    else: 
        pf_list = []
        roles_list = []

    return pf_list, roles_list

def get_value(frame): 
    results = []

    query = """
    PREFIX vcvf: <http://www.ontologydesignpatterns.org/ont/values/valuecore_with_value_frames.owl#> 
    PREFIX haidt: <https://w3id.org/spice/SON/HaidtValues#> 
    PREFIX pbdata: <https://w3id.org/framester/pb/pbdata/> 

    SELECT ?o
    WHERE
     {{ pbdata:{f} vcvf:triggers ?o . ?o ?ref haidt:MFT_Value .

    }}
    LIMIT 10
    """.format(f=frame)

    sparql = SPARQLWrapper("https://w3id.org/framester/sparql")
    sparql.setReturnFormat(JSON)

    sparql.setQuery(query
    )

    ret = sparql.queryAndConvert()

    for r in ret["results"]["bindings"]: 
        results.append(r["o"]["value"].split("#")[1].lower())
    result = results[0] if results else "/"
    return result

for file_index in range(16800,18200,200): 
    with open(input_file) as csvfile:
        large_dict = {}
        large_dict['data'] = []
        lines = []
        content = csv.reader(csvfile) 
        next(content, None)
    
        for i,row in enumerate(content): 
            if i in range(file_index, file_index + 200): 
                #print(row)
                json_dict = {}
                created_at = row[4]
                author_id = row[5]
                text = row[6]
                cleaned_text = text.replace("-", "")
                id = row[0]
                corpus = row[25]
                if cleaned_text: 
                    #print(cleaned_text)
                    frames_and_roles = get_frames(cleaned_text)
                    frames = frames_and_roles[0]
                    roles = frames_and_roles[1]
                else: 
                    frames = ['']
                    roles = ['']
                values = []

                all_values = ['authority', 'betrayal', 'care', 'cheating', 'degradation', 'fairness', 'harm', 'loyalty', 'non_moral' , 'purity' , 'subversion']

                # values start at 
                highest_value = row[26:].index(max(row[26:]))

                gold_standard = all_values[highest_value]
                json_dict["id"] = id
                json_dict["frames"] = frames
                json_dict["roles"] = roles
                
                for frame,role in zip(frames,roles): 
                    value = get_value(frame)
                    values.append(value)
                    lines.append([id, frame, role, value])

                json_dict["values"] = values

                success = 1 if gold_standard in [item for row in values for item in row] else 0

                #lines.append(row + [frames, values, roles])

                print(i)
                #print(json_dict)
                if large_dict['data']: 
                    large_dict['data'].append(json_dict)
                else:
                    large_dict['data'] = [json_dict]

    file_name = os.path.basename(output_file_json)
    #print(file_name)
    splitted_file_name = os.path.splitext(file_name)
    new_file_name = splitted_file_name[0] + '_' + str(file_index) + splitted_file_name[1]
    file_path = os.path.dirname(output_file_json)
    new_output_path = os.path.join(file_path, new_file_name)
    #print(new_output_path)

    with open(new_output_path, 'w+') as jsonfile_out: 
        json.dump(large_dict, jsonfile_out)

    end = time.time()
    print("Time elapsed: " + str(end - start))
