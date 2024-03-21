from SPARQLWrapper import SPARQLWrapper, JSON 
from argparse import ArgumentParser
from tqdm import tqdm

import requests
import csv
import time

start = time.time()

parser = ArgumentParser()
#parser.add_argument('--input_file', default="/Users/lara/Projects/datasets/MFT_twitter/MFT_twitter_corpus_sample.csv")          
parser.add_argument('--input_file', default="/Users/lara/Projects/datasets/MFT_twitter/filtered_MFTC_tweets.csv")          
parser.add_argument('--output_file', default="/Users/lara/Projects/propbank/out/annotated_tweets_values_ewt_ontonotes_18_march_filtered.csv") 

args = parser.parse_args()
input_file = args.input_file
output_file = args.output_file

def get_frames(utterance): 
    pf_list = []

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
    propbank_frames = response_json['frameSet']
    if propbank_frames: 
        for propbank_frame in propbank_frames: 
            pf = propbank_frame['frameName']
            pf_list.append(pf)

    return pf_list

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

with open(input_file) as csvfile:
    lines = []
    content = csv.reader(csvfile) 
    next(content, None)
    for i,row in enumerate(content): 
        created_at = row[4]
        author_id = row[5]
        text = row[6]
        cleaned_text = text.replace("-", "")
        id = row[0]
        corpus = row[25]
        frames = get_frames(cleaned_text)
        values = []

        all_values = ['authority', 'betrayal', 'care', 'cheating', 'degradation', 'fairness', 'harm', 'loyalty', 'non_moral' , 'purity' , 'subversion']

        # values start at 
        highest_value = row[26:].index(max(row[26:]))

        gold_standard = all_values[highest_value]

        for frame in frames: 
            value = get_value(frame)
            values.append(value)
        success = 1 if gold_standard in [item for row in values for item in row] else 0

        lines.append(row + [frames, values])
        print(i)
        if i == 500: 
            break


with open(output_file, 'w+') as csvfile_out: 
    writer = csv.writer(csvfile_out)
    for line in lines: 
        writer.writerow(line)

end = time.time()
print("Time elapsed: " + str(end - start))
