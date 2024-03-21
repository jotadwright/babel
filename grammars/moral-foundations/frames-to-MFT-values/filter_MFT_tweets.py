from argparse import ArgumentParser
import csv
import time

start = time.time()

parser = ArgumentParser()
parser.add_argument('--input_file_authors', default="/Users/lara/Projects/datasets/MFT_twitter/MFTC_users.csv")          
parser.add_argument('--input_file_tweets', default="/Users/lara/Projects/datasets/MFT_twitter/MFTC_tweets.csv") 
parser.add_argument('--output_file', default="/Users/lara/Projects/datasets/MFT_twitter/filtered_MFTC_tweets.csv") 


args = parser.parse_args()
input_file_authors = args.input_file_authors
input_file_tweets = args.input_file_tweets
output_file = args.output_file

authors = []
with open(input_file_authors) as authorcsvfile:
    lines = []
    content = csv.reader(authorcsvfile) 
    next(content, None)
    highest_count = 0
    highest_count_and_verified = 0
    for i,row in enumerate(content): 
        author_id = row[7]
        verified = row[6] 
        public_follower_count = int(row[13])
        if public_follower_count > highest_count: 
            highest_count = public_follower_count
        
        if verified == "TRUE": 
            authors.append(author_id)

with open(input_file_tweets) as tweetcsvfile: 
    tweet_content = csv.reader(tweetcsvfile) 
    next(tweet_content, None)
    for i, row in enumerate(tweet_content): 
        author_id = row[5]
        if author_id in authors: 
            lines.append(row)

with open(output_file, 'w+') as csvfile_out: 
    writer = csv.writer(csvfile_out)
    for line in lines: 
        writer.writerow(line)

end = time.time()
print("Time elapsed: " + str(end - start))
