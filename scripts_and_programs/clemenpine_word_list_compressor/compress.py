import copy
import json
import random
from typing import List

input_file = input("Input file path: ")
list_size = int(input("List size to generate: "))
fancy_formatting = input("Write list numbers (y/n): ")
lower_words = input("Treat capital and lowercase the same (y/n): ")

def get_trigrams(texts: List[str]):
        
    trigrams = {}

    counts = {
        'start': {},
        'end': {},
    }

    word_count = len(texts)

    #help prevent excessive capital letters by not considering them as separate trigrams
    if lower_words == "y":
        texts = [x.lower() for x in texts]
        
    for word in texts:
        
        padded_word = ' ' + word + ' '
        for i in range(len(padded_word) - 2):
            seq = padded_word[i:i+3]
            if not seq in trigrams:
                trigrams[seq] = word_count
            else:
                trigrams[seq] += word_count

        if not word[0] in counts['start']:
            counts['start'][word[0]] = 1
        else:
            counts['start'][word[0]] += 1

        if not word[-1] in counts['end']:
            counts['end'][word[-1]] = 1
        else:
            counts['end'][word[-1]] += 1

    for start in counts['start']:
        for end in counts['end']:
            trigrams[end + ' ' + start] = counts['start'][start] * counts['end'][end]

    # return dict(sorted(trigrams.items(), key=lambda x: x[1], reverse=True))
    return trigrams
    
    
def main():

    # initial info
    file = input_file
    with open(file, 'r') as f:
        json_file = json.load(f)
        if 'texts' in json_file:
            words = json_file['texts']
        elif 'words' in json_file:
            words = json_file['words']
    
    init_grams = get_trigrams(words).keys()


    # cut words
    while True:

        random.shuffle(words)
        for word in words:
            new_words = words.copy()
            new_words.remove(word)
    
            trigrams = get_trigrams(new_words).keys()
            for gram in init_grams:
                if not gram in trigrams:
                    break
            else:
                words = new_words
                print(f'{len(words)} words remaining. Removed {word} from list')
                break
        else:
            print('done')
            break

    # output
    res = {
        'total': len(words),
        'texts': words
    }

    with open('outfile.json', 'w') as jsonfile:
        jsonfile.write(json.dumps(res, indent=4))

    with open('outfile.txt', 'w') as plaintext:
        counter = 1
        list_number = 1
        if fancy_formatting == "y":
            plaintext.write("List " + str(list_number) + ":\n")
        for item in words:
            if counter > list_size:
                list_number += 1
                counter = 1
                plaintext.write("\n")
                if fancy_formatting == "y":
                    plaintext.write("List " + str(list_number) + ":\n")
            if counter <= list_size:
                plaintext.write(item)
                plaintext.write(" ")
                counter += 1


if __name__ == '__main__':
    main()
