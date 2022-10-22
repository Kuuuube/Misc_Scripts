import copy
import json
import random
from typing import List

def get_trigrams(texts: List[str]):
        
    trigrams = {}

    counts = {
        'start': {},
        'end': {},
    }

    word_count = len(texts)
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
    file = 'wordlists/english_5k.json'
    with open(file, 'r') as f:
        words = json.load(f)['texts']

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

    with open('outfile.json', 'w') as f:
        f.write(json.dumps(res, indent=4))


if __name__ == '__main__':
    main()
