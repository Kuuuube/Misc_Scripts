import sys
import json
import collections

def get_words(file: str):
    with open(file, 'r') as f:
        json_file = json.load(f)
        if 'texts' in json_file:
            words = json_file['texts']
        elif 'words' in json_file:
            words = json_file['words']
        else:
            print("Invalid json")

    return words


def compress(words: list[str], *, file: str):
    stats = get_data(words)

    save = []
    while words:

        word = words[0]
        grams = get_trigrams(word)
    
        remaining = [stats['trigrams'][gram] - count for gram, count in grams.items()]

        if (
            stats['boundary']['end'][word[-1]] > 1 and
            stats['boundary']['start'][word[0]] > 1 and
            min(remaining) > 0
        ):
            words.remove(word)
            for gram, count in grams.items():
                stats['trigrams'][gram] -= count

            stats['boundary']['end'][word[-1]] -= 1
            stats['boundary']['start'][word[0]] -= 1
            
        else:
            words.remove(word)
            save.append(word)
    return save


def get_trigrams(word: str):
    word = f' {word} '
    grams = [''.join(x) for x in zip(word, word[1:], word[2:])]
    return collections.Counter(grams)


def get_data(words: list[str]):
    
    data = {
        'trigrams': {},
        'boundary': {},
    }

    counts = {
        'start': {},
        'end': {},
    }

    for word in words:

        padded_word = ' ' + word + ' '
        for i in range(len(padded_word) - 2):
            seq = padded_word[i:i+3]
            if not seq in data['trigrams']:
                data['trigrams'][seq] = 1
            else:
                data['trigrams'][seq] += 1

        if not word[0] in counts['start']:
            counts['start'][word[0]] = 1
        else:
            counts['start'][word[0]] += 1

        if not word[-1] in counts['end']:
            counts['end'][word[-1]] = 1
        else:
            counts['end'][word[-1]] += 1

    data['boundary'] = counts

    return data


def main():

    file = input("Input file path: ")

    words = get_words(f'{file}')
    saved = compress(words, file=file)

    with open(f'output.txt', 'w') as txtfile:
        text = ' '.join(saved)
        txtfile.write(text)

    with open(f'output.json', 'w') as jsonfile:
        jsonfile.write("{\n    \"total\": " + str(len(saved)) + ",\n    \"texts\": [\n")
        for word in saved:
            jsonfile.write("        \"" + word + "\",\n")

        jsonfile.write("    ]\n}")


if __name__ == '__main__':
    main()
