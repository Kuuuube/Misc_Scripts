qwerty_to_dvorak ={
        "q" : "'",
        "w" : ",",
        "e" : ".",
        "r" : "p",
        "t" : "y",
        "y" : "f",
        "u" : "g",
        "i" : "c",
        "o" : "r",
        "p" : "l",
        "[" : "/",
        "]" : "=",
        "a" : "a",
        "s" : "o",
        "d" : "e",
        "f" : "u",
        "g" : "i",
        "h" : "d",
        "j" : "h",
        "k" : "t",
        "l" : "n",
        ";" : "s",
        "'" : "-",
        "z" : ";",
        "x" : "q",
        "c" : "j",
        "v" : "k",
        "b" : "x",
        "n" : "b",
        "m" : "m",
        "," : "w",
        "." : "v",
        "/" : "z",
        " " : " ",
        ":" : ":"        
        }
inverse_qwerty_to_dvorak = dict((v,k) for (k,v) in qwerty_to_dvorak.items())

qwerty_to_colemak_mod_dh_ansi ={
        "q" : "q",
        "w" : "w",
        "e" : "f",
        "r" : "p",
        "t" : "b",
        "y" : "j",
        "u" : "l",
        "i" : "u",
        "o" : "y",
        "p" : ";",
        "[" : "[",
        "]" : "]",
        "a" : "a",
        "s" : "r",
        "d" : "s",
        "f" : "t",
        "g" : "g",
        "h" : "k",
        "j" : "n",
        "k" : "e",
        "l" : "i",
        ";" : "o",
        "'" : "'",
        "z" : "x",
        "x" : "c",
        "c" : "d",
        "v" : "v",
        "b" : "z",
        "n" : "m",
        "m" : "h",
        "," : ",",
        "." : ".",
        "/" : "/",
        " " : " ",
        ":" : ":"
        }
inverse_qwerty_to_colemak_mod_dh_ansi = ((v,k) for (k,v) in qwerty_to_colemak_mod_dh_ansi.items())



def decode_qwerty_to_dvorak(message):
    decodedMessage = ""
    for char in message[:]:
        decodedMessage += inverse_qwerty_to_dvorak[char.lower()]
    print("Encoded (dvorak typing on qwerty) or Decoded (qwerty typing on dvorak):")
    print(decodedMessage)
        
def encode_qwerty_to_dvorak(message):
    encodedMessage = ""
    for char in message[:]:
        encodedMessage += qwerty_to_dvorak[char.lower()]
    print("Encoded (qwerty typing on dvorak) or Decoded (dvorak typing on qwerty):")
    print(encodedMessage)
