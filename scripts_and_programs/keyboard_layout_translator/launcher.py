import keyboard_layout_encode_and_decode

while 1:
    inputstring = input("Input string to decode or encode here: ")

    keyboard_layout_encode_and_decode.decode_qwerty_to_dvorak(inputstring)
    keyboard_layout_encode_and_decode.encode_qwerty_to_dvorak(inputstring)
