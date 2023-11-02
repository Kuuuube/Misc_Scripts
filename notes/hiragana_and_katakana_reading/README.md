# Hiragana and Katakana Reading

A method to learn to read Hiragana and Katakana incredibly fast through typing.

**If you are not familiar with using a terminal you may want to use the online [DJT Kana](https://djtguide.neocities.org/kana/) ([mirror](https://djtguide.github.io/learn/kana.html)) tool instead of this guide. You can use it without even having a Japanese IME installed.**

## Guide

- Install a Japanese IME.

    Windows: Install [the Japanese Language and Microsoft IME](https://support.microsoft.com/en-us/windows/install-a-language-for-windows-ccd853d3-9ecd-7da7-9ef0-72b4a055410a), Install [Google IME](https://www.google.co.jp/ime/), or Install [Mozc](https://github.com/google/mozc) (Download from github actions)

    Linux: Install [Mozc](https://github.com/google/mozc)

- You should now be able to switch between typing with your standard keyboard settings and typing with Japanese IME input. You will want to use romanji conversion to kana and not direct kana input.

    If you are using Microsoft IME style keybinds, you can press ``Alt + ` `` to switch from standard keyboard input to Japanese IME input. (`Alt + Caps` and `Ctrl + Caps` to switch between hiragana and katakana)

    To test if it's working, try typing something. For example, `a` should turn into `あ`. (Depending on the IME you may need to enable and disable per application so check that it hasn't disabled itself if you have issues)

- Download [Random Dict Picker](../../scripts_and_programs/random_dict_picker/) and install the dependencies it mentions. Don't worry about the program usage instructions.

- Make sure you have a good terminal emulator. If you're on Windows, CMD or Powershell may not suffice. [Alacritty](https://alacritty.org/) is a nice simple terminal you can install.

    Optionally, you can also copy my [Terminal and Shell Config](../../scripts_and_programs/terminal_and_shell_config/). However, the default settings of any good terminal will work for the purposes of this guide.

- Skip down to [Random Dict Picker Usage](./README.md#random-dict-picker-usage). This is the basic details of how you should run Random Dict Picker for the purposes of this guide. The files to use are detailed in the next step.

    Make sure to use the ways of typing that are closest to the pronunciation as opposed to the "optimal" way of typing the characters. For example, `つ` can be typed by inputting `tsu` or `tu`. You will want to input `tsu` as it is closest to the sound. Even though pressing two keys instead of one could make you faster at typing, it may make it harder to learn the character.

    If you don't already know how to pronounce Japanese syllables I suggest watching this: [Learn Hiragana ひらがな](https://www.youtube.com/watch?v=Bsfi4XbPE8M) ([archive.org mirror](https://web.archive.org/web/0/https://www.youtube.com/watch?v=Bsfi4XbPE8M&gl=US&hl=en))

- I have included json files for each step of learning in the `Levels` folder. These are the files you should input into Random Dict Picker.

    You should start at `hiragana_level_01.json` and move up each time you feel somewhat comfortable with the current level. Only start on katakana after you have completed all hiragana levels.

    I define the amount of comfort required to move on as being able to read any character in the level within 5-10 seconds. This may sound very slow but knowing every character is much more important than speed.

    It is very important that you try to read the sounds of the characters as you do this and not just memorize the hand positions to type them. One way to do this is to read each character aloud (make sure to keep the correct pronunciations in mind, not trying to read them correctly is worse than not reading them at all). You don't need to read them loudly, just quietly under your breath is enough. This may hinder your speed but that's okay. The goal is not to type fast, it is to learn to read hiragana and katakana.

- I have also included a hiragana (`hiragana_table_plaintext.txt`) and katakana chart (`katakana_table_plaintext.txt`). You can open these up in any text editor or viewer. If you would prefer, you can use your own hiragana and katakana chart. Make sure to zoom these in so you can properly read them, you want them to show somewhat thick, not thin 1px lines.

    Every time you get stuck on a character, don't start randomly pressing keys hoping to get it and don't skip the character either. Take a look at the chart and type it correctly. 

    Excluding the start of each level where you don't know the new characters yet, you want to look at the chart as little as possible. Try to think about the character you're stuck on for a few seconds before looking over.

- After you feel comfortable with levels 1-15, you should try the `hiragana_sutegana.json` and `katakana_sutegana.json` files.

    Sutegana are small versions of kana that are attatched to the characters around them. For example, in `にゃ`, `ゃ` is a sutegana.

    These are not hard once you know how to read the rest of hiragana or katakana. `にゃ` is just `に` (`ni`) and `や` (`ya`) mashed together to create `にゃ` (`nya`). There are a few such as `じゃ` or `ちゃ` which are slightly different but they shouldn't be hard to figure out with some practice. (Don't forget about the pronunciation guide I linked above if you're unsure)

## Random Dict Picker Usage

- I recommend using Random Dict Picker with the following args:

    ```
    random_dict_picker.py -f input_file.json -c 0.999 -m repeat
    ```

    Replace `input_file.json` with the file you want to use

- You should see a string of characters appear. You need to type the prompt exactly as it shows then press enter. You should backspace and correct any mistakes you notice as you are typing but don't bother proofreading everything you typed right before you press enter.

    Correct characters will be highlighted in green, mistakes will be highlighted in red.

    If the characters in the prompt are too small, zoom your terminal in as much as you need and hit enter to move on to the next prompt. The number of characters displayed will adjust for your terminal display size.

- After you have completed a prompt, you may then press enter again at the `:` to move on to the next prompt.

- If you want to change which file you're currently using, you dont need to restart Random Dict Picker. Instead, send the following arg at the `:` after a prompt.

    ```
    -f input_file.json
    ```

## Things this guide lacks:

- An important sutegana that not included in the files for this guide is `っ` or `ッ`. These are そくおん (also called 小さいつ or 小さなつ) or sokuon and they roughtly act to duplicate the consonant that comes after it and add a little pause in the pronunciation (watch [this](https://www.youtube.com/watch?v=EnGHbpZEoeQ) for pronunciation). 

    For example, in `まって` or `matte` the sokuon becomes an extra "t" for the `て` or `te`. You can type a sokuon in a word by simply repeating the consonant before a character twice (typing `matte` will automatically result in `まって`) or you can specifically type it with `xtsu` or `ltsu`. 

    (All small characters can be typed by appending `x` or `l` to the start of them. For example `xyu` or `lyu` results in `ゅ`)

- You may also see sutegana looking like this `ぁぃぅぇぉ` that also aren't included in this guide's files. These are less used but you may see them occasionally. 

    For example, `ふぁ` sounds like `ふ` but with an `あ` vowel instead of an `う` vowel. It can also be typed with `fa`. Generally, the way these act should be reasonably easy to figure out.

- Another important character missing from this guide is the `ちょうおんぷ` or chouonpu. It looks like a long dash `ー` and can be typed by inputting a dash. It is mostly used in katakana and extends a vowel sound. 

    (In hiragana, you will usually see an extra vowel character (`あいうえお`) instead of a chouonpu to extend the vowel sound)

- The last things missing from this guide are `ゐゑぢづぢゃぢゅぢょ` (`ヰヱヂヅヂャヂュヂョ`). 

    `ゐ` and `ゑ` (`ヰ` and `ヱ`) are near obselete kana. It's good to be aware of their existence but they aren't very important. Your IME also won't let you type them easily. Feel free to learn them if you are interested but it isn't required and you will almost never see them.

    `ぢ` and `づ` (`ヂ` and `ヅ`) are not as uncommon but are typically superceded by `じ` and `ず` (`ジ` and `ズ`) which are pronounced the same. The versions with added sutegana are also pronounced the same. You can type `ぢ` (`ヂ`) with `di` and `づ` (`ヅ`) with `du` but **do not** pronounce them like this. `ぢ` (`ヂ`) is pronounced the same as `じ` (`ジ`, `ji`) and `づ` (`ヅ`) is pronounced the same as `ず` (`ズ`, `zu`).

    `づ` is the only one of these that has any reasonable amount of usage but you don't need to worry much about it when you are starting out learning kana.

## Notes

- I learned all the hiragana and katakana in this guide in just over a week spending about an hour a day on this. I think most people should be able learn it equally as fast or faster. You don't need to study an hour each day either, you can make great progress from just 10-30 mins a day.

    Years before I actually learned hiragana and katakana using this method I had tried other things and given up after a few months of little to no progress. I randomly happened to get the idea for learning this through typing and had no trouble remembering the characters after some practice. I kept increasing the number of characters to remember and ended up learning all of it easily.

- I am not an expert on Japanese teaching or anything remotely close to that. I've written this guide to share the method that worked well for me.

- If you want to learn to read AND write hiragana and katakana, you do need to practice physical writing as well, not just typing. However, it is certainly easier to learn to write hiragana and katakana when you already know how to read it and have a general idea of how the characters work.

- If you do not know how to touch type and rely on "hunt and peck", the speed of your progress with this method may be hindered.