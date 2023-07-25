# JP Number Converter

Outputs the japanese numeral equivalent for given arabic numerals.

## Usage

- Run `jp_number_converter` and input a number.

## Building

```
cargo build --release
```

## Notes

- Currently conversion to hiragana, kanji, banknote-style daiji, and daiji are supported.

- Decimals and fractions (in this format `123.456`) are supported.

- Numbers from 0-999999999999999999999999999999999999999999999999999999999999999999999999 (10<sup>73</sup>-1) are supported (the highest number with specific words for it in japanese). If you're wondering how that looks written out:

    Hiragana:

    きゅうせんきゅうひゃくきゅうじゅうきゅうむりょうたいすうきゅうせんきゅうひゃくきゅうじゅうきゅうふかしぎきゅうせんきゅうひゃくきゅうじゅうきゅうなゆたきゅうせんきゅうひゃくきゅうじゅうきゅうあそうぎきゅうせんきゅうひゃくきゅうじゅうきゅうこうがしゃきゅうせんきゅうひゃくきゅうじゅうきゅうごくきゅうせんきゅうひゃくきゅうじゅうきゅうさいきゅうせんきゅうひゃくきゅうじゅうきゅうせいきゅうせんきゅうひゃくきゅうじゅうきゅうかんきゅうせんきゅうひゃくきゅうじゅうきゅうこうきゅうせんきゅうひゃくきゅうじゅうきゅうじょうきゅうせんきゅうひゃくきゅうじゅうきゅうじょきゅうせんきゅうひゃくきゅうじゅうきゅうがいきゅうせんきゅうひゃくきゅうじゅうきゅうけいきゅうせんきゅうひゃくきゅうじゅうきゅうちょうきゅうせんきゅうひゃくきゅうじゅうきゅうおくきゅうせんきゅうひゃくきゅうじゅうきゅうまんきゅうせんきゅうひゃくきゅうじゅうきゅう

    Kanji:

    九千九百九十九無量大数九千九百九十九不可思議九千九百九十九那由他九千九百九十九阿僧祇九千九百九十九恒河沙九千九百九十九極九千九百九十九載九千九百九十九正九千九百九十九澗九千九百九十九溝九千九百九十九穣九千九百九十九𥝱九千九百九十九垓九千九百九十九京九千九百九十九兆九千九百九十九億九千九百九十九万九千九百九十九

    Banknote-style Daiji:

    九阡九佰九拾九無量大数九阡九佰九拾九不可思議九阡九佰九拾九那由他九阡九佰九拾九阿僧祇九阡九佰九拾九恒河沙九阡九佰九拾九極九阡九佰九拾九載九阡九佰九拾九正九阡九佰九拾九澗九阡九佰九拾九溝九阡九佰九拾九穣九阡九佰九拾九𥝱九阡九佰九拾九垓九阡九佰九拾九京九阡九佰九拾九兆九阡九佰九拾九億九阡九佰九拾九萬九阡九佰九拾九

    Daiji:

    玖阡玖佰玖拾玖無量大数玖阡玖佰玖拾玖不可思議玖阡玖佰玖拾玖那由他玖阡玖佰玖拾玖阿僧祇玖阡玖佰玖拾玖恒河沙玖阡玖佰玖拾玖極玖阡玖佰玖拾玖載玖阡玖佰玖拾玖正玖阡玖佰玖拾玖澗玖阡玖佰玖拾玖溝玖阡玖佰玖拾玖穣玖阡玖佰玖拾玖𥝱玖阡玖佰玖拾玖垓玖阡玖佰玖拾玖京玖阡玖佰玖拾玖兆玖阡玖佰玖拾玖億玖阡玖佰玖拾玖萬玖阡玖佰玖拾玖

- Inspired by [https://www.sljfaq.org/cgi/numbers.cgi](https://www.sljfaq.org/cgi/numbers.cgi). If my number converter was something of interest to you, you my find many other interesting things on this site.