use std::collections::HashMap;

pub fn convert_number(input_string: &str) -> String {
    let split_string: Vec<&str> = input_string.split(".").collect();
    if split_string.len() > 1 {
        return format!("{}てん{}", whole_number(split_string.get(0).unwrap()), fraction(split_string.get(1).unwrap()));
    }
    return whole_number(input_string);
}

fn whole_number(input_string: &str) -> String {
    match input_string.len() {
        1 => return digits_1(input_string, true),
        2 => return digits_2(input_string),
        3 => return digits_3(input_string),
        4 => return digits_4(input_string, false),
        5 => return digits_5(input_string),
        6 => return digits_6(input_string),
        7 => return digits_7(input_string),
        8 => return digits_8(input_string),
        9 => return digits_9(input_string),
        10 => return digits_10(input_string),
        11 => return digits_11(input_string),
        12 => return digits_12(input_string),
        13 => return digits_13(input_string),
        14 => return digits_14(input_string),
        15 => return digits_15(input_string),
        16 => return digits_16(input_string),
        17 => return digits_17(input_string),
        18 => return digits_18(input_string),
        19 => return digits_19(input_string),
        20 => return digits_20(input_string),
        21 => return digits_21(input_string),
        22 => return digits_22(input_string),
        23 => return digits_23(input_string),
        24 => return digits_24(input_string),
        25 => return digits_25(input_string),
        26 => return digits_26(input_string),
        27 => return digits_27(input_string),
        28 => return digits_28(input_string),
        _ => return "".to_string()
    };
}

fn fraction(input_string: &str) -> String {
    return input_string.replace("0", "ゼロ").replace("1", "いち").replace("2", "に").replace("3", "さん").replace("4", "よん").replace("5", "ご").replace("6", "ろく").replace("7", "なな").replace("8", "はち").replace("9", "きゅう").to_string();
}

fn dict_get(input_string: &str, use_one: bool) -> String {
    let dict: HashMap<&str, &str> = HashMap::from([
        ("1", "いち"),
        ("2", "に"),
        ("3", "さん"),
        ("4", "よん"),
        ("5", "ご"),
        ("6", "ろく"),
        ("7", "なな"),
        ("8", "はち"),
        ("9", "きゅう")
    ]);
    if input_string == "1" && !use_one {
        return "".to_string();
    }
    return dict.get(input_string).unwrap_or(&"").to_string();
}

fn digits_1(input_string: &str, use_zero: bool) -> String {
    if input_string == "0" && use_zero {
        return "ゼロ".to_string();
    }
    return dict_get(input_string, true);
}

fn digits_2(input_string: &str) -> String {
    let first_char = &input_string[0..1];
    let second_char = &input_string[1..];
    if first_char != "0" {
        return format!("{}{}{}", dict_get(first_char, false), "じゅう", digits_1(second_char, false));
    }
    return format!("{}{}", dict_get(first_char, false), digits_1(second_char, false));
}

fn digits_3(input_string: &str) -> String {
    let first_char = &input_string[0..1];
    let remaining_chars = &input_string[1..];
    match first_char {
        "0" => return format!("{}{}", dict_get(first_char, false), digits_2(remaining_chars)),
        "3" => return format!("{}{}{}", dict_get(first_char, false), "びゃく", digits_2(remaining_chars)),
        "6" => return format!("{}{}{}", "ろっ", "ぴゃく", digits_2(remaining_chars)),
        "8" => return format!("{}{}{}", "はっ", "ぴゃく", digits_2(remaining_chars)),
        _ => return format!("{}{}{}", dict_get(first_char, false), "ひゃく", digits_2(remaining_chars))
    }
}

fn digits_4(input_string: &str, prefix: bool) -> String {
    let first_char = &input_string[0..1];
    let remaining_chars = &input_string[1..];
    if prefix && first_char == "1" {
        return format!("{}{}", "いっせん", digits_3(remaining_chars));
    }
    if !prefix && first_char == "8" {
        return format!("{}{}", "はっせん", digits_3(remaining_chars));
    }

    match first_char {
        "0" => return format!("{}", digits_3(remaining_chars)),
        "3" => return format!("{}{}{}", dict_get(first_char, false), "ぜん", digits_3(remaining_chars)),
        "8" => return format!("{}{}{}", "は", "っせん", digits_3(remaining_chars)),
        _ => return format!("{}{}{}", dict_get(first_char, false), "せん", digits_3(remaining_chars))
    }
}

fn digits_5(input_string: &str) -> String {
    let first_char = &input_string[0..1];
    let remaining_chars = &input_string[1..];
    return format!("{}{}{}", dict_get(first_char, true), "まん", digits_4(remaining_chars, true));
}

fn digits_6(input_string: &str) -> String {
    let first_chars = &input_string[0..2];
    let remaining_chars = &input_string[2..];
    return format!("{}{}{}", digits_2(first_chars), "まん", digits_4(remaining_chars, true));
}

fn digits_7(input_string: &str) -> String {
    let first_chars = &input_string[0..3];
    let remaining_chars = &input_string[3..];
    return format!("{}{}{}", digits_3(first_chars), "まん", digits_4(remaining_chars, true));
}

fn digits_8(input_string: &str) -> String {
    let first_chars = &input_string[0..4];
    let remaining_chars = &input_string[4..];
    return format!("{}{}{}", digits_4(first_chars, true), "まん", digits_4(remaining_chars, true));
}

fn digits_9(input_string: &str) -> String {
    let first_char = &input_string[0..1];
    let remaining_chars_0 = &input_string[1..5];
    let remaining_chars_1 = &input_string[5..];
    if remaining_chars_0 != "0000" {
        return format!("{}{}{}{}{}", dict_get(first_char, true), "おく", digits_4(remaining_chars_0, true), "まん", digits_4(remaining_chars_1, true));
    }
    return format!("{}{}{}{}", dict_get(first_char, true), "おく", digits_4(remaining_chars_0, true), digits_4(remaining_chars_1, true));
}

fn digits_10(input_string: &str) -> String {
    let first_chars = &input_string[0..2];
    let remaining_chars_0 = &input_string[2..6];
    let remaining_chars_1 = &input_string[6..];
    if remaining_chars_0 != "0000" {
        return format!("{}{}{}{}{}", digits_2(first_chars), "おく", digits_4(remaining_chars_0, true), "まん", digits_4(remaining_chars_1, true));
    }
    return format!("{}{}{}{}", digits_2(first_chars), "おく", digits_4(remaining_chars_0, true), digits_4(remaining_chars_1, true));
}

fn digits_11(input_string: &str) -> String {
    let first_chars = &input_string[0..3];
    let remaining_chars_0 = &input_string[3..7];
    let remaining_chars_1 = &input_string[7..];
    if remaining_chars_0 != "0000" {
        return format!("{}{}{}{}{}", digits_3(first_chars), "おく", digits_4(remaining_chars_0, true), "まん", digits_4(remaining_chars_1, true));
    }
    return format!("{}{}{}{}", digits_3(first_chars), "おく", digits_4(remaining_chars_0, true), digits_4(remaining_chars_1, true));
}

fn digits_12(input_string: &str) -> String {
    let first_chars = &input_string[0..4];
    let remaining_chars_0 = &input_string[4..8];
    let remaining_chars_1 = &input_string[8..];
    if remaining_chars_0 != "0000" {
        return format!("{}{}{}{}{}", digits_4(first_chars, true), "おく", digits_4(remaining_chars_0, true), "まん", digits_4(remaining_chars_1, true));
    }
    return format!("{}{}{}{}", digits_4(first_chars, true), "おく", digits_4(remaining_chars_0, true), digits_4(remaining_chars_1, true));
}

fn digits_13(input_string: &str) -> String {
    let first_char = &input_string[0..1];
    let remaining_chars_0 = &input_string[1..5];
    let remaining_chars_1 = &input_string[5..9];
    let remaining_chars_2 = &input_string[9..];
    if remaining_chars_0 != "0000" {
        return format!("{}{}{}{}", dict_get(first_char, true).replace("いち", "いっ"), "ちょう", digits_4(remaining_chars_0, true), digits_9(&format!("{}{}{}", "0", remaining_chars_1, remaining_chars_2)));
    }
    if remaining_chars_1 != "0000" {
        return format!("{}{}{}{}", dict_get(first_char, true).replace("いち", "いっ"), "ちょう", digits_4(remaining_chars_0, true), digits_8(&format!("{}{}", remaining_chars_1, remaining_chars_2)));
    }
    return format!("{}{}{}{}{}", dict_get(first_char, true).replace("いち", "いっ"), "ちょう", digits_4(remaining_chars_0, true), digits_4(remaining_chars_1, true), digits_4(remaining_chars_2, true));
}

fn digits_14(input_string: &str) -> String {
    let first_chars = &input_string[0..2];
    let remaining_chars_0 = &input_string[2..6];
    let remaining_chars_1 = &input_string[6..10];
    let remaining_chars_2 = &input_string[10..];

    if remaining_chars_0 != "0000" {
        return format!("{}{}{}{}", digits_2(first_chars).replace("いち", "いっ"), "ちょう", digits_4(remaining_chars_0, true), digits_9(&format!("{}{}{}", "0", remaining_chars_1, remaining_chars_2)));
    }
    if remaining_chars_1 != "0000" {
        return format!("{}{}{}{}", digits_2(first_chars).replace("いち", "いっ"), "ちょう", digits_4(remaining_chars_0, true), digits_8(&format!("{}{}", remaining_chars_1, remaining_chars_2)));
    }
    return format!("{}{}{}{}{}", digits_2(first_chars).replace("いち", "いっ"), "ちょう", digits_4(remaining_chars_0, true), digits_4(remaining_chars_1, true), digits_4(remaining_chars_2, true));
}

fn digits_15(input_string: &str) -> String {
    let first_chars = &input_string[0..3];
    let remaining_chars_0 = &input_string[3..7];
    let remaining_chars_1 = &input_string[7..11];
    let remaining_chars_2 = &input_string[11..];
    if remaining_chars_0 != "0000" {
        return format!("{}{}{}{}", digits_3(first_chars).replace("いち", "いっ"), "ちょう", digits_4(remaining_chars_0, true), digits_9(&format!("{}{}{}", "0", remaining_chars_1, remaining_chars_2)));
    }
    if remaining_chars_1 != "0000" {
        return format!("{}{}{}{}", digits_3(first_chars).replace("いち", "いっ"), "ちょう", digits_4(remaining_chars_0, true), digits_8(&format!("{}{}", remaining_chars_1, remaining_chars_2)));
    }
    return format!("{}{}{}{}{}", digits_3(first_chars).replace("いち", "いっ"), "ちょう", digits_4(remaining_chars_0, true), digits_4(remaining_chars_1, true), digits_4(remaining_chars_2, true));
}

fn digits_16(input_string: &str) -> String {
    let first_chars = &input_string[0..4];
    let remaining_chars_0 = &input_string[4..8];
    let remaining_chars_1 = &input_string[8..12];
    let remaining_chars_2 = &input_string[12..];
    if remaining_chars_0 != "0000" {
        return format!("{}{}{}{}", digits_4(first_chars, true).replace("いち", "いっ"), "ちょう", digits_4(remaining_chars_0, true), digits_9(&format!("{}{}{}", "0", remaining_chars_1, remaining_chars_2)));
    }
    if remaining_chars_1 != "0000" {
        return format!("{}{}{}{}", digits_3(first_chars).replace("いち", "いっ"), "ちょう", digits_4(remaining_chars_0, true), digits_8(&format!("{}{}", remaining_chars_1, remaining_chars_2)));
    }
    return format!("{}{}{}{}{}", digits_4(first_chars, true).replace("いち", "いっ"), "ちょう", digits_4(remaining_chars_0, true), digits_4(remaining_chars_1, true), digits_4(remaining_chars_2, true));
}

fn digits_17(input_string: &str) -> String {
    let first_char = &input_string[0..1];
    let remaining_chars_0 = &input_string[1..5];
    let remaining_chars_1 = &input_string[5..9];
    let remaining_chars_2 = &input_string[9..13];
    let remaining_chars_3 = &input_string[13..];
    if remaining_chars_0 != "0000" {
        return format!("{}{}{}{}", dict_get(first_char, true), "けい", digits_4(remaining_chars_0, true).replace("いち", "いっ"), digits_13(&format!("{}{}{}{}", "0", remaining_chars_1, remaining_chars_2, remaining_chars_3)));
    }
    if remaining_chars_1 != "0000" {
        return format!("{}{}{}{}", dict_get(first_char, true), "けい", digits_4(remaining_chars_0, true).replace("いち", "いっ"), digits_12(&format!("{}{}{}", remaining_chars_1, remaining_chars_2, remaining_chars_3)));
    }
    return format!("{}{}{}{}{}{}", dict_get(first_char, true), "けい", digits_4(remaining_chars_0, true), digits_4(remaining_chars_1, true), digits_4(remaining_chars_2, true), digits_4(remaining_chars_3, true));
}

fn digits_18(input_string: &str) -> String {
    let first_chars = &input_string[0..2];
    let remaining_chars_0 = &input_string[2..6];
    let remaining_chars_1 = &input_string[6..10];
    let remaining_chars_2 = &input_string[10..14];
    let remaining_chars_3 = &input_string[14..];
    if remaining_chars_0 != "0000" {
        return format!("{}{}{}{}", digits_2(first_chars), "けい", digits_4(remaining_chars_0, true).replace("いち", "いっ"), digits_13(&format!("{}{}{}{}", "0", remaining_chars_1, remaining_chars_2, remaining_chars_3)));
    }
    if remaining_chars_1 != "0000" {
        return format!("{}{}{}{}", digits_2(first_chars), "けい", digits_4(remaining_chars_0, true).replace("いち", "いっ"), digits_12(&format!("{}{}{}", remaining_chars_1, remaining_chars_2, remaining_chars_3)));
    }
    return format!("{}{}{}{}{}{}", digits_2(first_chars), "けい", digits_4(remaining_chars_0, true), digits_4(remaining_chars_1, true), digits_4(remaining_chars_2, true), digits_4(remaining_chars_3, true));
}

fn digits_19(input_string: &str) -> String {
    let first_chars = &input_string[0..3];
    let remaining_chars_0 = &input_string[3..7];
    let remaining_chars_1 = &input_string[7..11];
    let remaining_chars_2 = &input_string[11..15];
    let remaining_chars_3 = &input_string[15..];
    if remaining_chars_0 != "0000" {
        return format!("{}{}{}{}", digits_3(first_chars), "けい", digits_4(remaining_chars_0, true).replace("いち", "いっ"), digits_13(&format!("{}{}{}{}", "0", remaining_chars_1, remaining_chars_2, remaining_chars_3)));
    }
    if remaining_chars_1 != "0000" {
        return format!("{}{}{}{}", digits_3(first_chars), "けい", digits_4(remaining_chars_0, true).replace("いち", "いっ"), digits_12(&format!("{}{}{}", remaining_chars_1, remaining_chars_2, remaining_chars_3)));
    }
    return format!("{}{}{}{}{}{}", digits_3(first_chars), "けい", digits_4(remaining_chars_0, true), digits_4(remaining_chars_1, true), digits_4(remaining_chars_2, true), digits_4(remaining_chars_3, true));
}

fn digits_20(input_string: &str) -> String {
    let first_chars = &input_string[0..4];
    let remaining_chars_0 = &input_string[4..8];
    let remaining_chars_1 = &input_string[8..12];
    let remaining_chars_2 = &input_string[12..16];
    let remaining_chars_3 = &input_string[16..];
    if remaining_chars_0 != "0000" {
        return format!("{}{}{}{}", digits_4(first_chars, true), "けい", digits_4(remaining_chars_0, true).replace("いち", "いっ"), digits_13(&format!("{}{}{}{}", "0", remaining_chars_1, remaining_chars_2, remaining_chars_3)));
    }
    if remaining_chars_1 != "0000" {
        return format!("{}{}{}{}", digits_4(first_chars, true), "けい", digits_4(remaining_chars_0, true).replace("いち", "いっ"), digits_12(&format!("{}{}{}", remaining_chars_1, remaining_chars_2, remaining_chars_3)));
    }
    return format!("{}{}{}{}{}{}", digits_4(first_chars, true), "けい", digits_4(remaining_chars_0, true), digits_4(remaining_chars_1, true), digits_4(remaining_chars_2, true), digits_4(remaining_chars_3, true));
}

fn digits_21(input_string: &str) -> String {
    let first_char = &input_string[0..1];
    let remaining_chars_0 = &input_string[1..5];
    let remaining_chars_1 = &input_string[5..9];
    let remaining_chars_2 = &input_string[9..13];
    let remaining_chars_3 = &input_string[13..17];
    let remaining_chars_4 = &input_string[17..];
    if remaining_chars_0 != "0000" {
        return format!("{}{}{}{}", dict_get(first_char, true), "がい", digits_4(remaining_chars_0, true), digits_17(&format!("{}{}{}{}{}", "0", remaining_chars_1, remaining_chars_2, remaining_chars_3, remaining_chars_4)));
    }
    if remaining_chars_1 != "0000" {
        return format!("{}{}{}{}", dict_get(first_char, true), "がい", digits_4(remaining_chars_0, true), digits_16(&format!("{}{}{}{}", remaining_chars_1, remaining_chars_2, remaining_chars_3, remaining_chars_4)));
    }
    if remaining_chars_2 != "0000" {
        return format!("{}{}{}{}{}", dict_get(first_char, true), "がい", digits_4(remaining_chars_0, true), digits_4(remaining_chars_0, true), digits_12(&format!("{}{}{}", remaining_chars_2, remaining_chars_3, remaining_chars_4)));
    }
    return format!("{}{}{}{}{}{}{}", dict_get(first_char, true), "がい", digits_4(remaining_chars_0, true), digits_4(remaining_chars_1, true), digits_4(remaining_chars_2, true), digits_4(remaining_chars_3, true), digits_4(remaining_chars_4, true));
}

fn digits_22(input_string: &str) -> String {
    let first_chars = &input_string[0..2];
    let remaining_chars_0 = &input_string[2..6];
    let remaining_chars_1 = &input_string[6..10];
    let remaining_chars_2 = &input_string[10..14];
    let remaining_chars_3 = &input_string[14..18];
    let remaining_chars_4 = &input_string[18..];
    if remaining_chars_0 != "0000" {
        return format!("{}{}{}{}", digits_2(first_chars), "がい", digits_4(remaining_chars_0, true), digits_17(&format!("{}{}{}{}{}", "0", remaining_chars_1, remaining_chars_2, remaining_chars_3, remaining_chars_4)));
    }
    if remaining_chars_1 != "0000" {
        return format!("{}{}{}{}", digits_2(first_chars), "がい", digits_4(remaining_chars_0, true), digits_16(&format!("{}{}{}{}", remaining_chars_1, remaining_chars_2, remaining_chars_3, remaining_chars_4)));
    }
    if remaining_chars_2 != "0000" {
        return format!("{}{}{}{}{}", digits_2(first_chars), "がい", digits_4(remaining_chars_0, true), digits_4(remaining_chars_0, true), digits_12(&format!("{}{}{}", remaining_chars_2, remaining_chars_3, remaining_chars_4)));
    }
    return format!("{}{}{}{}{}{}{}", digits_2(first_chars), "がい", digits_4(remaining_chars_0, true), digits_4(remaining_chars_1, true), digits_4(remaining_chars_2, true), digits_4(remaining_chars_3, true), digits_4(remaining_chars_4, true));
}

fn digits_23(input_string: &str) -> String {
    let first_chars = &input_string[0..3];
    let remaining_chars_0 = &input_string[3..7];
    let remaining_chars_1 = &input_string[7..11];
    let remaining_chars_2 = &input_string[11..15];
    let remaining_chars_3 = &input_string[15..19];
    let remaining_chars_4 = &input_string[19..];
    if remaining_chars_0 != "0000" {
        return format!("{}{}{}{}", digits_3(first_chars), "がい", digits_4(remaining_chars_0, true), digits_17(&format!("{}{}{}{}{}", "0", remaining_chars_1, remaining_chars_2, remaining_chars_3, remaining_chars_4)));
    }
    if remaining_chars_1 != "0000" {
        return format!("{}{}{}{}", digits_3(first_chars), "がい", digits_4(remaining_chars_0, true), digits_16(&format!("{}{}{}{}", remaining_chars_1, remaining_chars_2, remaining_chars_3, remaining_chars_4)));
    }
    if remaining_chars_2 != "0000" {
        return format!("{}{}{}{}{}", digits_3(first_chars), "がい", digits_4(remaining_chars_0, true), digits_4(remaining_chars_0, true), digits_12(&format!("{}{}{}", remaining_chars_2, remaining_chars_3, remaining_chars_4)));
    }
    return format!("{}{}{}{}{}{}{}", digits_3(first_chars), "がい", digits_4(remaining_chars_0, true), digits_4(remaining_chars_1, true), digits_4(remaining_chars_2, true), digits_4(remaining_chars_3, true), digits_4(remaining_chars_4, true));
}

fn digits_24(input_string: &str) -> String {
    let first_chars = &input_string[0..4];
    let remaining_chars_0 = &input_string[4..8];
    let remaining_chars_1 = &input_string[8..12];
    let remaining_chars_2 = &input_string[12..16];
    let remaining_chars_3 = &input_string[16..20];
    let remaining_chars_4 = &input_string[20..];
    if remaining_chars_0 != "0000" {
        return format!("{}{}{}{}", digits_4(first_chars, true), "がい", digits_4(remaining_chars_0, true), digits_17(&format!("{}{}{}{}{}", "0", remaining_chars_1, remaining_chars_2, remaining_chars_3, remaining_chars_4)));
    }
    if remaining_chars_1 != "0000" {
        return format!("{}{}{}{}", digits_4(first_chars, true), "がい", digits_4(remaining_chars_0, true), digits_16(&format!("{}{}{}{}", remaining_chars_1, remaining_chars_2, remaining_chars_3, remaining_chars_4)));
    }
    if remaining_chars_2 != "0000" {
        return format!("{}{}{}{}{}", digits_4(first_chars, true), "がい", digits_4(remaining_chars_0, true), digits_4(remaining_chars_0, true), digits_12(&format!("{}{}{}", remaining_chars_2, remaining_chars_3, remaining_chars_4)));
    }
    return format!("{}{}{}{}{}{}{}", digits_4(first_chars, true), "がい", digits_4(remaining_chars_0, true), digits_4(remaining_chars_1, true), digits_4(remaining_chars_2, true), digits_4(remaining_chars_3, true), digits_4(remaining_chars_4, true));
}

fn digits_25(input_string: &str) -> String {
    let first_char = &input_string[0..1];
    let remaining_chars_0 = &input_string[1..5];
    let remaining_chars_1 = &input_string[5..9];
    let remaining_chars_2 = &input_string[9..13];
    let remaining_chars_3 = &input_string[13..17];
    let remaining_chars_4 = &input_string[17..21];
    let remaining_chars_5 = &input_string[21..];
    if remaining_chars_0 != "0000" {
        return format!("{}{}{}{}", dict_get(first_char, true), "じょ", digits_4(remaining_chars_0, true), digits_21(&format!("{}{}{}{}{}{}", "0", remaining_chars_1, remaining_chars_2, remaining_chars_3, remaining_chars_4, remaining_chars_5)));
    }
    if remaining_chars_1 != "0000" {
        return format!("{}{}{}{}", dict_get(first_char, true), "じょ", digits_4(remaining_chars_0, true), digits_20(&format!("{}{}{}{}{}", remaining_chars_1, remaining_chars_2, remaining_chars_3, remaining_chars_4, remaining_chars_5)));
    }
    if remaining_chars_2 != "0000" {
        return format!("{}{}{}{}{}", dict_get(first_char, true), "じょ", digits_4(remaining_chars_0, true), digits_4(remaining_chars_1, true), digits_16(&format!("{}{}{}{}", remaining_chars_2, remaining_chars_3, remaining_chars_4, remaining_chars_5)));
    }
    return format!("{}{}{}{}{}{}{}{}", dict_get(first_char, true), "じょ", digits_4(remaining_chars_0, true), digits_4(remaining_chars_1, true), digits_4(remaining_chars_2, true), digits_4(remaining_chars_3, true), digits_4(remaining_chars_4, true), digits_4(remaining_chars_5, true));}

fn digits_26(input_string: &str) -> String {
    let first_chars = &input_string[0..2];
    let remaining_chars_0 = &input_string[2..6];
    let remaining_chars_1 = &input_string[6..10];
    let remaining_chars_2 = &input_string[10..14];
    let remaining_chars_3 = &input_string[14..18];
    let remaining_chars_4 = &input_string[18..22];
    let remaining_chars_5 = &input_string[22..];
    if remaining_chars_0 != "0000" {
        return format!("{}{}{}{}", digits_2(first_chars), "じょ", digits_4(remaining_chars_0, true), digits_21(&format!("{}{}{}{}{}{}", "0", remaining_chars_1, remaining_chars_2, remaining_chars_3, remaining_chars_4, remaining_chars_5)));
    }
    if remaining_chars_1 != "0000" {
        return format!("{}{}{}{}", digits_2(first_chars), "じょ", digits_4(remaining_chars_0, true), digits_20(&format!("{}{}{}{}{}", remaining_chars_1, remaining_chars_2, remaining_chars_3, remaining_chars_4, remaining_chars_5)));
    }
    if remaining_chars_2 != "0000" {
        return format!("{}{}{}{}{}", digits_2(first_chars), "じょ", digits_4(remaining_chars_0, true), digits_4(remaining_chars_1, true), digits_16(&format!("{}{}{}{}", remaining_chars_2, remaining_chars_3, remaining_chars_4, remaining_chars_5)));
    }
    return format!("{}{}{}{}{}{}{}{}", digits_2(first_chars), "じょ", digits_4(remaining_chars_0, true), digits_4(remaining_chars_1, true), digits_4(remaining_chars_2, true), digits_4(remaining_chars_3, true), digits_4(remaining_chars_4, true), digits_4(remaining_chars_5, true));}

fn digits_27(input_string: &str) -> String {
    let first_chars = &input_string[0..3];
    let remaining_chars_0 = &input_string[3..7];
    let remaining_chars_1 = &input_string[7..11];
    let remaining_chars_2 = &input_string[11..15];
    let remaining_chars_3 = &input_string[15..19];
    let remaining_chars_4 = &input_string[19..23];
    let remaining_chars_5 = &input_string[23..];
    if remaining_chars_0 != "0000" {
        return format!("{}{}{}{}", digits_3(first_chars), "じょ", digits_4(remaining_chars_0, true), digits_21(&format!("{}{}{}{}{}{}", "0", remaining_chars_1, remaining_chars_2, remaining_chars_3, remaining_chars_4, remaining_chars_5)));
    }
    if remaining_chars_1 != "0000" {
        return format!("{}{}{}{}", digits_3(first_chars), "じょ", digits_4(remaining_chars_0, true), digits_20(&format!("{}{}{}{}{}", remaining_chars_1, remaining_chars_2, remaining_chars_3, remaining_chars_4, remaining_chars_5)));
    }
    if remaining_chars_2 != "0000" {
        return format!("{}{}{}{}{}", digits_3(first_chars), "じょ", digits_4(remaining_chars_0, true), digits_4(remaining_chars_1, true), digits_16(&format!("{}{}{}{}", remaining_chars_2, remaining_chars_3, remaining_chars_4, remaining_chars_5)));
    }
    return format!("{}{}{}{}{}{}{}{}", digits_3(first_chars), "じょ", digits_4(remaining_chars_0, true), digits_4(remaining_chars_1, true), digits_4(remaining_chars_2, true), digits_4(remaining_chars_3, true), digits_4(remaining_chars_4, true), digits_4(remaining_chars_5, true));}

fn digits_28(input_string: &str) -> String {
    let first_chars = &input_string[0..4];
    let remaining_chars_0 = &input_string[4..8];
    let remaining_chars_1 = &input_string[8..12];
    let remaining_chars_2 = &input_string[12..16];
    let remaining_chars_3 = &input_string[16..20];
    let remaining_chars_4 = &input_string[20..24];
    let remaining_chars_5 = &input_string[24..];
    if remaining_chars_0 != "0000" {
        return format!("{}{}{}{}", digits_4(first_chars, true), "じょ", digits_4(remaining_chars_0, true), digits_21(&format!("{}{}{}{}{}{}", "0", remaining_chars_1, remaining_chars_2, remaining_chars_3, remaining_chars_4, remaining_chars_5)));
    }
    if remaining_chars_1 != "0000" {
        return format!("{}{}{}{}", digits_4(first_chars, true), "じょ", digits_4(remaining_chars_0, true), digits_20(&format!("{}{}{}{}{}", remaining_chars_1, remaining_chars_2, remaining_chars_3, remaining_chars_4, remaining_chars_5)));
    }
    if remaining_chars_2 != "0000" {
        return format!("{}{}{}{}{}", digits_4(first_chars, true), "じょ", digits_4(remaining_chars_0, true), digits_4(remaining_chars_1, true), digits_16(&format!("{}{}{}{}", remaining_chars_2, remaining_chars_3, remaining_chars_4, remaining_chars_5)));
    }
    return format!("{}{}{}{}{}{}{}{}", digits_4(first_chars, true), "じょ", digits_4(remaining_chars_0, true), digits_4(remaining_chars_1, true), digits_4(remaining_chars_2, true), digits_4(remaining_chars_3, true), digits_4(remaining_chars_4, true), digits_4(remaining_chars_5, true));
}