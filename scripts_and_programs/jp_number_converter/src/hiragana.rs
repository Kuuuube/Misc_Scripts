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
        5 => return digits_5_8(input_string),
        6 => return digits_5_8(input_string),
        7 => return digits_5_8(input_string),
        8 => return digits_5_8(input_string),
        9 => return digits_9_12(input_string),
        10 => return digits_9_12(input_string),
        11 => return digits_9_12(input_string),
        12 => return digits_9_12(input_string),
        13 => return digits_13_16(input_string),
        14 => return digits_13_16(input_string),
        15 => return digits_13_16(input_string),
        16 => return digits_13_16(input_string),
        17 => return digits_17_20(input_string),
        18 => return digits_17_20(input_string),
        19 => return digits_17_20(input_string),
        20 => return digits_17_20(input_string),
        21 => return digits_21_24(input_string),
        22 => return digits_21_24(input_string),
        23 => return digits_21_24(input_string),
        24 => return digits_21_24(input_string),
        25 => return digits_25_28(input_string),
        26 => return digits_25_28(input_string),
        27 => return digits_25_28(input_string),
        28 => return digits_25_28(input_string),
        29 => return digits_29_32(input_string),
        30 => return digits_29_32(input_string),
        31 => return digits_29_32(input_string),
        32 => return digits_29_32(input_string),
        33 => return digits_33_36(input_string),
        34 => return digits_33_36(input_string),
        35 => return digits_33_36(input_string),
        36 => return digits_33_36(input_string),
        37 => return digits_37_40(input_string),
        38 => return digits_37_40(input_string),
        39 => return digits_37_40(input_string),
        40 => return digits_37_40(input_string),
        41 => return digits_41_44(input_string),
        42 => return digits_41_44(input_string),
        43 => return digits_41_44(input_string),
        44 => return digits_41_44(input_string),
        45 => return digits_45_48(input_string),
        46 => return digits_45_48(input_string),
        47 => return digits_45_48(input_string),
        48 => return digits_45_48(input_string),
        49 => return digits_49_52(input_string),
        50 => return digits_49_52(input_string),
        51 => return digits_49_52(input_string),
        52 => return digits_49_52(input_string),
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

fn handle_first_chars(first_chars: &str) -> String {
    match first_chars.len() {
        1 => return dict_get(first_chars, true),
        2 => return digits_2(first_chars),
        3 => return digits_3(first_chars),
        4 => return digits_4(first_chars, true),
        _ => return "".to_string()
    }
}

fn digits_4_for_slice(input_string: &str) -> String {
    let mut output_string = String::default();
    let max_len = input_string.len();
    let mut slices_vec: Vec<&str> = Default::default();
    for i in 4..=(max_len - 4) {
        if i % 4 == 0 {
            slices_vec.push(input_string.get(max_len - (max_len - i)..max_len - (max_len - (i + 4))).unwrap_or(""));
        }
    }
    for slice in slices_vec {
        output_string += digits_4(slice, true).as_str();
    }
    return output_string;
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

fn digits_5_8(input_string: &str) -> String {
    let digits = input_string.len();
    let first_chars = &input_string[0..digits - 4];
    let remaining_chars_0 = &input_string[digits - 4..];

    return format!("{}{}{}", handle_first_chars(first_chars), "まん", digits_4(remaining_chars_0, true));
}

fn digits_9_12(input_string: &str) -> String {
    let digits = input_string.len();
    let max_len = 12;
    let digit_range_characters = "おく";

    if &input_string[digits - (max_len - 4)..digits - (max_len - 8)] != "0000" {
        return format!("{}{}{}{}{}", handle_first_chars(&input_string[0..digits - (max_len - 4)]), digit_range_characters, digits_4(&input_string[digits - (max_len - 4)..digits - (max_len - 8)], true), "まん", digits_4(&input_string[digits - (max_len - 8)..digits - (max_len - 12)], true));
    }
    return format!("{}{}{}", handle_first_chars(&input_string[0..digits - (max_len - 4)]), digit_range_characters, digits_4_for_slice(&input_string[digits - (max_len - 4)..digits - (max_len - 12)]));
}

fn digits_13_16(input_string: &str) -> String {
    let digits = input_string.len();
    let max_len = 16;
    let first_chars = &input_string[0..digits - (max_len - 4)];
    let digit_range_characters = "ちょう";

    if &input_string[digits - (max_len - 4)..digits - (max_len - 8)] != "0000" {
        return format!("{}{}{}{}", handle_first_chars(first_chars).replace("いち", "いっ"), digit_range_characters, digits_4(&input_string[digits - (max_len - 4)..digits - (max_len - 8)], true), digits_9_12(&format!("{}{}", "0", &input_string[digits - (max_len - 8)..])));
    }
    if &input_string[digits - (max_len - 8)..digits - (max_len - 12)] != "0000" {
        return format!("{}{}{}{}", handle_first_chars(first_chars).replace("いち", "いっ"), digit_range_characters, digits_4(&input_string[digits - (max_len - 4)..digits - (max_len - 8)], true), digits_5_8(&input_string[digits - (max_len - 8)..]));
    }
    return format!("{}{}{}", handle_first_chars(first_chars).replace("いち", "いっ"), digit_range_characters, digits_4_for_slice(&input_string[digits - (max_len - 4)..digits - (max_len - 16)]));
}

fn digits_17_20(input_string: &str) -> String {
    let digits = input_string.len();
    let max_len = 20;
    let first_chars = &input_string[0..digits - (max_len - 4)];
    let digit_range_characters = "けい";

    if &input_string[digits - (max_len - 4)..digits - (max_len - 8)] != "0000" {
        return format!("{}{}{}{}", handle_first_chars(first_chars), digit_range_characters, digits_4(&input_string[digits - (max_len - 4)..digits - (max_len - 8)], true).replace("いち", "いっ"), digits_13_16(&format!("{}{}", "0", &input_string[digits - (max_len - 8)..])));
    }
    if &input_string[digits - (max_len - 8)..digits - (max_len - 12)] != "0000" {
        return format!("{}{}{}{}", handle_first_chars(first_chars), digit_range_characters, digits_4(&input_string[digits - (max_len - 4)..digits - (max_len - 8)], true).replace("いち", "いっ"), digits_9_12(&input_string[digits - (max_len - 8)..]));
    }
    if &input_string[digits - (max_len - 12)..digits - (max_len - 16)] != "0000" {
        return format!("{}{}{}{}", handle_first_chars(first_chars), digit_range_characters, digits_4_for_slice(&input_string[digits - (max_len - 4)..digits - (max_len - 12)]), digits_5_8(&input_string[digits - (max_len - 12)..]));
    }
    return format!("{}{}{}", handle_first_chars(first_chars), digit_range_characters, digits_4_for_slice(&input_string[digits - (max_len - 4)..digits - (max_len - 20)]));
}

fn digits_21_24(input_string: &str) -> String {
    let digits = input_string.len();
    let max_len = 24;
    let first_chars = &input_string[0..digits - (max_len - 4)];
    let digit_range_characters = "がい";

    if &input_string[digits - (max_len - 4)..digits - (max_len - 8)] != "0000" {
        return format!("{}{}{}{}", handle_first_chars(first_chars), digit_range_characters, digits_4(&input_string[digits - (max_len - 4)..digits - (max_len - 8)], true), digits_17_20(&format!("{}{}", "0", &input_string[digits - (max_len - 8)..])));
    }
    if &input_string[digits - (max_len - 8)..digits - (max_len - 12)] != "0000" {
        return format!("{}{}{}{}", handle_first_chars(first_chars), digit_range_characters, digits_4(&input_string[digits - (max_len - 4)..digits - (max_len - 8)], true), digits_13_16(&input_string[digits - (max_len - 8)..]));
    }
    if &input_string[digits - (max_len - 12)..digits - (max_len - 16)] != "0000" {
        return format!("{}{}{}{}", handle_first_chars(first_chars), digit_range_characters, digits_4_for_slice(&input_string[digits - (max_len - 4)..digits - (max_len - 12)]), digits_9_12(&input_string[digits - (max_len - 12)..]));
    }
    if &input_string[digits - (max_len - 16)..digits - (max_len - 20)] != "0000" {
        return format!("{}{}{}{}", handle_first_chars(first_chars), digit_range_characters, digits_4_for_slice(&input_string[digits - (max_len - 4)..digits - (max_len - 16)]), digits_5_8(&input_string[digits - (max_len - 16)..]));
    }
    return format!("{}{}{}", handle_first_chars(first_chars), digit_range_characters, digits_4_for_slice(&input_string[digits - (max_len - 4)..digits - (max_len - 24)]));
}

fn digits_25_28(input_string: &str) -> String {
    let digits = input_string.len();
    let max_len = 28;
    let first_chars = &input_string[0..digits - (max_len - 4)];
    let digit_range_characters = "じょ";

    if &input_string[digits - (max_len - 4)..digits - (max_len - 8)] != "0000" {
        return format!("{}{}{}{}", handle_first_chars(first_chars), digit_range_characters, digits_4(&input_string[digits - (max_len - 4)..digits - (max_len - 8)], true), digits_21_24(&format!("{}{}", "0", &input_string[digits - (max_len - 8)..])));
    }
    if &input_string[digits - (max_len - 8)..digits - (max_len - 12)] != "0000" {
        return format!("{}{}{}{}", handle_first_chars(first_chars), digit_range_characters, digits_4(&input_string[digits - (max_len - 4)..digits - (max_len - 8)], true), digits_17_20(&input_string[digits - (max_len - 8)..]));
    }
    if &input_string[digits - (max_len - 12)..digits - (max_len - 16)] != "0000" {
        return format!("{}{}{}{}", handle_first_chars(first_chars), digit_range_characters, digits_4_for_slice(&input_string[digits - (max_len - 4)..digits - (max_len - 12)]), digits_13_16(&input_string[digits - (max_len - 12)..]));
    }
    if &input_string[digits - (max_len - 16)..digits - (max_len - 20)] != "0000" {
        return format!("{}{}{}{}", handle_first_chars(first_chars), digit_range_characters, digits_4_for_slice(&input_string[digits - (max_len - 4)..digits - (max_len - 16)]), digits_9_12(&input_string[digits - (max_len - 16)..]));
    }
    if &input_string[digits - (max_len - 20)..digits - (max_len - 24)] != "0000" {
        return format!("{}{}{}{}", handle_first_chars(first_chars), digit_range_characters, digits_4_for_slice(&input_string[digits - (max_len - 4)..digits - (max_len - 20)]), digits_5_8(&input_string[digits - (max_len - 20)..]));
    }
    return format!("{}{}{}", handle_first_chars(first_chars), digit_range_characters, digits_4_for_slice(&input_string[digits - (max_len - 4)..digits - (max_len - 28)]));
}

fn digits_29_32(input_string: &str) -> String {
    let digits = input_string.len();
    let max_len = 32;
    let first_chars = &input_string[0..digits - (max_len - 4)];
    let digit_range_characters = "じょう";

    if &input_string[digits - (max_len - 4)..digits - (max_len - 8)] != "0000" {
        return format!("{}{}{}{}", handle_first_chars(first_chars), digit_range_characters, digits_4(&input_string[digits - (max_len - 4)..digits - (max_len - 8)], true), digits_25_28(&format!("{}{}", "0", &input_string[digits - (max_len - 8)..])));
    }
    if &input_string[digits - (max_len - 8)..digits - (max_len - 12)] != "0000" {
        return format!("{}{}{}{}", handle_first_chars(first_chars), digit_range_characters, digits_4(&input_string[digits - (max_len - 4)..digits - (max_len - 8)], true), digits_21_24(&input_string[digits - (max_len - 8)..]));
    }
    if &input_string[digits - (max_len - 12)..digits - (max_len - 16)] != "0000" {
        return format!("{}{}{}{}", handle_first_chars(first_chars), digit_range_characters, digits_4_for_slice(&input_string[digits - (max_len - 4)..digits - (max_len - 12)]), digits_17_20(&input_string[digits - (max_len - 12)..]));
    }
    if &input_string[digits - (max_len - 16)..digits - (max_len - 20)] != "0000" {
        return format!("{}{}{}{}", handle_first_chars(first_chars), digit_range_characters, digits_4_for_slice(&input_string[digits - (max_len - 4)..digits - (max_len - 16)]), digits_13_16(&input_string[digits - (max_len - 16)..]));
    }
    if &input_string[digits - (max_len - 20)..digits - (max_len - 24)] != "0000" {
        return format!("{}{}{}{}", handle_first_chars(first_chars), digit_range_characters, digits_4_for_slice(&input_string[digits - (max_len - 4)..digits - (max_len - 20)]), digits_9_12(&input_string[digits - (max_len - 20)..]));
    }
    if &input_string[digits - (max_len - 24)..digits - (max_len - 28)] != "0000" {
        return format!("{}{}{}{}", handle_first_chars(first_chars), digit_range_characters, digits_4_for_slice(&input_string[digits - (max_len - 4)..digits - (max_len - 24)]), digits_5_8(&input_string[digits - (max_len - 24)..]));
    }
    return format!("{}{}{}", handle_first_chars(first_chars), digit_range_characters, digits_4_for_slice(&input_string[digits - (max_len - 4)..digits - (max_len - 32)]));
}

fn digits_33_36(input_string: &str) -> String {
    let digits = input_string.len();
    let max_len = 36;
    let first_chars = &input_string[0..digits - (max_len - 4)];
    let digit_range_characters = "こう";

    if &input_string[digits - (max_len - 4)..digits - (max_len - 8)] != "0000" {
        return format!("{}{}{}{}", handle_first_chars(first_chars), digit_range_characters, digits_4(&input_string[digits - (max_len - 4)..digits - (max_len - 8)], true), digits_29_32(&format!("{}{}", "0", &input_string[digits - (max_len - 8)..])));
    }
    if &input_string[digits - (max_len - 8)..digits - (max_len - 12)] != "0000" {
        return format!("{}{}{}{}", handle_first_chars(first_chars), digit_range_characters, digits_4(&input_string[digits - (max_len - 4)..digits - (max_len - 8)], true), digits_25_28(&input_string[digits - (max_len - 8)..]));
    }
    if &input_string[digits - (max_len - 12)..digits - (max_len - 16)] != "0000" {
        return format!("{}{}{}{}", handle_first_chars(first_chars), digit_range_characters, digits_4_for_slice(&input_string[digits - (max_len - 4)..digits - (max_len - 12)]), digits_21_24(&input_string[digits - (max_len - 12)..]));
    }
    if &input_string[digits - (max_len - 16)..digits - (max_len - 20)] != "0000" {
        return format!("{}{}{}{}", handle_first_chars(first_chars), digit_range_characters, digits_4_for_slice(&input_string[digits - (max_len - 4)..digits - (max_len - 16)]), digits_17_20(&input_string[digits - (max_len - 16)..]));
    }
    if &input_string[digits - (max_len - 20)..digits - (max_len - 24)] != "0000" {
        return format!("{}{}{}{}", handle_first_chars(first_chars), digit_range_characters, digits_4_for_slice(&input_string[digits - (max_len - 4)..digits - (max_len - 20)]), digits_13_16(&input_string[digits - (max_len - 20)..]));
    }
    if &input_string[digits - (max_len - 24)..digits - (max_len - 28)] != "0000" {
        return format!("{}{}{}{}", handle_first_chars(first_chars), digit_range_characters, digits_4_for_slice(&input_string[digits - (max_len - 4)..digits - (max_len - 24)]), digits_9_12(&input_string[digits - (max_len - 24)..]));
    }
    if &input_string[digits - (max_len - 28)..digits - (max_len - 32)] != "0000" {
        return format!("{}{}{}{}", handle_first_chars(first_chars), digit_range_characters, digits_4_for_slice(&input_string[digits - (max_len - 4)..digits - (max_len - 28)]), digits_5_8(&input_string[digits - (max_len - 28)..]));
    }
    return format!("{}{}{}", handle_first_chars(first_chars), digit_range_characters, digits_4_for_slice(&input_string[digits - (max_len - 4)..digits - (max_len - 36)]));
}

fn digits_37_40(input_string: &str) -> String {
    let digits = input_string.len();
    let max_len = 40;
    let first_chars = &input_string[0..digits - (max_len - 4)];
    let digit_range_characters = "かん";

    if &input_string[digits - (max_len - 4)..digits - (max_len - 8)] != "0000" {
        return format!("{}{}{}{}", handle_first_chars(first_chars), digit_range_characters, digits_4(&input_string[digits - (max_len - 4)..digits - (max_len - 8)], true), digits_33_36(&format!("{}{}", "0", &input_string[digits - (max_len - 8)..])));
    }
    if &input_string[digits - (max_len - 8)..digits - (max_len - 12)] != "0000" {
        return format!("{}{}{}{}", handle_first_chars(first_chars), digit_range_characters, digits_4(&input_string[digits - (max_len - 4)..digits - (max_len - 8)], true), digits_29_32(&input_string[digits - (max_len - 8)..]));
    }
    if &input_string[digits - (max_len - 12)..digits - (max_len - 16)] != "0000" {
        return format!("{}{}{}{}", handle_first_chars(first_chars), digit_range_characters, digits_4_for_slice(&input_string[digits - (max_len - 4)..digits - (max_len - 12)]), digits_25_28(&input_string[digits - (max_len - 12)..]));
    }
    if &input_string[digits - (max_len - 16)..digits - (max_len - 20)] != "0000" {
        return format!("{}{}{}{}", handle_first_chars(first_chars), digit_range_characters, digits_4_for_slice(&input_string[digits - (max_len - 4)..digits - (max_len - 16)]), digits_21_24(&input_string[digits - (max_len - 16)..]));
    }
    if &input_string[digits - (max_len - 20)..digits - (max_len - 24)] != "0000" {
        return format!("{}{}{}{}", handle_first_chars(first_chars), digit_range_characters, digits_4_for_slice(&input_string[digits - (max_len - 4)..digits - (max_len - 20)]), digits_17_20(&input_string[digits - (max_len - 20)..]));
    }
    if &input_string[digits - (max_len - 24)..digits - (max_len - 28)] != "0000" {
        return format!("{}{}{}{}", handle_first_chars(first_chars), digit_range_characters, digits_4_for_slice(&input_string[digits - (max_len - 4)..digits - (max_len - 24)]), digits_13_16(&input_string[digits - (max_len - 24)..]));
    }
    if &input_string[digits - (max_len - 28)..digits - (max_len - 32)] != "0000" {
        return format!("{}{}{}{}", handle_first_chars(first_chars), digit_range_characters, digits_4_for_slice(&input_string[digits - (max_len - 4)..digits - (max_len - 28)]), digits_9_12(&input_string[digits - (max_len - 28)..]));
    }
    if &input_string[digits - (max_len - 32)..digits - (max_len - 36)] != "0000" {
        return format!("{}{}{}{}", handle_first_chars(first_chars), digit_range_characters, digits_4_for_slice(&input_string[digits - (max_len - 4)..digits - (max_len - 32)]), digits_5_8(&input_string[digits - (max_len - 32)..]));
    }
    return format!("{}{}{}", handle_first_chars(first_chars), digit_range_characters, digits_4_for_slice(&input_string[digits - (max_len - 4)..digits - (max_len - 40)]));
}

fn digits_41_44(input_string: &str) -> String {
    let digits = input_string.len();
    let max_len = 44;
    let first_chars = &input_string[0..digits - (max_len - 4)];
    let digit_range_characters = "せい";

    if &input_string[digits - (max_len - 4)..digits - (max_len - 8)] != "0000" {
        return format!("{}{}{}{}", handle_first_chars(first_chars), digit_range_characters, digits_4(&input_string[digits - (max_len - 4)..digits - (max_len - 8)], true), digits_37_40(&format!("{}{}", "0", &input_string[digits - (max_len - 8)..])));
    }
    if &input_string[digits - (max_len - 8)..digits - (max_len - 12)] != "0000" {
        return format!("{}{}{}{}", handle_first_chars(first_chars), digit_range_characters, digits_4(&input_string[digits - (max_len - 4)..digits - (max_len - 8)], true), digits_33_36(&input_string[digits - (max_len - 8)..]));
    }
    if &input_string[digits - (max_len - 12)..digits - (max_len - 16)] != "0000" {
        return format!("{}{}{}{}", handle_first_chars(first_chars), digit_range_characters, digits_4_for_slice(&input_string[digits - (max_len - 4)..digits - (max_len - 12)]), digits_29_32(&input_string[digits - (max_len - 12)..]));
    }
    if &input_string[digits - (max_len - 16)..digits - (max_len - 20)] != "0000" {
        return format!("{}{}{}{}", handle_first_chars(first_chars), digit_range_characters, digits_4_for_slice(&input_string[digits - (max_len - 4)..digits - (max_len - 16)]), digits_25_28(&input_string[digits - (max_len - 16)..]));
    }
    if &input_string[digits - (max_len - 20)..digits - (max_len - 24)] != "0000" {
        return format!("{}{}{}{}", handle_first_chars(first_chars), digit_range_characters, digits_4_for_slice(&input_string[digits - (max_len - 4)..digits - (max_len - 20)]), digits_21_24(&input_string[digits - (max_len - 20)..]));
    }
    if &input_string[digits - (max_len - 24)..digits - (max_len - 28)] != "0000" {
        return format!("{}{}{}{}", handle_first_chars(first_chars), digit_range_characters, digits_4_for_slice(&input_string[digits - (max_len - 4)..digits - (max_len - 24)]), digits_17_20(&input_string[digits - (max_len - 24)..]));
    }
    if &input_string[digits - (max_len - 28)..digits - (max_len - 32)] != "0000" {
        return format!("{}{}{}{}", handle_first_chars(first_chars), digit_range_characters, digits_4_for_slice(&input_string[digits - (max_len - 4)..digits - (max_len - 28)]), digits_13_16(&input_string[digits - (max_len - 28)..]));
    }
    if &input_string[digits - (max_len - 32)..digits - (max_len - 36)] != "0000" {
        return format!("{}{}{}{}", handle_first_chars(first_chars), digit_range_characters, digits_4_for_slice(&input_string[digits - (max_len - 4)..digits - (max_len - 32)]), digits_9_12(&input_string[digits - (max_len - 32)..]));
    }
    if &input_string[digits - (max_len - 36)..digits - (max_len - 40)] != "0000" {
        return format!("{}{}{}{}", handle_first_chars(first_chars), digit_range_characters, digits_4_for_slice(&input_string[digits - (max_len - 4)..digits - (max_len - 36)]), digits_5_8(&input_string[digits - (max_len - 32)..]));
    }
    return format!("{}{}{}", handle_first_chars(first_chars), digit_range_characters, digits_4_for_slice(&input_string[digits - (max_len - 4)..digits - (max_len - 44)]));
}

fn digits_45_48(input_string: &str) -> String {
    let digits = input_string.len();
    let max_len = 48;
    let first_chars = &input_string[0..digits - (max_len - 4)];
    let digit_range_characters = "さい";

    if &input_string[digits - (max_len - 4)..digits - (max_len - 8)] != "0000" {
        return format!("{}{}{}{}", handle_first_chars(first_chars), digit_range_characters, digits_4(&input_string[digits - (max_len - 4)..digits - (max_len - 8)], true), digits_41_44(&format!("{}{}", "0", &input_string[digits - (max_len - 8)..])));
    }
    if &input_string[digits - (max_len - 8)..digits - (max_len - 12)] != "0000" {
        return format!("{}{}{}{}", handle_first_chars(first_chars), digit_range_characters, digits_4(&input_string[digits - (max_len - 4)..digits - (max_len - 8)], true), digits_37_40(&input_string[digits - (max_len - 8)..]));
    }
    if &input_string[digits - (max_len - 12)..digits - (max_len - 16)] != "0000" {
        return format!("{}{}{}{}", handle_first_chars(first_chars), digit_range_characters, digits_4_for_slice(&input_string[digits - (max_len - 4)..digits - (max_len - 12)]), digits_33_36(&input_string[digits - (max_len - 12)..]));
    }
    if &input_string[digits - (max_len - 16)..digits - (max_len - 20)] != "0000" {
        return format!("{}{}{}{}", handle_first_chars(first_chars), digit_range_characters, digits_4_for_slice(&input_string[digits - (max_len - 4)..digits - (max_len - 16)]), digits_29_32(&input_string[digits - (max_len - 16)..]));
    }
    if &input_string[digits - (max_len - 20)..digits - (max_len - 24)] != "0000" {
        return format!("{}{}{}{}", handle_first_chars(first_chars), digit_range_characters, digits_4_for_slice(&input_string[digits - (max_len - 4)..digits - (max_len - 20)]), digits_25_28(&input_string[digits - (max_len - 20)..]));
    }
    if &input_string[digits - (max_len - 24)..digits - (max_len - 28)] != "0000" {
        return format!("{}{}{}{}", handle_first_chars(first_chars), digit_range_characters, digits_4_for_slice(&input_string[digits - (max_len - 4)..digits - (max_len - 24)]), digits_21_24(&input_string[digits - (max_len - 24)..]));
    }
    if &input_string[digits - (max_len - 28)..digits - (max_len - 32)] != "0000" {
        return format!("{}{}{}{}", handle_first_chars(first_chars), digit_range_characters, digits_4_for_slice(&input_string[digits - (max_len - 4)..digits - (max_len - 28)]), digits_17_20(&input_string[digits - (max_len - 28)..]));
    }
    if &input_string[digits - (max_len - 32)..digits - (max_len - 36)] != "0000" {
        return format!("{}{}{}{}", handle_first_chars(first_chars), digit_range_characters, digits_4_for_slice(&input_string[digits - (max_len - 4)..digits - (max_len - 32)]), digits_13_16(&input_string[digits - (max_len - 32)..]));
    }
    if &input_string[digits - (max_len - 36)..digits - (max_len - 40)] != "0000" {
        return format!("{}{}{}{}", handle_first_chars(first_chars), digit_range_characters, digits_4_for_slice(&input_string[digits - (max_len - 4)..digits - (max_len - 36)]), digits_9_12(&input_string[digits - (max_len - 32)..]));
    }
    if &input_string[digits - (max_len - 40)..digits - (max_len - 44)] != "0000" {
        return format!("{}{}{}{}", handle_first_chars(first_chars), digit_range_characters, digits_4_for_slice(&input_string[digits - (max_len - 4)..digits - (max_len - 40)]), digits_5_8(&input_string[digits - (max_len - 32)..]));
    }
    return format!("{}{}{}", handle_first_chars(first_chars), digit_range_characters, digits_4_for_slice(&input_string[digits - (max_len - 4)..digits - (max_len - 48)]));
}

fn digits_49_52(input_string: &str) -> String {
    let digits = input_string.len();
    let max_len = 52;
    let first_chars = &input_string[0..digits - (max_len - 4)];
    let digit_range_characters = "ごく";

    if &input_string[digits - (max_len - 4)..digits - (max_len - 8)] != "0000" {
        return format!("{}{}{}{}", handle_first_chars(first_chars), digit_range_characters, digits_4(&input_string[digits - (max_len - 4)..digits - (max_len - 8)], true), digits_45_48(&format!("{}{}", "0", &input_string[digits - (max_len - 8)..])));
    }
    if &input_string[digits - (max_len - 8)..digits - (max_len - 12)] != "0000" {
        return format!("{}{}{}{}", handle_first_chars(first_chars), digit_range_characters, digits_4(&input_string[digits - (max_len - 4)..digits - (max_len - 8)], true), digits_41_44(&input_string[digits - (max_len - 8)..]));
    }
    if &input_string[digits - (max_len - 12)..digits - (max_len - 16)] != "0000" {
        return format!("{}{}{}{}", handle_first_chars(first_chars), digit_range_characters, digits_4_for_slice(&input_string[digits - (max_len - 4)..digits - (max_len - 12)]), digits_37_40(&input_string[digits - (max_len - 12)..]));
    }
    if &input_string[digits - (max_len - 16)..digits - (max_len - 20)] != "0000" {
        return format!("{}{}{}{}", handle_first_chars(first_chars), digit_range_characters, digits_4_for_slice(&input_string[digits - (max_len - 4)..digits - (max_len - 16)]), digits_33_36(&input_string[digits - (max_len - 16)..]));
    }
    if &input_string[digits - (max_len - 20)..digits - (max_len - 24)] != "0000" {
        return format!("{}{}{}{}", handle_first_chars(first_chars), digit_range_characters, digits_4_for_slice(&input_string[digits - (max_len - 4)..digits - (max_len - 20)]), digits_29_32(&input_string[digits - (max_len - 20)..]));
    }
    if &input_string[digits - (max_len - 24)..digits - (max_len - 28)] != "0000" {
        return format!("{}{}{}{}", handle_first_chars(first_chars), digit_range_characters, digits_4_for_slice(&input_string[digits - (max_len - 4)..digits - (max_len - 24)]), digits_25_28(&input_string[digits - (max_len - 24)..]));
    }
    if &input_string[digits - (max_len - 28)..digits - (max_len - 32)] != "0000" {
        return format!("{}{}{}{}", handle_first_chars(first_chars), digit_range_characters, digits_4_for_slice(&input_string[digits - (max_len - 4)..digits - (max_len - 28)]), digits_21_24(&input_string[digits - (max_len - 28)..]));
    }
    if &input_string[digits - (max_len - 32)..digits - (max_len - 36)] != "0000" {
        return format!("{}{}{}{}", handle_first_chars(first_chars), digit_range_characters, digits_4_for_slice(&input_string[digits - (max_len - 4)..digits - (max_len - 32)]), digits_17_20(&input_string[digits - (max_len - 32)..]));
    }
    if &input_string[digits - (max_len - 36)..digits - (max_len - 40)] != "0000" {
        return format!("{}{}{}{}", handle_first_chars(first_chars), digit_range_characters, digits_4_for_slice(&input_string[digits - (max_len - 4)..digits - (max_len - 36)]), digits_13_16(&input_string[digits - (max_len - 32)..]));
    }
    if &input_string[digits - (max_len - 40)..digits - (max_len - 44)] != "0000" {
        return format!("{}{}{}{}", handle_first_chars(first_chars), digit_range_characters, digits_4_for_slice(&input_string[digits - (max_len - 4)..digits - (max_len - 40)]), digits_9_12(&input_string[digits - (max_len - 32)..]));
    }
    if &input_string[digits - (max_len - 44)..digits - (max_len - 48)] != "0000" {
        return format!("{}{}{}{}", handle_first_chars(first_chars), digit_range_characters, digits_4_for_slice(&input_string[digits - (max_len - 4)..digits - (max_len - 44)]), digits_5_8(&input_string[digits - (max_len - 32)..]));
    }
    return format!("{}{}{}", handle_first_chars(first_chars), digit_range_characters, digits_4_for_slice(&input_string[digits - (max_len - 4)..digits - (max_len - 12)]));
}
