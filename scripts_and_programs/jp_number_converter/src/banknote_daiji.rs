use std::collections::HashMap;

pub fn convert_number(input_string: &str) -> String {
    let split_string: Vec<&str> = input_string.split(".").collect();
    if split_string.len() > 1 {
        return format!("{}点{}", whole_number(split_string.get(0).unwrap()), fraction(split_string.get(1).unwrap()));
    }
    return whole_number(input_string);
}

fn whole_number(input_string: &str) -> String {
    let input_len = input_string.len();
    if input_len == 1 {return digits_1(input_string, true)}
    else if input_len == 2 {return digits_2(input_string)}
    else if input_len == 3 {return digits_3(input_string)}
    else if input_len == 4 {return digits_4(input_string, false)}
    else if input_len >= 5 && input_len <= 8 {return digits_5_8(input_string)}
    else if input_len >= 9 && input_len <= 12 {return digits_9_12(input_string)}
    else if input_len >= 13 && input_len <= 16 {return digits_13_16(input_string)}
    else if input_len >= 17 && input_len <= 20 {return digits_17_20(input_string)}
    else if input_len >= 21 && input_len <= 24 {return digits_21_24(input_string)}
    else if input_len >= 25 && input_len <= 28 {return digits_25_28(input_string)}
    else if input_len >= 29 && input_len <= 32 {return digits_29_32(input_string)}
    else if input_len >= 33 && input_len <= 36 {return digits_33_36(input_string)}
    else if input_len >= 37 && input_len <= 40 {return digits_37_40(input_string)}
    else if input_len >= 41 && input_len <= 44 {return digits_41_44(input_string)}
    else if input_len >= 45 && input_len <= 48 {return digits_45_48(input_string)}
    else if input_len >= 49 && input_len <= 52 {return digits_49_52(input_string)}
    else if input_len >= 53 && input_len <= 56 {return digits_53_56(input_string)}
    else if input_len >= 57 && input_len <= 60 {return digits_57_60(input_string)}
    else if input_len >= 61 && input_len <= 64 {return digits_61_64(input_string)}
    else if input_len >= 65 && input_len <= 68 {return digits_65_68(input_string)}
    else if input_len >= 69 && input_len <= 72 {return digits_69_72(input_string)}
    else {return "".to_string()}
}

fn fraction(input_string: &str) -> String {
    return input_string.replace("0", "零").replace("1", "壱").replace("2", "弐").replace("3", "参").replace("4", "四").replace("5", "伍").replace("6", "六").replace("7", "七").replace("8", "八").replace("9", "九").to_string();
}

fn dict_get(input_string: &str, use_one: bool) -> String {
    let dict: HashMap<&str, &str> = HashMap::from([
        ("1", "壱"),
        ("2", "弐"),
        ("3", "参"),
        ("4", "四"),
        ("5", "伍"),
        ("6", "六"),
        ("7", "七"),
        ("8", "八"),
        ("9", "九")
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
        return "零".to_string();
    }
    return dict_get(input_string, true);
}

fn digits_2(input_string: &str) -> String {
    let first_char = &input_string[0..1];
    let second_char = &input_string[1..];
    if first_char != "0" {
        return format!("{}{}{}", dict_get(first_char, false), "拾", digits_1(second_char, false));
    }
    return format!("{}{}", dict_get(first_char, false), digits_1(second_char, false));
}

fn digits_3(input_string: &str) -> String {
    let first_char = &input_string[0..1];
    let remaining_chars = &input_string[1..];
    match first_char {
        "0" => return format!("{}{}", dict_get(first_char, false), digits_2(remaining_chars)),
        _ => return format!("{}{}{}", dict_get(first_char, false), "佰", digits_2(remaining_chars))
    }
}

fn digits_4(input_string: &str, prefix: bool) -> String {
    let first_char = &input_string[0..1];
    let remaining_chars = &input_string[1..];
    if first_char == "0" {
        return format!("{}{}", dict_get(first_char, false), digits_3(remaining_chars));
    }
    if first_char == "1" && prefix {
        return format!("{}{}{}", dict_get(first_char, false), "壱阡", digits_3(remaining_chars))
    }
    return format!("{}{}{}", dict_get(first_char, false), "阡", digits_3(remaining_chars))
}

fn digits_5_8(input_string: &str) -> String {
    let digits = input_string.len();
    let first_chars = &input_string[0..digits - 4];
    let remaining_chars_0 = &input_string[digits - 4..];

    return format!("{}{}{}", handle_first_chars(first_chars), "萬", digits_4(remaining_chars_0, true));
}

fn digits_9_12(input_string: &str) -> String {
    let digits = input_string.len();
    let max_len = 12;
    let digit_range_characters = "億";

    if &input_string[digits - (max_len - 4)..digits - (max_len - 8)] != "0000" {
        return format!("{}{}{}{}{}", handle_first_chars(&input_string[0..digits - (max_len - 4)]), digit_range_characters, digits_4(&input_string[digits - (max_len - 4)..digits - (max_len - 8)], true), "萬", digits_4(&input_string[digits - (max_len - 8)..digits - (max_len - 12)], true));
    }
    return format!("{}{}{}", handle_first_chars(&input_string[0..digits - (max_len - 4)]), digit_range_characters, digits_4_for_slice(&input_string[digits - (max_len - 4)..digits - (max_len - 12)]));
}

fn digits_13_16(input_string: &str) -> String {
    let digits = input_string.len();
    let max_len = 16;
    let first_chars = &input_string[0..digits - (max_len - 4)];
    let digit_range_characters = "兆";

    if &input_string[digits - (max_len - 4)..digits - (max_len - 8)] != "0000" {
        return format!("{}{}{}{}", handle_first_chars(first_chars), digit_range_characters, digits_4(&input_string[digits - (max_len - 4)..digits - (max_len - 8)], true), digits_9_12(&format!("{}{}", "0", &input_string[digits - (max_len - 8)..])));
    }
    if &input_string[digits - (max_len - 8)..digits - (max_len - 12)] != "0000" {
        return format!("{}{}{}{}", handle_first_chars(first_chars), digit_range_characters, digits_4(&input_string[digits - (max_len - 4)..digits - (max_len - 8)], true), digits_5_8(&input_string[digits - (max_len - 8)..]));
    }
    return format!("{}{}{}", handle_first_chars(first_chars), digit_range_characters, digits_4_for_slice(&input_string[digits - (max_len - 4)..digits - (max_len - 16)]));
}

fn digits_17_20(input_string: &str) -> String {
    let digits = input_string.len();
    let max_len = 20;
    let first_chars = &input_string[0..digits - (max_len - 4)];
    let digit_range_characters = "京";

    if &input_string[digits - (max_len - 4)..digits - (max_len - 8)] != "0000" {
        return format!("{}{}{}{}", handle_first_chars(first_chars), digit_range_characters, digits_4(&input_string[digits - (max_len - 4)..digits - (max_len - 8)], true), digits_13_16(&format!("{}{}", "0", &input_string[digits - (max_len - 8)..])));
    }
    if &input_string[digits - (max_len - 8)..digits - (max_len - 12)] != "0000" {
        return format!("{}{}{}{}", handle_first_chars(first_chars), digit_range_characters, digits_4(&input_string[digits - (max_len - 4)..digits - (max_len - 8)], true), digits_9_12(&input_string[digits - (max_len - 8)..]));
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
    let digit_range_characters = "垓";

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
    let digit_range_characters = "𥝱";

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
    let digit_range_characters = "穣";

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
    let digit_range_characters = "溝";

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
    let digit_range_characters = "澗";

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
    let digit_range_characters = "正";

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
        return format!("{}{}{}{}", handle_first_chars(first_chars), digit_range_characters, digits_4_for_slice(&input_string[digits - (max_len - 4)..digits - (max_len - 36)]), digits_5_8(&input_string[digits - (max_len - 36)..]));
    }
    return format!("{}{}{}", handle_first_chars(first_chars), digit_range_characters, digits_4_for_slice(&input_string[digits - (max_len - 4)..digits - (max_len - 44)]));
}

fn digits_45_48(input_string: &str) -> String {
    let digits = input_string.len();
    let max_len = 48;
    let first_chars = &input_string[0..digits - (max_len - 4)];
    let digit_range_characters = "載";

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
        return format!("{}{}{}{}", handle_first_chars(first_chars), digit_range_characters, digits_4_for_slice(&input_string[digits - (max_len - 4)..digits - (max_len - 36)]), digits_9_12(&input_string[digits - (max_len - 36)..]));
    }
    if &input_string[digits - (max_len - 40)..digits - (max_len - 44)] != "0000" {
        return format!("{}{}{}{}", handle_first_chars(first_chars), digit_range_characters, digits_4_for_slice(&input_string[digits - (max_len - 4)..digits - (max_len - 40)]), digits_5_8(&input_string[digits - (max_len - 40)..]));
    }
    return format!("{}{}{}", handle_first_chars(first_chars), digit_range_characters, digits_4_for_slice(&input_string[digits - (max_len - 4)..digits - (max_len - 48)]));
}

fn digits_49_52(input_string: &str) -> String {
    let digits = input_string.len();
    let max_len = 52;
    let first_chars = &input_string[0..digits - (max_len - 4)];
    let digit_range_characters = "極";

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
        return format!("{}{}{}{}", handle_first_chars(first_chars), digit_range_characters, digits_4_for_slice(&input_string[digits - (max_len - 4)..digits - (max_len - 36)]), digits_13_16(&input_string[digits - (max_len - 36)..]));
    }
    if &input_string[digits - (max_len - 40)..digits - (max_len - 44)] != "0000" {
        return format!("{}{}{}{}", handle_first_chars(first_chars), digit_range_characters, digits_4_for_slice(&input_string[digits - (max_len - 4)..digits - (max_len - 40)]), digits_9_12(&input_string[digits - (max_len - 40)..]));
    }
    if &input_string[digits - (max_len - 44)..digits - (max_len - 48)] != "0000" {
        return format!("{}{}{}{}", handle_first_chars(first_chars), digit_range_characters, digits_4_for_slice(&input_string[digits - (max_len - 4)..digits - (max_len - 44)]), digits_5_8(&input_string[digits - (max_len - 44)..]));
    }
    return format!("{}{}{}", handle_first_chars(first_chars), digit_range_characters, digits_4_for_slice(&input_string[digits - (max_len - 4)..digits - (max_len - 52)]));
}

fn digits_53_56(input_string: &str) -> String {
    let digits = input_string.len();
    let max_len = 56;
    let first_chars = &input_string[0..digits - (max_len - 4)];
    let digit_range_characters = "恒河沙";

    if &input_string[digits - (max_len - 4)..digits - (max_len - 8)] != "0000" {
        return format!("{}{}{}{}", handle_first_chars(first_chars), digit_range_characters, digits_4(&input_string[digits - (max_len - 4)..digits - (max_len - 8)], true), digits_49_52(&format!("{}{}", "0", &input_string[digits - (max_len - 8)..])));
    }
    if &input_string[digits - (max_len - 8)..digits - (max_len - 12)] != "0000" {
        return format!("{}{}{}{}", handle_first_chars(first_chars), digit_range_characters, digits_4(&input_string[digits - (max_len - 4)..digits - (max_len - 8)], true), digits_45_48(&input_string[digits - (max_len - 8)..]));
    }
    if &input_string[digits - (max_len - 12)..digits - (max_len - 16)] != "0000" {
        return format!("{}{}{}{}", handle_first_chars(first_chars), digit_range_characters, digits_4_for_slice(&input_string[digits - (max_len - 4)..digits - (max_len - 12)]), digits_41_44(&input_string[digits - (max_len - 12)..]));
    }
    if &input_string[digits - (max_len - 16)..digits - (max_len - 20)] != "0000" {
        return format!("{}{}{}{}", handle_first_chars(first_chars), digit_range_characters, digits_4_for_slice(&input_string[digits - (max_len - 4)..digits - (max_len - 16)]), digits_37_40(&input_string[digits - (max_len - 16)..]));
    }
    if &input_string[digits - (max_len - 20)..digits - (max_len - 24)] != "0000" {
        return format!("{}{}{}{}", handle_first_chars(first_chars), digit_range_characters, digits_4_for_slice(&input_string[digits - (max_len - 4)..digits - (max_len - 20)]), digits_33_36(&input_string[digits - (max_len - 20)..]));
    }
    if &input_string[digits - (max_len - 24)..digits - (max_len - 28)] != "0000" {
        return format!("{}{}{}{}", handle_first_chars(first_chars), digit_range_characters, digits_4_for_slice(&input_string[digits - (max_len - 4)..digits - (max_len - 24)]), digits_29_32(&input_string[digits - (max_len - 24)..]));
    }
    if &input_string[digits - (max_len - 28)..digits - (max_len - 32)] != "0000" {
        return format!("{}{}{}{}", handle_first_chars(first_chars), digit_range_characters, digits_4_for_slice(&input_string[digits - (max_len - 4)..digits - (max_len - 28)]), digits_25_28(&input_string[digits - (max_len - 28)..]));
    }
    if &input_string[digits - (max_len - 32)..digits - (max_len - 36)] != "0000" {
        return format!("{}{}{}{}", handle_first_chars(first_chars), digit_range_characters, digits_4_for_slice(&input_string[digits - (max_len - 4)..digits - (max_len - 32)]), digits_21_24(&input_string[digits - (max_len - 32)..]));
    }
    if &input_string[digits - (max_len - 36)..digits - (max_len - 40)] != "0000" {
        return format!("{}{}{}{}", handle_first_chars(first_chars), digit_range_characters, digits_4_for_slice(&input_string[digits - (max_len - 4)..digits - (max_len - 36)]), digits_17_20(&input_string[digits - (max_len - 36)..]));
    }
    if &input_string[digits - (max_len - 40)..digits - (max_len - 44)] != "0000" {
        return format!("{}{}{}{}", handle_first_chars(first_chars), digit_range_characters, digits_4_for_slice(&input_string[digits - (max_len - 4)..digits - (max_len - 40)]), digits_13_16(&input_string[digits - (max_len - 40)..]));
    }
    if &input_string[digits - (max_len - 44)..digits - (max_len - 48)] != "0000" {
        return format!("{}{}{}{}", handle_first_chars(first_chars), digit_range_characters, digits_4_for_slice(&input_string[digits - (max_len - 4)..digits - (max_len - 44)]), digits_9_12(&input_string[digits - (max_len - 44)..]));
    }
    if &input_string[digits - (max_len - 48)..digits - (max_len - 52)] != "0000" {
        return format!("{}{}{}{}", handle_first_chars(first_chars), digit_range_characters, digits_4_for_slice(&input_string[digits - (max_len - 4)..digits - (max_len - 48)]), digits_5_8(&input_string[digits - (max_len - 48)..]));
    }
    return format!("{}{}{}", handle_first_chars(first_chars), digit_range_characters, digits_4_for_slice(&input_string[digits - (max_len - 4)..digits - (max_len - 56)]));
}

fn digits_57_60(input_string: &str) -> String {
    let digits = input_string.len();
    let max_len = 60;
    let first_chars = &input_string[0..digits - (max_len - 4)];
    let digit_range_characters = "阿僧祇";

    if &input_string[digits - (max_len - 4)..digits - (max_len - 8)] != "0000" {
        return format!("{}{}{}{}", handle_first_chars(first_chars), digit_range_characters, digits_4(&input_string[digits - (max_len - 4)..digits - (max_len - 8)], true), digits_53_56(&format!("{}{}", "0", &input_string[digits - (max_len - 8)..])));
    }
    if &input_string[digits - (max_len - 8)..digits - (max_len - 12)] != "0000" {
        return format!("{}{}{}{}", handle_first_chars(first_chars), digit_range_characters, digits_4(&input_string[digits - (max_len - 4)..digits - (max_len - 8)], true), digits_49_52(&input_string[digits - (max_len - 8)..]));
    }
    if &input_string[digits - (max_len - 12)..digits - (max_len - 16)] != "0000" {
        return format!("{}{}{}{}", handle_first_chars(first_chars), digit_range_characters, digits_4_for_slice(&input_string[digits - (max_len - 4)..digits - (max_len - 12)]), digits_45_48(&input_string[digits - (max_len - 12)..]));
    }
    if &input_string[digits - (max_len - 16)..digits - (max_len - 20)] != "0000" {
        return format!("{}{}{}{}", handle_first_chars(first_chars), digit_range_characters, digits_4_for_slice(&input_string[digits - (max_len - 4)..digits - (max_len - 16)]), digits_41_44(&input_string[digits - (max_len - 16)..]));
    }
    if &input_string[digits - (max_len - 20)..digits - (max_len - 24)] != "0000" {
        return format!("{}{}{}{}", handle_first_chars(first_chars), digit_range_characters, digits_4_for_slice(&input_string[digits - (max_len - 4)..digits - (max_len - 20)]), digits_37_40(&input_string[digits - (max_len - 20)..]));
    }
    if &input_string[digits - (max_len - 24)..digits - (max_len - 28)] != "0000" {
        return format!("{}{}{}{}", handle_first_chars(first_chars), digit_range_characters, digits_4_for_slice(&input_string[digits - (max_len - 4)..digits - (max_len - 24)]), digits_33_36(&input_string[digits - (max_len - 24)..]));
    }
    if &input_string[digits - (max_len - 28)..digits - (max_len - 32)] != "0000" {
        return format!("{}{}{}{}", handle_first_chars(first_chars), digit_range_characters, digits_4_for_slice(&input_string[digits - (max_len - 4)..digits - (max_len - 28)]), digits_29_32(&input_string[digits - (max_len - 28)..]));
    }
    if &input_string[digits - (max_len - 32)..digits - (max_len - 36)] != "0000" {
        return format!("{}{}{}{}", handle_first_chars(first_chars), digit_range_characters, digits_4_for_slice(&input_string[digits - (max_len - 4)..digits - (max_len - 32)]), digits_25_28(&input_string[digits - (max_len - 32)..]));
    }
    if &input_string[digits - (max_len - 36)..digits - (max_len - 40)] != "0000" {
        return format!("{}{}{}{}", handle_first_chars(first_chars), digit_range_characters, digits_4_for_slice(&input_string[digits - (max_len - 4)..digits - (max_len - 36)]), digits_21_24(&input_string[digits - (max_len - 36)..]));
    }
    if &input_string[digits - (max_len - 40)..digits - (max_len - 44)] != "0000" {
        return format!("{}{}{}{}", handle_first_chars(first_chars), digit_range_characters, digits_4_for_slice(&input_string[digits - (max_len - 4)..digits - (max_len - 40)]), digits_17_20(&input_string[digits - (max_len - 40)..]));
    }
    if &input_string[digits - (max_len - 44)..digits - (max_len - 48)] != "0000" {
        return format!("{}{}{}{}", handle_first_chars(first_chars), digit_range_characters, digits_4_for_slice(&input_string[digits - (max_len - 4)..digits - (max_len - 44)]), digits_13_16(&input_string[digits - (max_len - 44)..]));
    }
    if &input_string[digits - (max_len - 48)..digits - (max_len - 52)] != "0000" {
        return format!("{}{}{}{}", handle_first_chars(first_chars), digit_range_characters, digits_4_for_slice(&input_string[digits - (max_len - 4)..digits - (max_len - 48)]), digits_9_12(&input_string[digits - (max_len - 48)..]));
    }
    if &input_string[digits - (max_len - 52)..digits - (max_len - 56)] != "0000" {
        return format!("{}{}{}{}", handle_first_chars(first_chars), digit_range_characters, digits_4_for_slice(&input_string[digits - (max_len - 4)..digits - (max_len - 52)]), digits_5_8(&input_string[digits - (max_len - 52)..]));
    }
    return format!("{}{}{}", handle_first_chars(first_chars), digit_range_characters, digits_4_for_slice(&input_string[digits - (max_len - 4)..digits - (max_len - 60)]));
}

fn digits_61_64(input_string: &str) -> String {
    let digits = input_string.len();
    let max_len = 64;
    let first_chars = &input_string[0..digits - (max_len - 4)];
    let digit_range_characters = "那由他";

    if &input_string[digits - (max_len - 4)..digits - (max_len - 8)] != "0000" {
        return format!("{}{}{}{}", handle_first_chars(first_chars), digit_range_characters, digits_4(&input_string[digits - (max_len - 4)..digits - (max_len - 8)], true), digits_57_60(&format!("{}{}", "0", &input_string[digits - (max_len - 8)..])));
    }
    if &input_string[digits - (max_len - 8)..digits - (max_len - 12)] != "0000" {
        return format!("{}{}{}{}", handle_first_chars(first_chars), digit_range_characters, digits_4(&input_string[digits - (max_len - 4)..digits - (max_len - 8)], true), digits_53_56(&input_string[digits - (max_len - 8)..]));
    }
    if &input_string[digits - (max_len - 12)..digits - (max_len - 16)] != "0000" {
        return format!("{}{}{}{}", handle_first_chars(first_chars), digit_range_characters, digits_4_for_slice(&input_string[digits - (max_len - 4)..digits - (max_len - 12)]), digits_49_52(&input_string[digits - (max_len - 12)..]));
    }
    if &input_string[digits - (max_len - 16)..digits - (max_len - 20)] != "0000" {
        return format!("{}{}{}{}", handle_first_chars(first_chars), digit_range_characters, digits_4_for_slice(&input_string[digits - (max_len - 4)..digits - (max_len - 16)]), digits_45_48(&input_string[digits - (max_len - 16)..]));
    }
    if &input_string[digits - (max_len - 20)..digits - (max_len - 24)] != "0000" {
        return format!("{}{}{}{}", handle_first_chars(first_chars), digit_range_characters, digits_4_for_slice(&input_string[digits - (max_len - 4)..digits - (max_len - 20)]), digits_41_44(&input_string[digits - (max_len - 20)..]));
    }
    if &input_string[digits - (max_len - 24)..digits - (max_len - 28)] != "0000" {
        return format!("{}{}{}{}", handle_first_chars(first_chars), digit_range_characters, digits_4_for_slice(&input_string[digits - (max_len - 4)..digits - (max_len - 24)]), digits_37_40(&input_string[digits - (max_len - 24)..]));
    }
    if &input_string[digits - (max_len - 28)..digits - (max_len - 32)] != "0000" {
        return format!("{}{}{}{}", handle_first_chars(first_chars), digit_range_characters, digits_4_for_slice(&input_string[digits - (max_len - 4)..digits - (max_len - 28)]), digits_33_36(&input_string[digits - (max_len - 28)..]));
    }
    if &input_string[digits - (max_len - 32)..digits - (max_len - 36)] != "0000" {
        return format!("{}{}{}{}", handle_first_chars(first_chars), digit_range_characters, digits_4_for_slice(&input_string[digits - (max_len - 4)..digits - (max_len - 32)]), digits_29_32(&input_string[digits - (max_len - 32)..]));
    }
    if &input_string[digits - (max_len - 36)..digits - (max_len - 40)] != "0000" {
        return format!("{}{}{}{}", handle_first_chars(first_chars), digit_range_characters, digits_4_for_slice(&input_string[digits - (max_len - 4)..digits - (max_len - 36)]), digits_25_28(&input_string[digits - (max_len - 36)..]));
    }
    if &input_string[digits - (max_len - 40)..digits - (max_len - 44)] != "0000" {
        return format!("{}{}{}{}", handle_first_chars(first_chars), digit_range_characters, digits_4_for_slice(&input_string[digits - (max_len - 4)..digits - (max_len - 40)]), digits_21_24(&input_string[digits - (max_len - 40)..]));
    }
    if &input_string[digits - (max_len - 44)..digits - (max_len - 48)] != "0000" {
        return format!("{}{}{}{}", handle_first_chars(first_chars), digit_range_characters, digits_4_for_slice(&input_string[digits - (max_len - 4)..digits - (max_len - 44)]), digits_17_20(&input_string[digits - (max_len - 44)..]));
    }
    if &input_string[digits - (max_len - 48)..digits - (max_len - 52)] != "0000" {
        return format!("{}{}{}{}", handle_first_chars(first_chars), digit_range_characters, digits_4_for_slice(&input_string[digits - (max_len - 4)..digits - (max_len - 48)]), digits_13_16(&input_string[digits - (max_len - 48)..]));
    }
    if &input_string[digits - (max_len - 52)..digits - (max_len - 56)] != "0000" {
        return format!("{}{}{}{}", handle_first_chars(first_chars), digit_range_characters, digits_4_for_slice(&input_string[digits - (max_len - 4)..digits - (max_len - 52)]), digits_9_12(&input_string[digits - (max_len - 52)..]));
    }
    if &input_string[digits - (max_len - 56)..digits - (max_len - 60)] != "0000" {
        return format!("{}{}{}{}", handle_first_chars(first_chars), digit_range_characters, digits_4_for_slice(&input_string[digits - (max_len - 4)..digits - (max_len - 56)]), digits_5_8(&input_string[digits - (max_len - 56)..]));
    }
    return format!("{}{}{}", handle_first_chars(first_chars), digit_range_characters, digits_4_for_slice(&input_string[digits - (max_len - 4)..digits - (max_len - 64)]));
}

fn digits_65_68(input_string: &str) -> String {
    let digits = input_string.len();
    let max_len = 68;
    let first_chars = &input_string[0..digits - (max_len - 4)];
    let digit_range_characters = "不可思議";

    if &input_string[digits - (max_len - 4)..digits - (max_len - 8)] != "0000" {
        return format!("{}{}{}{}", handle_first_chars(first_chars), digit_range_characters, digits_4(&input_string[digits - (max_len - 4)..digits - (max_len - 8)], true), digits_61_64(&format!("{}{}", "0", &input_string[digits - (max_len - 8)..])));
    }
    if &input_string[digits - (max_len - 8)..digits - (max_len - 12)] != "0000" {
        return format!("{}{}{}{}", handle_first_chars(first_chars), digit_range_characters, digits_4(&input_string[digits - (max_len - 4)..digits - (max_len - 8)], true), digits_57_60(&input_string[digits - (max_len - 8)..]));
    }
    if &input_string[digits - (max_len - 12)..digits - (max_len - 16)] != "0000" {
        return format!("{}{}{}{}", handle_first_chars(first_chars), digit_range_characters, digits_4_for_slice(&input_string[digits - (max_len - 4)..digits - (max_len - 12)]), digits_53_56(&input_string[digits - (max_len - 12)..]));
    }
    if &input_string[digits - (max_len - 16)..digits - (max_len - 20)] != "0000" {
        return format!("{}{}{}{}", handle_first_chars(first_chars), digit_range_characters, digits_4_for_slice(&input_string[digits - (max_len - 4)..digits - (max_len - 16)]), digits_49_52(&input_string[digits - (max_len - 16)..]));
    }
    if &input_string[digits - (max_len - 20)..digits - (max_len - 24)] != "0000" {
        return format!("{}{}{}{}", handle_first_chars(first_chars), digit_range_characters, digits_4_for_slice(&input_string[digits - (max_len - 4)..digits - (max_len - 20)]), digits_45_48(&input_string[digits - (max_len - 20)..]));
    }
    if &input_string[digits - (max_len - 24)..digits - (max_len - 28)] != "0000" {
        return format!("{}{}{}{}", handle_first_chars(first_chars), digit_range_characters, digits_4_for_slice(&input_string[digits - (max_len - 4)..digits - (max_len - 24)]), digits_41_44(&input_string[digits - (max_len - 24)..]));
    }
    if &input_string[digits - (max_len - 28)..digits - (max_len - 32)] != "0000" {
        return format!("{}{}{}{}", handle_first_chars(first_chars), digit_range_characters, digits_4_for_slice(&input_string[digits - (max_len - 4)..digits - (max_len - 28)]), digits_37_40(&input_string[digits - (max_len - 28)..]));
    }
    if &input_string[digits - (max_len - 32)..digits - (max_len - 36)] != "0000" {
        return format!("{}{}{}{}", handle_first_chars(first_chars), digit_range_characters, digits_4_for_slice(&input_string[digits - (max_len - 4)..digits - (max_len - 32)]), digits_33_36(&input_string[digits - (max_len - 32)..]));
    }
    if &input_string[digits - (max_len - 36)..digits - (max_len - 40)] != "0000" {
        return format!("{}{}{}{}", handle_first_chars(first_chars), digit_range_characters, digits_4_for_slice(&input_string[digits - (max_len - 4)..digits - (max_len - 36)]), digits_29_32(&input_string[digits - (max_len - 36)..]));
    }
    if &input_string[digits - (max_len - 40)..digits - (max_len - 44)] != "0000" {
        return format!("{}{}{}{}", handle_first_chars(first_chars), digit_range_characters, digits_4_for_slice(&input_string[digits - (max_len - 4)..digits - (max_len - 40)]), digits_25_28(&input_string[digits - (max_len - 40)..]));
    }
    if &input_string[digits - (max_len - 44)..digits - (max_len - 48)] != "0000" {
        return format!("{}{}{}{}", handle_first_chars(first_chars), digit_range_characters, digits_4_for_slice(&input_string[digits - (max_len - 4)..digits - (max_len - 44)]), digits_21_24(&input_string[digits - (max_len - 44)..]));
    }
    if &input_string[digits - (max_len - 48)..digits - (max_len - 52)] != "0000" {
        return format!("{}{}{}{}", handle_first_chars(first_chars), digit_range_characters, digits_4_for_slice(&input_string[digits - (max_len - 4)..digits - (max_len - 48)]), digits_17_20(&input_string[digits - (max_len - 48)..]));
    }
    if &input_string[digits - (max_len - 52)..digits - (max_len - 56)] != "0000" {
        return format!("{}{}{}{}", handle_first_chars(first_chars), digit_range_characters, digits_4_for_slice(&input_string[digits - (max_len - 4)..digits - (max_len - 52)]), digits_13_16(&input_string[digits - (max_len - 52)..]));
    }
    if &input_string[digits - (max_len - 56)..digits - (max_len - 60)] != "0000" {
        return format!("{}{}{}{}", handle_first_chars(first_chars), digit_range_characters, digits_4_for_slice(&input_string[digits - (max_len - 4)..digits - (max_len - 56)]), digits_9_12(&input_string[digits - (max_len - 56)..]));
    }
    if &input_string[digits - (max_len - 60)..digits - (max_len - 64)] != "0000" {
        return format!("{}{}{}{}", handle_first_chars(first_chars), digit_range_characters, digits_4_for_slice(&input_string[digits - (max_len - 4)..digits - (max_len - 60)]), digits_5_8(&input_string[digits - (max_len - 60)..]));
    }
    return format!("{}{}{}", handle_first_chars(first_chars), digit_range_characters, digits_4_for_slice(&input_string[digits - (max_len - 4)..digits - (max_len - 68)]));
}

fn digits_69_72(input_string: &str) -> String {
    let digits = input_string.len();
    let max_len = 72;
    let first_chars = &input_string[0..digits - (max_len - 4)];
    let digit_range_characters = "無量大数";

    if &input_string[digits - (max_len - 4)..digits - (max_len - 8)] != "0000" {
        return format!("{}{}{}{}", handle_first_chars(first_chars), digit_range_characters, digits_4(&input_string[digits - (max_len - 4)..digits - (max_len - 8)], true), digits_65_68(&format!("{}{}", "0", &input_string[digits - (max_len - 8)..])));
    }
    if &input_string[digits - (max_len - 8)..digits - (max_len - 12)] != "0000" {
        return format!("{}{}{}{}", handle_first_chars(first_chars), digit_range_characters, digits_4(&input_string[digits - (max_len - 4)..digits - (max_len - 8)], true), digits_61_64(&input_string[digits - (max_len - 8)..]));
    }
    if &input_string[digits - (max_len - 12)..digits - (max_len - 16)] != "0000" {
        return format!("{}{}{}{}", handle_first_chars(first_chars), digit_range_characters, digits_4_for_slice(&input_string[digits - (max_len - 4)..digits - (max_len - 12)]), digits_57_60(&input_string[digits - (max_len - 12)..]));
    }
    if &input_string[digits - (max_len - 16)..digits - (max_len - 20)] != "0000" {
        return format!("{}{}{}{}", handle_first_chars(first_chars), digit_range_characters, digits_4_for_slice(&input_string[digits - (max_len - 4)..digits - (max_len - 16)]), digits_53_56(&input_string[digits - (max_len - 16)..]));
    }
    if &input_string[digits - (max_len - 20)..digits - (max_len - 24)] != "0000" {
        return format!("{}{}{}{}", handle_first_chars(first_chars), digit_range_characters, digits_4_for_slice(&input_string[digits - (max_len - 4)..digits - (max_len - 20)]), digits_49_52(&input_string[digits - (max_len - 20)..]));
    }
    if &input_string[digits - (max_len - 24)..digits - (max_len - 28)] != "0000" {
        return format!("{}{}{}{}", handle_first_chars(first_chars), digit_range_characters, digits_4_for_slice(&input_string[digits - (max_len - 4)..digits - (max_len - 24)]), digits_45_48(&input_string[digits - (max_len - 24)..]));
    }
    if &input_string[digits - (max_len - 28)..digits - (max_len - 32)] != "0000" {
        return format!("{}{}{}{}", handle_first_chars(first_chars), digit_range_characters, digits_4_for_slice(&input_string[digits - (max_len - 4)..digits - (max_len - 28)]), digits_41_44(&input_string[digits - (max_len - 28)..]));
    }
    if &input_string[digits - (max_len - 32)..digits - (max_len - 36)] != "0000" {
        return format!("{}{}{}{}", handle_first_chars(first_chars), digit_range_characters, digits_4_for_slice(&input_string[digits - (max_len - 4)..digits - (max_len - 32)]), digits_37_40(&input_string[digits - (max_len - 32)..]));
    }
    if &input_string[digits - (max_len - 36)..digits - (max_len - 40)] != "0000" {
        return format!("{}{}{}{}", handle_first_chars(first_chars), digit_range_characters, digits_4_for_slice(&input_string[digits - (max_len - 4)..digits - (max_len - 36)]), digits_33_36(&input_string[digits - (max_len - 36)..]));
    }
    if &input_string[digits - (max_len - 40)..digits - (max_len - 44)] != "0000" {
        return format!("{}{}{}{}", handle_first_chars(first_chars), digit_range_characters, digits_4_for_slice(&input_string[digits - (max_len - 4)..digits - (max_len - 40)]), digits_29_32(&input_string[digits - (max_len - 40)..]));
    }
    if &input_string[digits - (max_len - 44)..digits - (max_len - 48)] != "0000" {
        return format!("{}{}{}{}", handle_first_chars(first_chars), digit_range_characters, digits_4_for_slice(&input_string[digits - (max_len - 4)..digits - (max_len - 44)]), digits_25_28(&input_string[digits - (max_len - 44)..]));
    }
    if &input_string[digits - (max_len - 48)..digits - (max_len - 52)] != "0000" {
        return format!("{}{}{}{}", handle_first_chars(first_chars), digit_range_characters, digits_4_for_slice(&input_string[digits - (max_len - 4)..digits - (max_len - 48)]), digits_21_24(&input_string[digits - (max_len - 48)..]));
    }
    if &input_string[digits - (max_len - 52)..digits - (max_len - 56)] != "0000" {
        return format!("{}{}{}{}", handle_first_chars(first_chars), digit_range_characters, digits_4_for_slice(&input_string[digits - (max_len - 4)..digits - (max_len - 52)]), digits_17_20(&input_string[digits - (max_len - 52)..]));
    }
    if &input_string[digits - (max_len - 56)..digits - (max_len - 60)] != "0000" {
        return format!("{}{}{}{}", handle_first_chars(first_chars), digit_range_characters, digits_4_for_slice(&input_string[digits - (max_len - 4)..digits - (max_len - 56)]), digits_13_16(&input_string[digits - (max_len - 56)..]));
    }
    if &input_string[digits - (max_len - 60)..digits - (max_len - 64)] != "0000" {
        return format!("{}{}{}{}", handle_first_chars(first_chars), digit_range_characters, digits_4_for_slice(&input_string[digits - (max_len - 4)..digits - (max_len - 60)]), digits_9_12(&input_string[digits - (max_len - 60)..]));
    }
    if &input_string[digits - (max_len - 64)..digits - (max_len - 68)] != "0000" {
        return format!("{}{}{}{}", handle_first_chars(first_chars), digit_range_characters, digits_4_for_slice(&input_string[digits - (max_len - 4)..digits - (max_len - 64)]), digits_5_8(&input_string[digits - (max_len - 64)..]));
    }
    return format!("{}{}{}", handle_first_chars(first_chars), digit_range_characters, digits_4_for_slice(&input_string[digits - (max_len - 4)..digits - (max_len - 72)]));
}
