mod tests;

mod hiragana;
mod kanji;
mod banknote_daiji;
mod daiji;

fn main() {
    println!("Format String (optional): ");
    let read_format_string = read_line().trim().to_string();
    let format_string = if read_format_string.len() == 0 {
        "Hiragana: {hiragana}\nKanji: {kanji}\nBanknote-style Daiji: {banknote_daiji}\nDaiji: {daiji}".to_string()
    } else {
        read_format_string
    };
    let hiragana_convert = format_string.contains("{hiragana}");
    let kanji_convert = format_string.contains("{kanji}");
    let banknote_daiji_convert = format_string.contains("{banknote_daiji}");
    let daiji_convert = format_string.contains("{daiji}");

    loop {
        println!("Input: ");
        let input_string = read_line().trim().to_string();
        let hiragana_output = if hiragana_convert {
            hiragana::convert_number(&input_string)
        } else {
            "".to_string()
        };
        let kanji_output = if kanji_convert {
            kanji::convert_number(&input_string)
        } else {
            "".to_string()
        };
        let banknote_daiji_output = if banknote_daiji_convert {
            banknote_daiji::convert_number(&input_string)
        } else {
            "".to_string()
        };
        let daiji_output = if daiji_convert {
            daiji::convert_number(&input_string)
        } else {
            "".to_string()
        };
        println!("{}", format_string.replace("{hiragana}", &hiragana_output).replace("{kanji}", &kanji_output).replace("{banknote_daiji}", &banknote_daiji_output).replace("{daiji}", &daiji_output));
    }
}

fn read_line() -> String {
    let mut buffer = String::default();
    std::io::stdin().read_line(&mut buffer).unwrap_or_default();
    return buffer;
}
