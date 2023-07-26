use crate::Settings;
use crate::{banknote_daiji, daiji, hiragana, kanji};

pub fn interactive_mode(settings: Settings) {
    let hiragana_convert = settings.format_string.contains("{hiragana}");
    let kanji_convert = settings.format_string.contains("{kanji}");
    let banknote_daiji_convert = settings.format_string.contains("{banknote_daiji}");
    let daiji_convert = settings.format_string.contains("{daiji}");

    println!("Interactive Mode");

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
        print!("{}", settings.format_string.replace("{arabic}", &input_string).replace("{hiragana}", &hiragana_output).replace("{kanji}", &kanji_output).replace("{banknote_daiji}", &banknote_daiji_output).replace("{daiji}", &daiji_output).replace("\\n", "\n"));
    }
}

fn read_line() -> String {
    let mut buffer = String::default();
    std::io::stdin().read_line(&mut buffer).unwrap_or_default();
    return buffer;
}