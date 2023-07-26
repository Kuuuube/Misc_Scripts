use rand::Rng;

use crate::utils::{clean_f64_to_string, safe_parse_f64, read_line};
use crate::Settings;
use crate::{banknote_daiji, daiji, hiragana, kanji};

pub fn guessing_mode(settings: Settings) {
    println!("Guessing Mode");

    loop {
        let input_string;

        if settings.weight {
            let range = settings.range.clone();
            let digit_start = range.0.len();
            let digits_end = range.1.len();

            let start_float = safe_parse_f64(range.0);
            let end_float = safe_parse_f64(range.1);

            let digit = rand::thread_rng().gen_range(digit_start..=digits_end);

            let mut minimum_number = 1.0;
            let mut maximum_number = 9.0;
            for _ in 1..digit {
                minimum_number = minimum_number * 10.0;
                maximum_number = maximum_number * 10.0 + 9.0;
            }

            if minimum_number < start_float || (start_float == 0.0 && digit == 1) {
                minimum_number = start_float;
            }

            if maximum_number > end_float {
                maximum_number = end_float;
            }

            let number = rand::thread_rng().gen_range(minimum_number..=maximum_number);
            input_string = clean_f64_to_string(number, settings.max_decimal);
        } else {
            let range = settings.range.clone();
            let number = rand::thread_rng().gen_range(safe_parse_f64(range.0)..=safe_parse_f64(range.1));
            input_string = clean_f64_to_string(number, settings.max_decimal)
        }

        let hiragana_output = hiragana::convert_number(&input_string);
        let kanji_output = kanji::convert_number(&input_string);
        let banknote_daiji_output = banknote_daiji::convert_number(&input_string);
        let daiji_output = daiji::convert_number(&input_string);

        print!("{}", settings.format_string.replace("{arabic}", &input_string).replace("{hiragana}", &hiragana_output).replace("{kanji}", &kanji_output).replace("{banknote_daiji}", &banknote_daiji_output).replace("{daiji}", &daiji_output).replace("\\n", "\n"));

        print!("{}", settings.prompt.replace("\\n", "\n"));
        let user_input = read_line().trim().to_string();
        if [input_string.as_str(), hiragana_output.as_str(), kanji_output.as_str(), banknote_daiji_output.as_str(), daiji_output.as_str()].contains(&user_input.as_str()) && &user_input != "" {
            print!("{}", settings.correct.replace("{arabic}", &input_string).replace("{hiragana}", &hiragana_output).replace("{kanji}", &kanji_output).replace("{banknote_daiji}", &banknote_daiji_output).replace("{daiji}", &daiji_output).replace("\\n", "\n"));
        } else {
            print!("{}", settings.incorrect.replace("{arabic}", &input_string).replace("{hiragana}", &hiragana_output).replace("{kanji}", &kanji_output).replace("{banknote_daiji}", &banknote_daiji_output).replace("{daiji}", &daiji_output).replace("\\n", "\n"));
        }
    }
}