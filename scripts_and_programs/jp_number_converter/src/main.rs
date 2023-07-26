use std::time::Instant;
use rand::Rng;

use args_parser::{Settings, Mode, StepType};

mod tests;

mod hiragana;
mod kanji;
mod banknote_daiji;
mod daiji;
mod args_parser;

fn main() {
    let settings = match args_parser::parse_args() {
        Some(some) => some,
        None => return
    };

    match settings.mode {
        Mode::Interactive => interactive_mode(settings),
        Mode::Generation => generation_mode(settings),
        Mode::Guessing => guessing_mode(settings)
    }
}

fn read_line() -> String {
    let mut buffer = String::default();
    std::io::stdin().read_line(&mut buffer).unwrap_or_default();
    return buffer;
}

fn clean_f64_to_string(float: f64, rounding: usize) -> String {
    let rounded = format!("{:.rounding$}", float);
    if rounded.contains(".") {
        return rounded.trim_end_matches("0").trim_end_matches(".").to_string();
    }
    return rounded;
}

fn safe_parse_f64(input: String) -> f64 {
    return input.parse::<f64>().unwrap_or_default();
}

fn interactive_mode(settings: Settings) {
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
        println!("{}", settings.format_string.replace("{arabic}", &input_string).replace("{hiragana}", &hiragana_output).replace("{kanji}", &kanji_output).replace("{banknote_daiji}", &banknote_daiji_output).replace("{daiji}", &daiji_output).replace("\\n", "\n"));
    }
}

fn generation_mode(settings: Settings) {
    let hiragana_convert = settings.format_string.contains("{hiragana}");
    let kanji_convert = settings.format_string.contains("{kanji}");
    let banknote_daiji_convert = settings.format_string.contains("{banknote_daiji}");
    let daiji_convert = settings.format_string.contains("{daiji}");

    println!("Generation Mode");

    //benchmarking code
    let start_time = Instant::now();

    let mut output_file = match std::fs::OpenOptions::new().create(true).write(true).truncate(true).open(&settings.output) {
        Ok(ok) => ok,
        Err(_) => panic!("Couldn't open file.")
    };

    let mut generated_string = String::default();
    let start_number = settings.range.0.parse::<f64>().unwrap_or_default();
    let end_number = settings.range.1.parse::<f64>().unwrap_or_default();
    let mut i = start_number;

    while i < end_number + settings.step {
        let input_string = clean_f64_to_string(i, settings.step_decimal_len);
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
        generated_string += &format!("{}", settings.format_string.replace("{arabic}", &input_string).replace("{hiragana}", &hiragana_output).replace("{kanji}", &kanji_output).replace("{banknote_daiji}", &banknote_daiji_output).replace("{daiji}", &daiji_output).replace("\\n", "\n"));

        match settings.step_type {
            StepType::Add => i += settings.step,
            StepType::Multiply => i *= settings.step,
            StepType::Exponent => i = f64::powf(i, settings.step)
        }
    }

    match std::io::Write::write(&mut output_file, generated_string.as_bytes()) {
        Ok(_) => (),
        Err(_) => println!("Failed to write file.")
    };

    //benchmarking code
    let time_elapsed = start_time.elapsed();
    println!("Generated in: {time_elapsed:.6?}");
}

fn guessing_mode(settings: Settings) {
    let hiragana_convert = settings.format_string.contains("{hiragana}");
    let kanji_convert = settings.format_string.contains("{kanji}");
    let banknote_daiji_convert = settings.format_string.contains("{banknote_daiji}");
    let daiji_convert = settings.format_string.contains("{daiji}");

    println!("Guessing Mode");

    loop {
        let input_string;

        if settings.weight {
            let range = settings.range.clone();
            let digit_start = range.0.len();
            let digits_end = range.1.len();

            let digit = rand::thread_rng().gen_range(digit_start..=digits_end);

            let mut minimum_number = 1.0;
            let mut maximum_number = 9.0;
            for _ in [0..digit] {
                minimum_number = minimum_number * 10.0;
                maximum_number = maximum_number * 10.0 + 9.0;
            }

            let number = rand::thread_rng().gen_range(minimum_number..=maximum_number);
            input_string = clean_f64_to_string(number, settings.max_decimal);
        } else {
            let range = settings.range.clone();
            let number = rand::thread_rng().gen_range(safe_parse_f64(range.0)..=safe_parse_f64(range.1));
            input_string = clean_f64_to_string(number, settings.max_decimal)
        }

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
        println!("{}", settings.format_string.replace("{arabic}", &input_string).replace("{hiragana}", &hiragana_output).replace("{kanji}", &kanji_output).replace("{banknote_daiji}", &banknote_daiji_output).replace("{daiji}", &daiji_output).replace("\\n", "\n"));

        println!("Input: ");
        let user_input = read_line().trim().to_string();
        if vec![input_string.as_str(), hiragana_output.as_str(), kanji_output.as_str(), banknote_daiji_output.as_str(), daiji_output.as_str()].contains(&user_input.as_str()) {
            println!("Correct!")
        } else {
            println!("Incorrect. Arabic Numeral: {}", &input_string);
        }
    }
}