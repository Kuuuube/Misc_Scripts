use std::str::FromStr;
use std::time::Instant;
use bigdecimal::BigDecimal;

use crate::utils::{clean_decimal_string, clean_f64_to_string, bigdecimal_powf};
use crate::{Settings, StepType};
use crate::{banknote_daiji, daiji, hiragana, kanji};

pub fn generation_mode(settings: Settings) {
    if settings.precise {
        precise_generation_mode(settings);
    } else {
        float_generation_mode(settings);
    }
}

fn float_generation_mode(settings: Settings) {
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

pub fn precise_generation_mode(settings: Settings) {
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
    let start_number = settings.range.0.parse::<BigDecimal>().unwrap_or_default();
    let end_number = settings.range.1.parse::<BigDecimal>().unwrap_or_default();
    let bigdecimal_step = BigDecimal::from_str(&settings.raw_step).unwrap_or_default();
    let mut i = start_number;

    while i < &end_number + &bigdecimal_step {
        let input_string = clean_decimal_string(format!("{}", i));
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
            StepType::Add => i += &bigdecimal_step,
            StepType::Multiply => i = (i * &bigdecimal_step).round(settings.round),
            StepType::Exponent => i = bigdecimal_powf(i, &bigdecimal_step).round(settings.round)
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