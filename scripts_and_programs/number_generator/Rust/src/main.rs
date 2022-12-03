use std::fs;
use std::io::Write;
use std::io;
use std::time::Instant;

fn main() {
    let stdin = io::stdin();
    
    let mut start_number_str: String = Default::default();
    println!("Start Number: ");
    match stdin.read_line(&mut start_number_str) {
        Ok(_) => (),
        Err(_) => println!("Failed to read input.")
    };
    let mut start_number: i128 = match start_number_str.trim().parse::<i128>() {
        Ok(parsed) => parsed,
        Err(error) => parsing_error(error)
    };

    let mut end_number_str: String = Default::default();
    println!("End Number: ");
    match stdin.read_line(&mut end_number_str) {
        Ok(_) => (),
        Err(_) => println!("Failed to read input.")
    };
    let end_number: i128 = match end_number_str.trim().parse::<i128>() {
        Ok(parsed) => parsed,
        Err(error) => parsing_error(error)
    };

    let mut number_prefix: String = Default::default();
    println!("Number Prefix: ");
    match stdin.read_line(&mut number_prefix) {
        Ok(_) => (),
        Err(_) => println!("Failed to read input.")
    };
    number_prefix = number_prefix.replace("\n", "").replace("\r", "");

    let mut number_suffix: String = Default::default();
    println!("Number Suffix: ");
    match stdin.read_line(&mut number_suffix) {
        Ok(_) => (),
        Err(_) => println!("Failed to read input.")
    };

    //benchmarking code
    let start_time = Instant::now();

    let file_path = "output.txt";
    let mut output_file = match fs::OpenOptions::new().create(true).write(true).truncate(true).open(file_path) {
        Ok(file_ok) => file_ok,
        Err(_err) => panic!("Couldn't open file.")
    };

    while start_number <= end_number {
        match output_file.write(format!("{number_prefix}{start_number}{number_suffix}").as_bytes()) {
            Ok(_) => (),
            Err(_) => println!("Failed to write file.")
        };

        start_number += 1;
    }

    //benchmarking code
    let time_elapsed = start_time.elapsed();
    println!("Generated in: {time_elapsed:.6?}");
}

fn parsing_error(input_error: std::num::ParseIntError) -> i128 {
    println!("{input_error}");
    return 0;
}