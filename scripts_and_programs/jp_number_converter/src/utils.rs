use bigdecimal::{BigDecimal, Zero, FromPrimitive};
use std::str::FromStr;

pub fn bigdecimal_powf(x: BigDecimal, e: &BigDecimal) -> BigDecimal {
    let exponent_string = format!("{}", e);
    let split: Vec<&str> = exponent_string.split(".").collect();
    let split_whole = split.get(0).unwrap();
    let split_decimal = split.get(1).unwrap_or(&"");

    let whole_value = BigDecimal::from_str(split_whole).unwrap();

    if split_decimal.len() >= 1 {
        let numerator = split_whole.parse::<u32>().unwrap() * u32::pow(10, split_decimal.len() as u32) + split_decimal.parse::<u32>().unwrap();
        let denominator = u32::pow(10, split_decimal.len() as u32);
        
        let gcd = euclid_gcd(numerator, denominator);
        let simplified_numerator = numerator / gcd;
        let simplified_denominator = denominator / gcd;

        let whole_result = bigdecimal_powi(&x.round(50), &BigDecimal::from_u32(simplified_numerator).unwrap()).round(50);
        let result = bigdecimal_root(BigDecimal::from_u32(simplified_denominator).unwrap(), whole_result.clone());
        return result;
    } else {
        return bigdecimal_powi(&x, &whole_value);
    }
}

fn euclid_gcd(mut m: u32, mut n: u32) -> u32 {
    while m != 0 {
        let old_m = m;
        m = n % m;
        n = old_m;
    }
    return n
 }

pub fn bigdecimal_powi(x: &BigDecimal, e: &BigDecimal) -> BigDecimal {
    let mut r = BigDecimal::from_str("1").unwrap();
    let mut i = BigDecimal::zero();
    while i < *e {
        r *= x.clone();
        i += 1;
    }
    return r;
}

pub fn bigdecimal_root(n: BigDecimal, x: BigDecimal) -> BigDecimal {
    let mut d: BigDecimal;
    let mut r = BigDecimal::from_str("1").unwrap();
    if x == BigDecimal::zero() {
        return BigDecimal::zero();
    }
    if n < BigDecimal::from_str("1").unwrap() {
        return BigDecimal::zero(); //NaN
    }
    loop {
        r = r.with_prec(50); //looping with round is too expensive, with_prec must be used
        d = (&x / bigdecimal_powi(&r, &(&n - &1)) - &r) / &n;
        r += &d;
        if !(&d >= &(BigDecimal::from_f64(f64::EPSILON).unwrap() * 10) || &d <= &(BigDecimal::from_f64(-f64::EPSILON).unwrap() * 10)) {
            break;
        }
    }
    return r;
}

pub fn clean_decimal_string(input_string: String) -> String {
    if input_string.contains(".") {
        return input_string.trim_end_matches("0").trim_end_matches(".").to_string();
    }
    return input_string;
}

pub fn clean_f64_to_string(float: f64, rounding: usize) -> String {
    let rounded = format!("{:.rounding$}", float);
    if rounded.contains(".") {
        return rounded.trim_end_matches("0").trim_end_matches(".").to_string();
    }
    return rounded;
}

pub fn safe_parse_f64(input: String) -> f64 {
    return input.parse::<f64>().unwrap_or_default();
}

pub fn read_line() -> String {
    std::io::Write::flush(&mut std::io::stdout()).unwrap();
    let mut buffer = String::default();
    std::io::stdin().read_line(&mut buffer).unwrap_or_default();
    return buffer;
}