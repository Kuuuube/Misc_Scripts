pub fn parse_args() -> Option<Settings> {
    let args: Vec<String> = std::env::args().collect();

    let mut settings: Settings = Default::default();

    if args.len() == 1 {
        return Some(settings);
    }

    let mut i: usize = 1;
    while i < args.len() {
        let arg = args.get(i).unwrap_or(&"".to_string()).to_string();
        let split: Vec<&str> = arg.split("=").collect();
        match split[0].to_lowercase().as_str() {
            "--help" => {help_message(); return None},
            "-h" => {help_message(); return None},
            "--mode" => {settings.mode = match safe_get_string(split, 1).as_str() {"generation" => Mode::Generation, "guessing" => Mode::Guessing, _ => Mode::Interactive}},
            "--format" => {settings.format_string = safe_get_string(split, 1)},
            "--range" => {settings.range = parse_range(safe_get_string(split, 1))},
            "--step" => {settings.step = safe_parse_f64(safe_get_string(split.clone(), 1)); if safe_get_string(split.clone(), 1).split(".").collect::<Vec<&str>>().len() == 2 {settings.step_decimal_len = safe_get_string(split, 1).len()}},
            "--step-type" => {settings.step_type = match safe_get_string(split, 1).as_str() {"multiply" => StepType::Multiply, "exponent" => StepType::Exponent, _ => StepType::Add}},
            "--output" => {settings.output = safe_get_string(split, 1)},
            "--weight" => {settings.weight = true},
            "--max-decimal" => {settings.max_decimal = safe_parse_usize(safe_get_string(split.clone(), 1))}
            _ => {unknown_command_message(split[0]); return None}
        }

        i += 1;
    }
    return Some(settings);
}

fn help_message() {
    println!("jp_number_converter\nUsage: jp_number_converter [OPTION]...\n\nModes:\n  --mode=MODE                    (interactive|generation|guessing)\n\nAll Modes:\n  --format=STR                   format string to override default in the following format:\n                                 `Arabic: {{arabic}}, Hiragana: {{hiragana}}, Kanji: {{kanji}}, Banknote-style Daiji: {{banknote_daiji}}, Daiji: {{daiji}}\\n`\n\nInteractive Mode:\n\n\nGeneration Mode:\n  --range=ARGS                   range of numbers in the following format: `1-1000`\n  --step=FLOAT                   number to increment the output by\n  --step-type                    (add|multiply|exponent)\n  --output=FILE                  set output FILE\n\nGuessing Mode:\n  --range=ARGS                   range of numbers in the following format: `1-1000`\n  --weight                       makes all digits within the range equally likely\n  --max-decimal                  the maximum decimal places in generated numbers");
}

fn unknown_command_message(command: &str) {
    println!("jp_number_converter: unknown option {}\nUsage: jp_number_converter [OPTION]...\n\nTry `jp_number_converter --help` for more options.\n", command);
}

fn parse_range(input: String) -> (String, String) {
    let range_vec: Vec<&str> = input.split("-").collect();
    return (range_vec.get(0).unwrap_or(&"").to_string(), range_vec.get(1).unwrap_or(&"").to_string());
}

fn safe_get_string(input: Vec<&str>, index: usize) -> String {
    return input.get(index).unwrap_or(&"").to_string();
}

fn safe_parse_f64(input: String) -> f64 {
    return input.parse::<f64>().unwrap_or_default();
}

fn safe_parse_usize(input: String) -> usize {
    return input.parse::<usize>().unwrap_or_default();
}

pub struct Settings {
    pub mode: Mode,
    pub format_string: String,
    pub range: (String, String),
    pub step: f64,
    pub step_decimal_len: usize,
    pub step_type: StepType,
    pub output: String,
    pub weight: bool,
    pub max_decimal: usize
}

impl Default for Settings {
    fn default() -> Self {
        Settings {
            mode: Mode::Interactive,
            format_string: "Arabic: {arabic}, Hiragana: {hiragana}, Kanji: {kanji}, Banknote-style Daiji: {banknote_daiji}, Daiji: {daiji}\n".to_string(),
            range: ("0".to_string(), "1000".to_string()),
            step: 1.0,
            step_decimal_len: 0,
            step_type: StepType::Add,
            output: "".to_string(),
            weight: false,
            max_decimal: 0
        }
    }
}

#[derive(PartialEq)]
pub enum Mode {
    Interactive,
    Generation,
    Guessing
}

impl Default for Mode {
    fn default() -> Self {Mode::Interactive}
}

#[derive(PartialEq)]
pub enum StepType {
    Add,
    Multiply,
    Exponent
}

impl Default for StepType {
    fn default() -> Self {StepType::Add}
}