mod tests;
mod conversions;
mod args_parser;
mod modes;
mod utils;

use modes::{interactive, generation, guessing};
use conversions::{banknote_daiji, daiji, hiragana, kanji};
use args_parser::{Settings, Mode, StepType};

fn main() {
    let settings = match args_parser::parse_args() {
        Some(some) => some,
        None => return
    };

    match settings.mode {
        Mode::Interactive => interactive::interactive_mode(settings),
        Mode::Generation => generation::generation_mode(settings),
        Mode::Guessing => guessing::guessing_mode(settings)
    }
}