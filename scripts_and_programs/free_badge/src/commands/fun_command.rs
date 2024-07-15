use rand::seq::SliceRandom;
use serenity::all::Context;
use serenity::builder::CreateCommand;
use serenity::model::application::ResolvedOption;

pub async fn run(_options: &Vec<ResolvedOption<'_>>, ctx: &Context) -> String {
    let data_read = ctx.data.read().await;

    let command_counter_lock = data_read
        .get::<crate::FunCommandData>()
        .expect("Expected FunCommandData in TypeMap.")
        .clone();

    let command_counter = command_counter_lock.read().await;
    match command_counter.choose(&mut rand::thread_rng()) {
        Some(some) => return some.to_string(),
        None => return "Failed to fetch details".to_string(),
    }
}

pub fn register() -> CreateCommand {
    CreateCommand::new("fun_command")
}
