use serenity::all::Context;
use serenity::builder::CreateCommand;
use serenity::model::application::ResolvedOption;

use crate::load_data;

pub async fn run(_options: &Vec<ResolvedOption<'_>>, ctx: &Context) -> String {
    let mut errors = vec![];

    let free_badge_data = {
        let data_read = ctx.data.read().await;
        data_read
            .get::<crate::FreeBadgeData>()
            .expect("Expected CommandCounter in TypeMap.")
            .clone()
    };

    {
        match load_data::safe_read_lines("fun_command_file.txt") {
            Some(some) => {
                let mut free_badge_data_guard = free_badge_data.write().await;
                *free_badge_data_guard = some;
            }
            None => errors.push("Free badge file read failed"),
        }
    }

    let fun_command_data = {
        let data_read = ctx.data.read().await;
        data_read
            .get::<crate::FunCommandData>()
            .expect("Expected CommandCounter in TypeMap.")
            .clone()
    };

    {
        match load_data::safe_read_lines("fun_command_file.txt") {
            Some(some) => {
                let mut fun_command_data_guard = fun_command_data.write().await;
                *fun_command_data_guard = some;
            }
            None => errors.push("Fun command file read failed"),
        }
    }

    if errors.len() == 0 {
        return "Successfully reloaded data".to_string();
    }
    return format!("Reload failed: {}", errors.join(", "));
}

pub fn register() -> CreateCommand {
    CreateCommand::new("reload")
}
