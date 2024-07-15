use serenity::all::{
    Command, CreateInteractionResponse, CreateInteractionResponseMessage, Interaction,
};
use serenity::async_trait;
use serenity::model::gateway::Ready;
use serenity::prelude::*;
use std::sync::Arc;

mod commands;
mod load_data;
mod token_env;

struct FreeBadgeData;
impl TypeMapKey for FreeBadgeData {
    type Value = Arc<RwLock<Vec<String>>>;
}

struct FunCommandData;
impl TypeMapKey for FunCommandData {
    type Value = Arc<RwLock<Vec<String>>>;
}

struct Handler;

#[async_trait]
impl EventHandler for Handler {
    async fn interaction_create(&self, ctx: Context, interaction: Interaction) {
        if let Interaction::Command(command) = interaction {
            println!("Received command interaction: {command:?}");

            let content = match command.data.name.as_str() {
                "free_badge" => {
                    Some(commands::free_badge::run(&command.data.options(), &ctx).await)
                }
                "fun_command" => {
                    Some(commands::fun_command::run(&command.data.options(), &ctx).await)
                }
                "reload" if is_owner_id(command.user.id.to_string(), &ctx).await => {
                    Some(commands::reload::run(&command.data.options(), &ctx).await)
                }
                _ => None,
            };

            if let Some(content) = content {
                let data = CreateInteractionResponseMessage::new()
                    .ephemeral(true)
                    .content(content);
                let builder = CreateInteractionResponse::Message(data);
                if let Err(why) = command.create_response(&ctx.http, builder).await {
                    println!("Cannot respond to slash command: {why}");
                }
            }
        }
    }

    async fn ready(&self, ctx: Context, ready: Ready) {
        println!("{} is connected!", ready.user.name);

        let free_badge_command =
            Command::create_global_command(&ctx.http, commands::free_badge::register()).await;
        let fun_command_command =
            Command::create_global_command(&ctx.http, commands::fun_command::register()).await;
        let reload_command = Command::create_global_command(
            &ctx.http,
            commands::reload::register().default_member_permissions(
                serenity::model::permissions::Permissions::ADMINISTRATOR,
            ),
        )
        .await;

        println!("I created the following global slash commands: {free_badge_command:?} {fun_command_command:?} {reload_command:?}");
    }
}

async fn is_owner_id(input_id: String, ctx: &Context) -> bool {
    let owner_id = match ctx.http.get_current_application_info().await {
        Ok(ok) => match ok.owner {
            Some(some) => some.id.to_string(),
            None => return false,
        },
        Err(_) => return false,
    };
    return input_id == owner_id;
}

#[tokio::main]
async fn main() {
    let token = token_env::get_dotenv_token()
        .ok_or_else(|| token_env::get_env_token())
        .expect("Failed to get token from .env or `FREE_BADGE_BOT_DISCORD_TOKEN` env variable");

    let mut client = Client::builder(&token, GatewayIntents::default())
        .event_handler(Handler)
        .await
        .expect("Err creating client");

    {
        let mut data = client.data.write().await;
        data.insert::<FreeBadgeData>(Arc::new(RwLock::new(
            load_data::safe_read_lines("free_badge_file.txt")
                .expect("Failed to read free_badge_file.txt"),
        )));
        data.insert::<FunCommandData>(Arc::new(RwLock::new(
            load_data::safe_read_lines("fun_command_file.txt")
                .expect("Failed to read fun_command_file.txt"),
        )));
    }

    if let Err(why) = client.start().await {
        println!("Client error: {why:?}");
    }
}
