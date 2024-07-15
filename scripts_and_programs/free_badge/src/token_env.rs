pub fn get_dotenv_token() -> Option<String> {
    let dotenv_file = std::fs::read_to_string(".env").ok()?;
    for line in dotenv_file.split("\n") {
        let (key, value) = match line.split_once("=") {
            Some(some) => some,
            None => continue,
        };
        if key.trim() == "TOKEN" {
            return Some(value.trim().to_string());
        }
    }

    return None;
}

pub fn get_env_token() -> Option<String> {
    return std::env::var("FREE_BADGE_BOT_DISCORD_TOKEN").ok();
}
