pub fn safe_read_lines(filename: &str) -> Option<Vec<String>> {
    let file = std::fs::read_to_string(filename).ok()?;
    return Some(file.split("\n").map(|x| x.to_owned()).collect());
}
