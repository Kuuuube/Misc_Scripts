use std::collections::HashMap;

pub fn greek_prefix(number_str: String) -> String {
let number_int: u32 = number_str.parse().unwrap();
let mut number_string: String = Default::default();
    if number_int <= 2 {
        number_string += &dict_0_2(number_int);
    } else if number_int <= 19 {
        for (i, character) in number_str.chars().rev().enumerate() {
            let current_value: u32 = character.to_digit(10).unwrap() * 10_u32.pow(i as u32);
            number_string += &dict_3_19(current_value);
        }
    } else {
        for (i, character) in number_str.chars().rev().enumerate() {
            let current_value: u32 = character.to_digit(10).unwrap() * 10_u32.pow(i as u32);
            number_string = dict_20_mil(current_value) + &number_string;
        }
    }
    return number_string;
}

fn dict_0_2(input_num: u32) -> String {
    let dict: HashMap<u32, &str> = HashMap::from([
        (0, "miden"),
        (1, "mono"),
        (2, "di")
    ]);
    return dict.get(&input_num).unwrap().to_string();
}

fn dict_3_19(input_num: u32) -> String {
    let dict: HashMap<u32, &str> = HashMap::from([
        (0, ""),
        (1, "hen"),
        (2, "do"),
        (3, "tri"),
        (4, "tetra"),
        (5, "penta"),
        (6, "hexa"),
        (7, "hepta"),
        (8, "octo"),
        (9, "ennea"),
        (10, "deca")
    ]);
    return dict.get(&input_num).unwrap().to_string();
}

fn dict_20_mil(input_num: u32) -> String {
    let dict: HashMap<u32, &str> = HashMap::from([
        (0, ""),
        (1, "hen"),
        (2, "do"),
        (3, "tri"),
        (4, "tetra"),
        (5, "penta"),
        (6, "hexa"),
        (7, "hepta"),
        (8, "octo"),
        (9, "ennea"),
        (10, "deca"),
        (20, "icosi"),
        (30, "triconta"),
        (40, "tetraconta"),
        (50, "pentaconta"),
        (60, "hexaconta"),
        (70, "heptaconta"),
        (80, "octaconta"),
        (90, "enneaconta"),
        (100, "hecato"),
        (200, "diacosia"),
        (300, "tricosia"),
        (400, "tetracosia"),
        (500, "pentacosia"),
        (600, "hexacosia"),
        (700, "heptacosia"),
        (800, "octocosia"),
        (900, "enneacosia"),
        (1000, "chili"),
        (2000, "dischili"),
        (3000, "trischili"),
        (4000, "tetraischili"),
        (5000, "pentaischili"),
        (6000, "hexaischili"),
        (7000, "heptaischili"),
        (8000, "octoischili"),
        (9000, "enneaischili"),
        (10000, "myria"),
        (20000, "dicismyria"),
        (30000, "tricismyria"),
        (40000, "tetracismyria"),
        (50000, "pentacismyria"),
        (60000, "hexacismyria"),
        (70000, "heptacismyria"),
        (80000, "octocismyria"),
        (90000, "enneacismyria"),
        (100000, "decakismyria"),
        (200000, "dodecakismyria"),
        (300000, "tridecakismyria"),
        (400000, "tetradecakismyria"),
        (500000, "pentadecakismyria"),
        (600000, "hexadecakismyria"),
        (700000, "heptadecakismyria"),
        (800000, "octodecakismyria"),
        (900000, "enneadecakismyria"),
        (1000000, "hecatomyria")
    ]);
    return dict.get(&input_num).unwrap().to_string();
}