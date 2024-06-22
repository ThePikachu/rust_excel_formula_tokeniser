
mod enums;
mod token;
mod tokens;
mod token_stack;
mod tokeniser;

use tokeniser::tokenize;

fn main() {
    println!("Hello, world!");
    // let tokens = tokenize(r#"IF(SUM(IF(FOO = BAR, 10, 0), 10 ) = 20 , "FOO", "BAR")"#);
    let tokens = tokenize(r#"IF(C4>0.05, "High Interest", "Low Interest") & " in Year " & A4"#);
    println!("{:?}", tokens.items);

    // for token in tokens.items {
    // println!("{:?}", token);
    // }
}
