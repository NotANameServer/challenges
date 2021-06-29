/* 
 * Add `getopt_rs = "0.1.1"` in the dependencies field of Cargo.toml to use this program.
 *
 * License: WTFPL.
 *
 */

use getopt_rs::getopt;
use std::{
    collections::HashMap,
    env,
    io::{self, Write},
    process::exit,
};

#[derive(Debug, PartialEq)]
enum Token {
    Operator(char),
    Number(f64),
}
#[derive(Debug)]
struct Error(pub String);

const PRECEDENCE: [(char, usize); 5] = [('+', 0), ('-', 0), ('*', 1), ('/', 1), ('^', 2)];

fn print_err(err: Error) -> ! {
    eprintln!("\x1b[0;31mError:\x1b[0m {}", err.0);
    exit(1);
}
fn separate(code: String) -> Vec<Token> {
    let mut i = 0;
    let mut tokens = vec![];
    let chars = code.chars().collect::<Vec<char>>();
    while i < code.len() {
        match chars[i] {
            ' ' | '\n' | '\t' | '\r' => i += 1,
            x if x.is_digit(10) => {
                let start = i;
                i += 1;
                while i < code.len() && code[start..i + 1].parse::<f64>().is_ok() {
                    i += 1;
                }
                tokens.push(Token::Number(code[start..i].parse::<f64>().unwrap()));
                continue;
            }
            c => {
                tokens.push(Token::Operator(c));
                i += 1;
            }
        }
    }
    tokens
}
fn get_precedence(operator: char) -> Result<usize, Error> {
    PRECEDENCE
        .iter()
        .map(|(op, lvl)| if *op == operator { Some(lvl) } else { None })
        .filter(|f| f.is_some())
        .last()
        .map_or(
            Err(Error(format!("Unrecognized operator: `{}`.", operator))),
            |o| Ok(*o.unwrap()),
        )
}
fn compile(code: impl ToString) -> Result<Vec<Token>, Error> {
    let tokens = separate(code.to_string());
    let mut queue = vec![];
    let mut stack = vec![];

    tokens
        .into_iter()
        .map(|s| {
            if let Token::Number(_) = s {
                queue.push(s);
            } else if let Token::Operator(c) = s {
                if c == ')' {
                    let mut found = false;
                    while let Some(c) = stack.pop() {
                        if c == '(' {
                            found = true;
                            break;
                        }
                        queue.push(Token::Operator(c))
                    }
                    if !found {
                        return Err(Error("Mismatched parenthesis.".to_string()));
                    }
                } else if c == '(' {
                    stack.push(c);
                } else {
                    match stack.last() {
                        Some(o) => {
                            if *o != '(' && get_precedence(*o)? > get_precedence(c)? {
                                queue.push(Token::Operator(stack.pop().unwrap()));
                                stack.push(c);
                            } else {
                                stack.push(c);
                            }
                        }
                        None => stack.push(c),
                    }
                }
            }
            Ok(())
        })
        .collect::<Result<(), Error>>()?;
    stack
        .into_iter()
        .rev()
        .for_each(|c| queue.push(Token::Operator(c)));
    Ok(queue)
}
fn pop(stack: &mut Vec<f64>) -> f64 {
    match stack.pop() {
        Some(s) => s,
        None => {
            eprintln!("Stack underflow.");
            exit(1);
        }
    }
}
fn fact(n: i64) -> i64 {
    match n {
        0 => 1,
        n => n * fact(n - 1),
    }
}
fn run(code: &[Token]) -> Result<f64, Error> {
    let mut stack = Vec::with_capacity(256);
    let mut functions: HashMap<char, fn(&mut Vec<f64>)> = HashMap::new();
    functions.insert('+', |stack: &mut Vec<f64>| {
        let rhs = pop(stack);
        let lhs = pop(stack);
        stack.push(lhs + rhs);
    });
    functions.insert('-', |stack: &mut Vec<f64>| {
        let rhs = pop(stack);
        let lhs = pop(stack);
        stack.push(lhs - rhs);
    });
    functions.insert('*', |stack: &mut Vec<f64>| {
        let rhs = pop(stack);
        let lhs = pop(stack);
        stack.push(lhs * rhs);
    });
    functions.insert('/', |stack: &mut Vec<f64>| {
        let rhs = pop(stack);
        let lhs = pop(stack);
        stack.push(lhs / rhs);
    });
    functions.insert('^', |stack: &mut Vec<f64>| {
        let rhs = pop(stack);
        let lhs = pop(stack);
        stack.push(lhs.powf(rhs));
    });
    functions.insert('!', |stack: &mut Vec<f64>| {
        let v = pop(stack).ceil() as i64;
        stack.push(fact(v) as f64);
    });

    code.into_iter()
        .map(|p| {
            match p {
                Token::Number(x) => stack.push(*x),
                Token::Operator(op) => {
                    if functions.contains_key(op) {
                        functions[op](&mut stack);
                    } else {
                        return Err(Error(format!("Unrecognised operator: `{}`.", op)));
                    }
                }
            }
            Ok(())
        })
        .collect::<Result<(), Error>>()?;
    Ok(stack.last().and_then(|f| Some(*f)).unwrap_or(0.))
}
fn repl(postfix: bool) {
    loop {
        let mut buffer = String::new();
        print!("$ ");
        io::stdout().flush().unwrap();
        io::stdin().read_line(&mut buffer).unwrap();
        let compiled = if postfix {
            separate(buffer)
        } else {
            match compile(buffer.trim()) {
                Ok(c) => c,
                Err(e) => print_err(e),
            }
        };
        match run(&compiled) {
            Ok(f) => println!("{}", f),
            Err(e) => print_err(e),
        }
    }
}
fn main() {
    let mut args = env::args().collect::<Vec<String>>();
    let mut expression = None;
    let mut postfix = false;
    while let Some(opt) = getopt(&mut args, "e:p", &[]) {
        match opt {
            ('e', val) => expression = val,
            ('p', _) => postfix = true,
            _ => exit(1),
        }
    }
    if let Some(s) = expression {
        let tokens = if postfix {
            separate(s)
        } else {
            match compile(s) {
                Ok(t) => t,
                Err(e) => print_err(e),
            }
        };
        match run(&tokens) {
            Ok(f) => println!("{}", f),
            Err(e) => print_err(e),
        }
    } else {
        repl(postfix);
    }
}
#[cfg(test)]
mod test {
    use super::*;

    #[test]
    fn comp() -> Result<(), Error> {
        let comp = compile("5 * (10 + 2) - 2")?;
        assert_eq!(
            comp,
            vec![
                Token::Number(5.),
                Token::Number(10.),
                Token::Number(2.),
                Token::Operator('+'),
                Token::Operator('*'),
                Token::Number(2.),
                Token::Operator('-')
            ]
        );
        Ok(())
    }
}
