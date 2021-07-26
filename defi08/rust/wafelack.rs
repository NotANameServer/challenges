use std::{
    io::{self, Read, Write},
    process::exit,
};

const HEIGHT: usize = 6;
const WIDTH: usize = 7;
const FIRST_COL: char = 'A';

fn read_char(counter: usize) -> Option<char> {
    print!("\r{}> ", if counter % 2 == 0 { "X" } else { "O" });
    io::stdout().flush().unwrap();
    io::stdin()
        .bytes()
        .next()
        .and_then(|v| v.map_or(None, |v| Some(v as char)))
}
fn move_(
    bitboard: &mut [u64; 2],
    moves: &mut Vec<usize>,
    height: &mut [usize; WIDTH],
    counter: &mut usize,
    col: usize,
) {
    let mov = 1 << height[col];
    bitboard[*counter & 1] ^= mov;
    moves.push(col);
    *counter += 1;
    height[col] += 1;
}
fn won(board: u64) -> bool {
    for direction in [1, 7, 6, 8] {
        let board = board & (board >> direction);
        if board & (board >> (2 * direction)) != 0 {
            return true;
        }
    }
    false
}
fn play_turn(
    bitboard: &mut [u64; 2],
    moves: &mut Vec<usize>,
    height: &mut [usize; WIDTH],
    counter: &mut usize,
) {
    while let Some(c) = read_char(*counter) {
        match c as isize - FIRST_COL as isize {
            idx if idx < WIDTH as isize && idx >= 0 => {
                let idx = idx as usize;
                if height[idx] >= idx * 8 + HEIGHT {
                    continue;
                }
                move_(bitboard, moves, height, counter, idx);
                return;
            }
            _ => {}
        }
    }
    exit(0);
}
fn draw(bitboard: [u64; 2]) {
    print!("\x1B[2J\x1B[1;1H");
    for y in 0..WIDTH as u8 {
        print!("{} ", (FIRST_COL as u8 + y) as char);
    }
    println!();
    for y in (0..HEIGHT).rev() {
        for x in 0..WIDTH {
            let idx = y + x * 7;
            let x = (bitboard[0] >> idx) & 1;
            let o = (bitboard[1] >> idx) & 1;
            print!(
                "{}",
                match (x, o) {
                    (0, 0) => ". ",
                    (1, 0) => "X ",
                    _ => "O ",
                }
            );
        }
        println!();
    }
}
fn main() {
    let mut counter = 0;
    let mut bitboard = [0; 2];
    let mut moves = vec![];
    let mut height = [0, 7, 14, 21, 28, 35, 42];
    loop {
        draw(bitboard);
        if won(bitboard[0]) {
            println!("X won in {} moves.", counter / 2);
            break;
        } else if won(bitboard[1]) {
            println!("O won in {} moves.", counter / 2);
            break;
        }
        play_turn(&mut bitboard, &mut moves, &mut height, &mut counter);
    }
}
