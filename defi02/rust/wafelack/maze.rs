use std::time::{SystemTime, UNIX_EPOCH};

pub struct MtGenerator {
    seed: f64,
    j: f64,
    k: f64,
    period: u64,
}

impl MtGenerator {
    pub fn new(seed: i32) -> Self {
        Self {
            seed: seed as f64,
            j: 2f64.powi(31) - 1.,
            k: 16807.,
            period: 2u64.pow(30),
        }
    }
    pub fn gen_number(&mut self, min: i32, max: i32) -> i32 {
        self.seed = (self.k * self.seed) % self.j;
        let toret = (max as f64 - min as f64 + 1.) * (self.seed / self.j) + min as f64;
        self.period -= 1;
        if self.period == 0 {
            self.period = 2u64.pow(30)
        }
        toret.ceil() as i32
    }
}

pub const LEFT: usize = 0;
pub const RIGHT: usize = 2;
pub const TOP: usize = 1;
pub const BOT: usize = 3;

fn gen_map(w: usize, h: usize) -> String {
    let mut generator = MtGenerator::new(SystemTime::now().duration_since(UNIX_EPOCH).unwrap().as_secs() as i32);
    let mut map = vec![vec![1 ; w] ; h];

    let side = generator.gen_number(0, 3);

    let (mut x, mut y, mut direction) = match side {
        0 => (generator.gen_number(1, w as i32 - 2) as usize, 0usize, BOT),
        1 => (w - 1, generator.gen_number(1, h as i32 - 2) as usize, LEFT),
        3 => (0usize, generator.gen_number(1, h  as i32 - 2) as usize, RIGHT),
        2 => (generator.gen_number(1, w as i32 - 2) as usize, h - 1, TOP),
        _ => panic!("Should not be called."),
    };

    for _ in 0..200 {
        match direction {
            BOT => {
                if y + 2 < h - 1 {
                    map[y][x] = 0;
                    y += 1;
                    map[y][x] = 0;
                    y += 1;

                }
            }
            TOP => {
                if y as i32 - 2 >= 1 {
                    map[y][x] = 0;
                    y -= 1;
                    map[y][x] = 0;
                    y -= 1;

                }
            }
            LEFT => {
                if x as i32 - 2 >= 1 {
                    map[y][x] = 0;
                    x -= 1;
                    map[y][x] = 0;
                    x -= 1;

                }
            }
            RIGHT => {
                if x + 2 < w - 1 {
                    map[y][x] = 0;
                    x += 1;
                    map[y][x] = 0;
                    x += 1;

                }
            }
            _ => {}
        }
        let prev_dir = direction;

        let mut gen_dir = || {
            let tmp = generator.gen_number(0, 29);

            if tmp < 10 {
                LEFT
            } else if tmp < 20 {
                RIGHT
            } else if tmp < 25 {
                TOP
            } else {
                BOT
            }
        };

        direction = gen_dir();

        while direction == prev_dir {
            direction = gen_dir();
        }
    }

    map[y][x] = 2;

    let mut to_ret = String::new();
                              
    for y in 0..h {
        for x in 0..w {
            if map[y][x] == 2 {
                to_ret.push('@');
            } else if map[y][x] == 1 {
                to_ret.push('█');
            } else {
                to_ret.push(' ');
            }
        } 
        to_ret.push('\n');
    }                              
                              
    to_ret
}

#[cfg(test)]
mod test {
  use super::*;
                              
  #[test]
  fn gen() {
    let width = 22;
    let height = 14;

    let map = gen_map(width, height);

    println!("{}", map);
  }
}
