use bevy::prelude::*;

use super::*;

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub(crate) enum TokenColor {
    Red,
    Yellow,
    Black,
}

pub(crate) struct Winner(pub TokenColor);

pub(crate) enum GameTurn {
    Player1,
    Player2,
}

pub(crate) struct GameBoard([[Entity; 6]; 7]);

/// Between 0 and 6, included
pub(crate) struct ClickedColumnEvent(u8);

pub(crate) struct ShotCounter(u32);

/// unique component
pub(crate) struct Board;

pub(crate) fn init_game(mut commands: Commands, mat: Res<Materials>) {
    // Board
    commands
        .spawn_bundle(SpriteBundle {
            material: mat.blue.clone(),
            sprite: Sprite::new(Vec2::new(40.0 * 7.0, 40.0 * 6.0)),
            ..Default::default()
        })
        .insert(Board);

    // Tokens
    let mut board = [[Entity::new(0); 6]; 7];
    let origin = Vec3::new(-40.0 * 3.0, -40.0 * 2.5, 0.0);
    for i in 0..7 {
        for j in 0..6 {
            let id = commands
                .spawn_bundle(SpriteBundle {
                    material: mat.black.clone(),
                    sprite: Sprite::new(Vec2::new(35.0, 35.0)),
                    transform: Transform::from_translation(
                        Vec3::new((i * 40) as f32, (j * 40) as f32, 1.0) + origin,
                    ),
                    ..Default::default()
                })
                .insert(TokenColor::Black)
                .id();
            board[i][j] = id;
        }
    }
    commands.insert_resource(GameBoard(board));

    commands.insert_resource(GameTurn::Player1);
    commands.insert_resource(ShotCounter(0));
    commands.insert_resource(Winner(TokenColor::Black));
}

pub(crate) fn deinit_game(
    mut commands: Commands,
    q: Query<Entity, With<Board>>,
    board: Res<GameBoard>,
) {
    let board_entity = q.single().unwrap();
    commands.entity(board_entity).despawn();

    for i in 0..7 {
        for j in 0..6 {
            commands.entity(board.0[i][j]).despawn();
        }
    }
}

pub(crate) fn handle_click_game(
    mouse: Res<Input<MouseButton>>,
    window: Res<Windows>,
    mut evt: EventWriter<ClickedColumnEvent>,
) {
    if mouse.just_pressed(MouseButton::Left) {
        let _ = window
            .get_primary()
            .and_then(Window::cursor_position)
            .map(|pos| (pos.x / 40.0).clamp(0.0, 6.0) as u8)
            .map(|column| evt.send(ClickedColumnEvent(column)));
    }
}

pub(crate) fn shot_played(
    mut evt_queue: EventReader<ClickedColumnEvent>,
    mut board: ResMut<GameBoard>,
    mut turn: ResMut<GameTurn>,
    mut q: Query<(&mut TokenColor, &mut Handle<ColorMaterial>)>,
    materials: Res<Materials>,
    mut counter: ResMut<ShotCounter>,
    mut state: ResMut<State<AppState>>,
    mut winner: ResMut<Winner>,
) {
    evt_queue.iter().try_for_each(|evt| {
        let i = evt.0 as usize;
        board.0[i]
            .iter_mut()
            .enumerate()
            .find(|(_, cell)| {
                let (color, _) = q.get_mut(**cell).unwrap();
                *color == TokenColor::Black
            })
            .map(|(j, cell)| {
                counter.0 += 1;
                let (mut color, mut mat) = q.get_mut(*cell).unwrap();
                match *turn {
                    GameTurn::Player1 => {
                        *color = TokenColor::Red;
                        *mat = materials.red.clone();
                    }
                    GameTurn::Player2 => {
                        *color = TokenColor::Yellow;
                        *mat = materials.yellow.clone();
                    }
                }
                (j, *color)
            })
            .and_then(|(j, color)| {
                // TODO: /!\ ça check seulement les alignements dont la dernière case jouée est une extrémité, problème !
                /// Check if contains 4 consecutive times the given color
                fn check<I: IntoIterator<Item = (Option<usize>, Option<usize>)>>(
                    t: &GameBoard,
                    coords: I,
                    color: TokenColor,
                    q: &mut Query<(&mut TokenColor, &mut Handle<ColorMaterial>)>,
                ) -> bool {
                    let mut res = 0;
                    for (i, j) in coords {
                        let i = match i {
                            Some(i) => i,
                            None => {
                                res = 0;
                                continue;
                            }
                        };
                        let j = match j {
                            Some(j) => j,
                            None => {
                                res = 0;
                                continue;
                            }
                        };

                        if *q.get_component_mut::<TokenColor>(t.0[i][j]).unwrap() == color {
                            res += 1;
                        } else {
                            res = 0;
                        }

                        if res == 4 {
                            return true;
                        }
                    }
                    return false;
                }

                let pos = IVec2::new(i as _, j as _);
                let directions = [
                    IVec2::new(1, 0),
                    IVec2::new(0, 1),
                    IVec2::new(1, 1),
                    IVec2::new(-1, 1),
                ];

                if directions.iter().any(|&dir| {
                    let it = std::iter::successors(Some(-3 * dir), |&v| Some(v + dir))
                        .map(|vec| {
                            let (x, y) = (pos + vec).into();
                            let check = |a: i32, bounds: std::ops::Range<i32>| -> Option<usize> {
                                bounds.contains(&a).then(|| a as usize)
                            };
                            (check(x, 0..7), check(y, 0..6))
                        })
                        .take(7);
                    check(&board, it, color, &mut q)
                }) {
                    end_game(color, &mut state, &mut winner);
                    return None;
                } else if counter.0 >= 7 * 6 {
                    end_game(TokenColor::Black, &mut state, &mut winner);
                    return None;
                }

                *turn = match color {
                    TokenColor::Red => GameTurn::Player2,
                    TokenColor::Yellow => GameTurn::Player1,
                    TokenColor::Black => unreachable!(),
                };
                Some(())
            })
    });
}

fn end_game(color: TokenColor, state: &mut State<AppState>, winner: &mut Winner) {
    let _ = state.set(AppState::EndOfGame);
    winner.0 = color;
}
