use bevy::{app::AppExit, prelude::*};

use super::*;

pub(crate) struct WinnerText;
pub(crate) struct ExitButton;
pub(crate) struct ReturnButton;

pub(crate) fn init_end(
    mut commands: Commands,
    font: Res<GameFont>,
    mat: Res<Materials>,
    winner: Res<Winner>,
) {
    commands
        .spawn_bundle(NodeBundle {
            style: Style {
                size: Size::new(Val::Px(40.0 * 6.0), Val::Px(40.0)),
                align_self: AlignSelf::FlexEnd,
                align_items: AlignItems::Center,
                position_type: PositionType::Absolute,
                position: Rect {
                    top: Val::Px(30.0),
                    left: Val::Px(20.0),
                    ..Default::default()
                },
                margin: Rect::all(Val::Auto),
                justify_content: JustifyContent::Center,
                ..Default::default()
            },
            material: mat.black.clone(),
            ..Default::default()
        })
        .with_children(|parent| {
            parent.spawn_bundle(TextBundle {
                // text: Text::with_section(
                //     match *winner {
                //         Winner(TokenColor::Black) => "Nobody wins",
                //         Winner(TokenColor::Red) => "Red wins",
                //         Winner(TokenColor::Yellow) => "Yellow wins",
                //     },
                //     TextStyle {
                //         font: font.0.clone(),
                //         font_size: 30.0,
                //         color: Color::rgb(1.0, 1.0, 1.0),
                //     },
                //     TextAlignment::default(),
                // ),
                text: Text {
                    sections: vec![
                        TextSection {
                            value: match *winner {
                                Winner(TokenColor::Black) => "Nobody",
                                Winner(TokenColor::Red) => "Red",
                                Winner(TokenColor::Yellow) => "Yellow",
                            }
                            .to_owned(),
                            style: TextStyle {
                                font: font.0.clone(),
                                font_size: 30.0,
                                color: match *winner {
                                    Winner(TokenColor::Black) => Color::rgb(0.2, 0.2, 0.2),
                                    Winner(TokenColor::Red) => Color::rgb(1.0, 0.0, 0.0),
                                    Winner(TokenColor::Yellow) => Color::rgb(1.0, 1.0, 0.0),
                                },
                            },
                        },
                        TextSection {
                            value: " wins".to_owned(),
                            style: TextStyle {
                                font: font.0.clone(),
                                font_size: 30.0,
                                color: Color::rgb(1.0, 1.0, 1.0),
                            },
                        },
                    ],
                    alignment: TextAlignment::default(),
                },
                ..Default::default()
            });
        })
        .insert(WinnerText);

    commands
        .spawn_bundle(ButtonBundle {
            style: Style {
                size: Size::new(Val::Px(40.0 * 6.0), Val::Px(40.0)),
                align_self: AlignSelf::FlexEnd,
                align_items: AlignItems::Center,
                position_type: PositionType::Absolute,
                position: Rect {
                    bottom: Val::Px(2.0 * 30.0 + 40.0),
                    left: Val::Px(20.0),
                    ..Default::default()
                },
                margin: Rect::all(Val::Auto),
                justify_content: JustifyContent::Center,
                ..Default::default()
            },
            material: mat.black.clone(),
            ..Default::default()
        })
        .with_children(|parent| {
            parent.spawn_bundle(TextBundle {
                text: Text::with_section(
                    "Return to menu",
                    TextStyle {
                        font: font.0.clone(),
                        font_size: 30.0,
                        color: Color::rgb(1.0, 1.0, 1.0),
                    },
                    TextAlignment::default(),
                ),
                ..Default::default()
            });
        })
        .insert(ReturnButton);

    commands
        .spawn_bundle(ButtonBundle {
            style: Style {
                size: Size::new(Val::Px(40.0 * 6.0), Val::Px(40.0)),
                align_self: AlignSelf::FlexEnd,
                align_items: AlignItems::Center,
                position_type: PositionType::Absolute,
                position: Rect {
                    bottom: Val::Px(30.0),
                    left: Val::Px(20.0),
                    ..Default::default()
                },
                margin: Rect::all(Val::Auto),
                justify_content: JustifyContent::Center,
                ..Default::default()
            },
            material: mat.black.clone(),
            ..Default::default()
        })
        .with_children(|parent| {
            parent.spawn_bundle(TextBundle {
                text: Text::with_section(
                    "Exit game",
                    TextStyle {
                        font: font.0.clone(),
                        font_size: 30.0,
                        color: Color::rgb(1.0, 1.0, 1.0),
                    },
                    TextAlignment::default(),
                ),
                ..Default::default()
            });
        })
        .insert(ExitButton);
}

pub(crate) fn deinit_end(
    mut commands: Commands,
    winner_text: Query<Entity, With<WinnerText>>,
    return_button: Query<Entity, With<ReturnButton>>,
    exit_button: Query<Entity, With<ExitButton>>,
) {
    commands
        .entity(winner_text.single().unwrap())
        .despawn_recursive();
    commands
        .entity(return_button.single().unwrap())
        .despawn_recursive();
    commands
        .entity(exit_button.single().unwrap())
        .despawn_recursive();
}

pub(crate) fn handle_buttons_end(
    pvp_q: Query<&Interaction, (Changed<Interaction>, With<ReturnButton>)>,
    ai_q: Query<&Interaction, (Changed<Interaction>, With<ExitButton>)>,
    mut state: ResMut<State<AppState>>,
    mut exit: EventWriter<AppExit>,
) {
    for &interaction in pvp_q.iter() {
        if let Interaction::Clicked = interaction {
            let _ = state.set(AppState::Menu);
            return;
        }
    }

    for &interaction in ai_q.iter() {
        if let Interaction::Clicked = interaction {
            exit.send(AppExit);
            return;
        }
    }
}
