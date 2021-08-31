use bevy::prelude::*;

use super::*;

pub(crate) struct GameFont(pub Handle<Font>);

pub(crate) struct PvPButton;
pub(crate) struct AIButton;

pub(crate) fn init_menu(mut commands: Commands, font: Res<GameFont>, mat: Res<Materials>) {
    commands
        .spawn_bundle(ButtonBundle {
            style: Style {
                size: Size::new(Val::Px(40.0 * 6.0), Val::Px(40.0)),
                align_self: AlignSelf::FlexEnd,
                align_items: AlignItems::Center,
                position_type: PositionType::Absolute,
                position: Rect {
                    top: Val::Px(60.0),
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
                    "PVP",
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
        .insert(PvPButton);

    commands
        .spawn_bundle(ButtonBundle {
            style: Style {
                size: Size::new(Val::Px(40.0 * 6.0), Val::Px(40.0)),
                align_self: AlignSelf::FlexEnd,
                align_items: AlignItems::Center,
                position_type: PositionType::Absolute,
                position: Rect {
                    bottom: Val::Px(60.0),
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
                    "AI",
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
        .insert(AIButton);
}

pub(crate) fn deinit_menu(
    mut commands: Commands,
    pvp_button: Query<Entity, With<PvPButton>>,
    ai_button: Query<Entity, With<AIButton>>,
) {
    commands
        .entity(pvp_button.single().unwrap())
        .despawn_recursive();
    commands
        .entity(ai_button.single().unwrap())
        .despawn_recursive();
}

pub(crate) fn handle_buttons_menu(
    pvp_q: Query<&Interaction, (Changed<Interaction>, With<PvPButton>)>,
    ai_q: Query<&Interaction, (Changed<Interaction>, With<AIButton>)>,
    mut state: ResMut<State<AppState>>,
    mut input: ResMut<Input<MouseButton>>,
) {
    for &interaction in pvp_q.iter() {
        if let Interaction::Clicked = interaction {
            let _ = state.set(AppState::GamePvP);
            input.update();
            return;
        }
    }

    for &interaction in ai_q.iter() {
        if let Interaction::Clicked = interaction {
            let _ = state.set(AppState::GameAI);
            input.update();
            return;
        }
    }
}
