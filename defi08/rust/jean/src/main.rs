use bevy::prelude::*;

mod end;
mod game;
mod menu;

use end::*;
use game::*;
use menu::*;

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
enum AppState {
    Menu,
    GamePvP,
    GameAI,
    EndOfGame,
}

pub(crate) struct Materials {
    black: Handle<ColorMaterial>,
    red: Handle<ColorMaterial>,
    yellow: Handle<ColorMaterial>,
    blue: Handle<ColorMaterial>,
}

struct Game;

impl Plugin for Game {
    fn build(&self, app: &mut AppBuilder) {
        app.add_event::<ClickedColumnEvent>()
            .add_startup_system(setup.system())
            .add_state(AppState::Menu)
            // Menu
            .add_system_set(SystemSet::on_enter(AppState::Menu).with_system(init_menu.system()))
            .add_system_set(
                SystemSet::on_update(AppState::Menu).with_system(handle_buttons_menu.system()),
            )
            .add_system_set(SystemSet::on_exit(AppState::Menu).with_system(deinit_menu.system()))
            // PvP
            .add_system_set(SystemSet::on_enter(AppState::GamePvP).with_system(init_game.system()))
            .add_system_set(
                SystemSet::on_update(AppState::GamePvP)
                    .with_system(handle_click_game.system())
                    .with_system(shot_played.system()),
            )
            .add_system_set(SystemSet::on_exit(AppState::GamePvP).with_system(deinit_game.system()))
            // End
            .add_system_set(SystemSet::on_enter(AppState::EndOfGame).with_system(init_end.system()))
            .add_system_set(
                SystemSet::on_update(AppState::EndOfGame).with_system(handle_buttons_end.system()),
            )
            .add_system_set(
                SystemSet::on_exit(AppState::EndOfGame).with_system(deinit_end.system()),
            );
    }
}

fn setup(
    mut commands: Commands,
    mut window: ResMut<Windows>,
    asset_server: Res<AssetServer>,
    mut materials: ResMut<Assets<ColorMaterial>>,
) {
    commands.insert_resource(ClearColor(Color::rgb(0.2, 0.2, 0.2)));
    commands.spawn_bundle(OrthographicCameraBundle::new_2d());
    commands.spawn_bundle(UiCameraBundle::default());

    let _ = window.get_primary_mut().map(|window| {
        window.set_title("Puissance 4".to_owned());
        window.set_resolution(40.0 * 7.0, 40.0 * 6.0);
        window.set_resizable(false);
    });

    commands.insert_resource(GameFont(asset_server.load("FiraSans-Bold.ttf")));

    let black = materials.add(Color::rgb(0.0, 0.0, 0.0).into());
    let red = materials.add(Color::rgb(1.0, 0.0, 0.0).into());
    let yellow = materials.add(Color::rgb(1.0, 1.0, 0.0).into());
    let blue = materials.add(Color::rgb(0.0, 0.0, 1.0).into());

    commands.insert_resource(Materials {
        black,
        red,
        yellow,
        blue,
    });
}

fn main() {
    App::build()
        .add_plugins(DefaultPlugins)
        .add_plugin(Game)
        .run();
}
