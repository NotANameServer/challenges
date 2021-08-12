import { Player } from '../core';

export type GameMode = {
  name: string;
  players: Player[];
  onClick: (players: Player[]) => void;
  slug: string;
};
