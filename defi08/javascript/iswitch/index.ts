// Import stylesheets for Webpack
import './assets/style.css';

// Write TypeScript code
import { ConnectFour } from './src/core';

const gameElement: HTMLElement | null = document.querySelector('.connect-four');
const connectFour = new ConnectFour(gameElement!);
connectFour.start();
