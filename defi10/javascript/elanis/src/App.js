import './App.css';

import useWindowDimensions from './hooks/useWindowDimensions';

import { Circle } from 'canvas2d-wrapper'

import World from './components/World';

export default function App() {
	const windowDimensions = useWindowDimensions();
	const elements = [];

	for(let i = 0; i < 25; i++) {
		const element = new Circle({
			id: i,
			x: Math.round(-0.45 * windowDimensions.width + (windowDimensions.width * 0.9 * Math.random())),
			y: Math.round(-0.45 * windowDimensions.height + (windowDimensions.height * 0.9 * Math.random())),
			radius: 10,
			fill: '#'+(Math.random()*0xFFFFFF<<0).toString(16),
		});

		element.velocity = {
			x: 35 + Math.round(Math.random() * 50) * (Math.random() > 0.5 ? 1 : -1),
			y: 35 + Math.round(Math.random() * 50) * (Math.random() > 0.5 ? 1 : -1),
		};

		elements.push(element);
	}

	return (
		<World windowDimensions={windowDimensions} baseElements={elements} />
	);
}
