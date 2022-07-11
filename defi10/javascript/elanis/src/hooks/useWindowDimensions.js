import { useState, useEffect } from 'react';

function getWindowDimensions() {
	const { clientWidth: width, clientHeight: height } = document.documentElement;
	return {
		width,
		height
	};
}

export default function useWindowDimensions() {
	const [windowDimensions, setWindowDimensions] = useState(getWindowDimensions());

	useEffect(() => {
		function handleResize() {
			setWindowDimensions(getWindowDimensions());
		}

		window.addEventListener('resize', handleResize);
		return () => window.removeEventListener('resize', handleResize);
	}, []);

	return windowDimensions;
}