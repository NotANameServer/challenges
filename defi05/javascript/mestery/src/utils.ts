declare global {
  interface HTMLCanvasElement {
    captureStream(frameRate: number): MediaStream;
  }
}

const SUPPORTED_FORMAT = ['video/mp4', 'video/webm;codecs=vp9', 'video/webm;codecs=vp8', 'video/webm'].filter(MediaRecorder.isTypeSupported);

export function canvasRecord(canvas: HTMLCanvasElement): () => Promise<Blob> {
  const videoStream = canvas.captureStream(60);
  const mediaRecorder = new MediaRecorder(videoStream, {
    audioBitsPerSecond: 0,
    videoBitsPerSecond: 2500000,
    mimeType: SUPPORTED_FORMAT[0],
  });
  const chunks: Blob[] = [];
  mediaRecorder.addEventListener('dataavailable', ({ data }) => chunks.push(data));
  
  mediaRecorder.start();

  return () => new Promise((resolve) => {
    mediaRecorder.addEventListener('stop', () => {
      resolve(new Blob(chunks, { type : mediaRecorder.mimeType.split(';')[0] }));
    });
    mediaRecorder.stop();
  });
}
