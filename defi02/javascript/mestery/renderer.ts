import { CellType, Cell } from "./maze";

export interface RendererConstructor {
  new(grid: CellType[][]): Renderer
};

export interface Renderer {
  toString?(...args: any[]): string;
}
export abstract class Renderer {
  public constructor(protected readonly grid: CellType[][]) {}
  public abstract render(...args: unknown[]): unknown;
}

export interface TextRendererOptions {
  customCharMap?: { [P in CellType]?: string },
  separator?: string,
}

export class TextRenderer extends Renderer {
  public static CHAR_MAP = {
    [CellType.EMPTY]: ' ',
    [CellType.WALL]: '#',
    [CellType.ENTRY_POINT]: '.',
    // [CellType.EXIT]: ' ',
  } as const;

  public render({ separator = '', customCharMap = {} }: TextRendererOptions = {}): string {
    const charMap = { ...TextRenderer.CHAR_MAP, ...customCharMap };
    return this.grid.map((row) => row.map((cell) => charMap[cell]).join(separator ?? '')).join('\n');
  }

  public toString(options?: TextRendererOptions): string {
    return this.render(options);
  }
}
