export class Loader {
    private _container: HTMLElement;
    private _loadingReason: string;
    private _loaderElement: HTMLDivElement;
    private _isDisplayed: boolean;

    public get isDisplayed(): boolean {
        return this._isDisplayed;
    }

    public set container(container: HTMLElement) {
        this.close();
        this._container = container;
    }

    public set loadingReason(loadingReason: string) {
        this._loadingReason = loadingReason;
        this._loaderElement = this._buildLoaderElement();
    }

    constructor(container: HTMLElement) {
        this._container = container;
        this._loadingReason = "";
        this._loaderElement = this._buildLoaderElement();
        this._isDisplayed = false;
    }

    private _buildLoaderElement(): HTMLDivElement {
        const loaderElement: HTMLDivElement = document.createElement("div");
        loaderElement.classList.add("loader-container");
        const loader: HTMLDivElement = document.createElement("div");
        loader.classList.add("loader");
        for (let loaderPartIndex = 0; loaderPartIndex < 4; ++loaderPartIndex) {
            const loaderPart: HTMLDivElement = document.createElement("div");
            loaderPart.classList.add("loader-part");
            loader.appendChild(loaderPart);
        }
        loaderElement.appendChild(loader);
        if (this._loadingReason) {
            const reason: HTMLParagraphElement = document.createElement("p");
            reason.classList.add("loader-reason");
            reason.textContent = this._loadingReason;
            loaderElement.appendChild(reason);
        }
        return loaderElement;
    }

    public show() {
        if (!this._isDisplayed) {
            this._container.appendChild(this._loaderElement);
            this._isDisplayed = true;
        }
    }

    public close() {
        if (this._isDisplayed) {
            this._container.removeChild(this._loaderElement);
            this._isDisplayed = false;
        }
    }
}