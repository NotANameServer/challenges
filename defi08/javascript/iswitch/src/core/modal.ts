export class Modal {
  private _container: HTMLElement;
  private _content: DocumentFragment;
  private _modal: HTMLDivElement;
  private _modalContent: HTMLDivElement;

  public set container(container: HTMLElement) {
    this.close()
    this._container = container;
  }

  public set content(content: DocumentFragment) {
    this._content = content;
    this._modal.innerHTML = '';
    this._modalContent.innerHTML = '';
    this._modalContent.appendChild(this._content);
    this._modal.appendChild(this._modalContent);
  }

  constructor(
    container: HTMLElement = document.body,
    content: DocumentFragment = document.createDocumentFragment()
  ) {
    this._container = container;
    this._content = content;
    this._modal = this._buildEmptyModal();
    this._modalContent = this._buildEmptyModalContent();
    this._modal.appendChild(this._modalContent);
    this.content = this._content;
  }

  private _buildEmptyModal(): HTMLDivElement {
    const modal: HTMLDivElement = document.createElement('div');
    modal.classList.add('modal-backdrop');
    modal.addEventListener('click', (mouseEvent: MouseEvent) => {
      this.close();
    });
    return modal;
  }

  private _buildEmptyModalContent(): HTMLDivElement {
    const modalContent: HTMLDivElement = document.createElement('div');
    modalContent.classList.add('modal');
    modalContent.addEventListener('click', (mouseEvent: MouseEvent) => {
      mouseEvent.stopPropagation();
    });
    return modalContent;
  }

  public show() {
    this._container.appendChild(this._modal);
  }

  public close() {
    this._container.removeChild(this._modal);
  }
}
