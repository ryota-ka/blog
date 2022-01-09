import { LitElement } from 'lit';

export class NonShadowedElement extends LitElement {
    public constructor() {
        super();

        this.innerHTML = '';
    }

    protected createRenderRoot(): this {
        return this;
    }
}
