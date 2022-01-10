import { define } from './define';
import { NonShadowedElement } from './NonShadowedElement';

@define('embedded-tweet')
export class EmbeddedTweet extends NonShadowedElement {
    public static properties = {
        src: { attribute: true },
    };

    public src!: string;

    public constructor() {
        super();

        this.style.display = 'block';
    }

    public connectedCallback(): void {
        window.twttr.widgets.createTweet(this.id, this);
    }

    public get id(): string {
        const { pathname } = new URL(this.src);
        const pieces = pathname.split('/');

        return pieces[pieces.length - 1]!;
    }
}
