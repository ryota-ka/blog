import { define } from './define';
import { NonShadowedElement } from './NonShadowedElement';

const skeletonTemplate = `
    <div class="tweet-skeleton rounded-lg border border-zinc-200 dark:border-zinc-700 p-4 my-4 min-h-[231px] max-w-[548px] animate-pulse">
        <div class="flex items-center gap-3 mb-4">
            <div class="w-12 h-12 bg-zinc-200 dark:bg-zinc-700 rounded-full"></div>
            <div class="flex-1">
                <div class="h-4 w-32 bg-zinc-200 dark:bg-zinc-700 rounded mb-2"></div>
                <div class="h-3 w-24 bg-zinc-200 dark:bg-zinc-700 rounded"></div>
            </div>
        </div>
        <div class="space-y-2">
            <div class="h-4 bg-zinc-200 dark:bg-zinc-700 rounded w-full"></div>
            <div class="h-4 bg-zinc-200 dark:bg-zinc-700 rounded w-5/6"></div>
            <div class="h-4 bg-zinc-200 dark:bg-zinc-700 rounded w-4/6"></div>
        </div>
        <div class="mt-4 flex gap-6">
            <div class="h-4 w-16 bg-zinc-200 dark:bg-zinc-700 rounded"></div>
            <div class="h-4 w-16 bg-zinc-200 dark:bg-zinc-700 rounded"></div>
            <div class="h-4 w-16 bg-zinc-200 dark:bg-zinc-700 rounded"></div>
        </div>
    </div>
`;

@define('embedded-tweet')
export class EmbeddedTweet extends NonShadowedElement {
    public static properties = {
        src: { attribute: true },
    };

    public src!: string;

    public constructor() {
        super();
        this.innerHTML = skeletonTemplate;
    }

    public connectedCallback(): void {
        const id = this.id;
        const twttr = window.twttr;
        if (id.length === 0 || twttr === undefined) {
            return;
        }
        const isDark = window.matchMedia('(prefers-color-scheme: dark)').matches;
        twttr.ready(async () => {
            await twttr.widgets.createTweet(id, this, {
                theme: isDark ? 'dark' : 'light',
            });
            // Remove the skeleton template after the tweet appears
            const skeleton = Array.from(this.children).find((child: Element) =>
                child.classList.contains('tweet-skeleton'),
            );
            skeleton?.remove();
        });
    }

    public get id(): string {
        const { pathname } = new URL(this.src);
        const pieces = pathname.split('/');

        const last = pieces[pieces.length - 1];
        return last ?? '';
    }
}
