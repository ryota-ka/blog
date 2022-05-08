import { html, TemplateResult } from 'lit';

import { define } from './define';
import { NonShadowedElement } from './NonShadowedElement';

@define('open-graph-card')
export class OpenGraphCard extends NonShadowedElement {
    public static properties = {
        description: { attribute: true },
        href: { attribute: true },
        image: { attribute: true },
        title: { attribute: true },
    };

    public description!: string | null;
    public href!: string;
    public image!: string | null;
    public title!: string;

    public constructor() {
        super();

        this.style.display = 'block';
    }

    public render(): TemplateResult {
        return html`
            <a
                class="rounded-xl border border-zinc-300 dark:border-zinc-600 leading-snug block text-left font-sans w-full md:w-3/4 lg:w-2/3 text-sm bg-zinc-50 dark:bg-zinc-900 hover:bg-zinc-100 dark:hover:bg-zinc-800 transition ease-out duration-100"
                href=${this.href}
                target="_blank"
                title=${this.title}
                rel="noopener noreferrer"
            >
                ${this.#renderImage()}
                <div class="py-2 px-3">
                    <div class="line-clamp-1 text-gray-500 dark:text-gray-400">${new URL(this.href).host}</div>
                    <div class="line-clamp-1 text-gray-900 dark:text-gray-100">${this.title}</div>
                    ${this.#renderDescription()}
                </div>
            </a>
        `;
    }

    #renderDescription(): TemplateResult | null {
        return this.description === null
            ? null
            : html`<div class="text-ellipsis overflow-hidden text-gray-500 dark:text-gray-400 line-clamp-2">
                  ${this.description}
              </div> `;
    }

    #renderImage(): TemplateResult | null {
        return this.image === null
            ? null
            : html`<img class="rounded-t-xl" alt="" src=${this.image} loading="lazy" decoding="async" />`;
    }
}
