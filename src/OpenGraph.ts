import { JSDOM } from 'jsdom';

type OpenGraph = {
    title: string | null;
    description: string | null;
    image: string | null;
};

const OpenGraph = {
    parse: (html: string) => {
        const {
            window: { document },
        } = new JSDOM(html);

        const title = document.head.getElementsByTagName('title')[0]?.textContent;

        const getProp = (prop: string): string | null =>
            document.head.querySelector(`meta[property="${prop}"]`)?.getAttribute('content') ?? null;

        return {
            title: getProp('og:title') ?? title ?? null,
            description: getProp('og:description'),
            image: getProp('og:image'),
        };
    },
};

export { OpenGraph };
