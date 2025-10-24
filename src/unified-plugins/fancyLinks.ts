import type { Root } from 'mdast';
import type { Plugin } from 'unified';

import { OpenGraph } from '../OpenGraph';

const fancyLinks: Plugin<[], Root, void> = () => async (tree: Root) => {
    const children = await Promise.all(
        tree.children.map(async (child) => {
            if (child.type !== 'paragraph' || child.children.length !== 1 || child.children[0]?.type !== 'link') {
                return child;
            }

            const node = child.children[0];
            node.data ??= {};
            const { url } = node;
            const { host, pathname } = new URL(url);
            const pieces = pathname.slice(1).split('/');

            if ((host === 'twitter.com' || host === 'x.com') && pieces[1] === 'status') {
                Object.assign(node.data, {
                    hName: 'embedded-tweet',
                    hProperties: {
                        src: url,
                    },
                });
                return node;
            }

            let openGraph: OpenGraph = {
                description: null,
                image: null,
                title: null,
            };
            try {
                const res = await fetch(url, {
                    headers: {
                        Accept: 'text/html',
                        'Accept-Language': 'ja;q=1.0, en;q=0.8, *;q=0.5',
                    },
                });
                const html = await res.text();

                openGraph = OpenGraph.parse(html);
            } catch {
                // nop
            }

            Object.assign(node.data, {
                hName: 'open-graph-card',
                hProperties: {
                    href: url,
                    ...openGraph,
                },
                hChildren: [],
            });

            return node;
        }),
    );

    return {
        ...tree,
        children,
    };
};

export { fancyLinks };
