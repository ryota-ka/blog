import fetch from 'isomorphic-unfetch';
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
            const { url } = node;

            const res = await fetch(url, {
                headers: {
                    Accept: 'text/html',
                },
            });
            const html = await res.text();

            const openGraph = OpenGraph.parse(html);

            node.data ??= {};
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
