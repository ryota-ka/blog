import type { Root } from 'mdast';
import type { Plugin } from 'unified';
import { visit } from 'unist-util-visit';

const targetBlank: Plugin<[], Root, Root> = () => (tree) => {
    visit(tree, 'link', (node) => {
        if (node.data?.hName !== undefined) {
            return;
        }

        node.data ??= {};
        node.data.hProperties ??= {};

        if (typeof node.data.hProperties !== 'object') {
            throw new Error(`hProperties must be an object`);
        }

        Object.assign(node.data.hProperties ?? {}, {
            rel: 'noopener',
            target: '_blank',
        });
    });
};

export { targetBlank };
