import type { Root } from 'mdast';
import type { Plugin } from 'unified';
import { remove } from 'unist-util-remove';

const removeTitle: Plugin<[], Root, Root> = () => (tree) =>
    remove(tree, (node) => 'depth' in node && node.depth === 1) ?? tree;

export { removeTitle };
