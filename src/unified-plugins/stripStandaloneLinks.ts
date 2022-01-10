import type { Root } from 'mdast';
import type { Plugin } from 'unified';

const stripStandaloneLinks: Plugin<[], Root, Root> = () => (tree: Root) => {
    const children = tree.children.filter(
        (child) => child.type !== 'paragraph' || child.children.length !== 1 || child.children[0]?.type !== 'link',
    );

    return {
        ...tree,
        children,
    };
};

export { stripStandaloneLinks };
