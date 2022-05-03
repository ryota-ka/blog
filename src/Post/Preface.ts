import { Content, Root } from 'mdast';

const extract = (root: Root): Content[] => {
    const index = root.children.findIndex((node) => node.type === 'thematicBreak');

    if (index === -1) {
        throw new Error('A thematic break must be present in the body');
    }

    return root.children
        .slice(0, index)
        .filter((node) => node.type !== 'yaml' && (node.type !== 'heading' || node.depth !== 1));
};

export { extract };
