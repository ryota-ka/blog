import { Root } from 'mdast';
import { toString } from 'mdast-util-to-string';

const extract = (root: Root): string => {
    const index = root.children.findIndex((node) => node.type === 'thematicBreak');

    return toString({
        type: 'root',
        children: root.children.slice(0, index).filter((node) => node.type !== 'heading' || node.depth !== 1),
    });
};

export { extract };
