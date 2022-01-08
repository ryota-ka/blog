import { Root } from 'mdast';
import { toString } from 'mdast-util-to-string';

const extract = (root: Root): string => {
    for (const child of root.children) {
        if (child.type === 'heading' && child.depth === 1) {
            return toString(child);
        }
    }

    throw new Error('Level 1 heading must be present in the body');
};

export { extract };
